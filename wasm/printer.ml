type comment_kind = Line_start | Inline
type comment_type = Line | Block

type trivia =
  | Comment of {
      comment : string;
      typ : comment_type;
      kind : comment_kind;
      cnum : int;
    }
  | Blank_line of { cnum : int }

module Comments = struct
  type t = { entries : trivia list ref; mutable last_line : int }

  let create () = { entries = ref []; last_line = 0 }

  let add_line_comment t comment (span : Ast.location) =
    let is_line_start = span.loc_start.pos_lnum > t.last_line in
    let kind = if is_line_start then Line_start else Inline in
    t.entries :=
      Comment { comment; typ = Line; kind; cnum = span.loc_start.pos_cnum }
      :: !(t.entries);
    t.last_line <- span.loc_end.pos_lnum

  let add_block_comment t comment (span : Ast.location) =
    let is_line_start = span.loc_start.pos_lnum > t.last_line in
    let kind = if is_line_start then Line_start else Inline in
    t.entries :=
      Comment { comment; typ = Block; kind; cnum = span.loc_start.pos_cnum }
      :: !(t.entries);
    t.last_line <- span.loc_end.pos_lnum

  let add_newline t (span : Ast.location) =
    let is_line_start = span.loc_start.pos_lnum > t.last_line in
    if is_line_start then
      t.entries := Blank_line { cnum = span.loc_start.pos_cnum } :: !(t.entries)

  let visit_token t (span : Ast.location) =
    t.last_line <- span.loc_start.pos_lnum

  let contents t = List.rev !(t.entries)
end

type t = {
  fmt : Format.formatter;
  mutable trivia : trivia list;
  mutable pending_newlines : int; (* Number of newlines queued for output *)
  mutable pending_space : bool; (* A space is queued for output *)
  mutable has_emitted : bool; (* Any output has been generated *)
  mutable indent : int; (* Current indentation level (for breaks) *)
  mutable indent_stack : int list;
}

let create comments fmt =
  {
    fmt;
    trivia = Comments.contents comments;
    pending_space = false;
    pending_newlines = 0;
    has_emitted = false;
    indent = 0;
    indent_stack = [];
  }

(* --- PRIMITIVES --- *)

let set_indent ctx i = ctx.indent <- i

let flush ?(skip_space = false) ctx =
  if ctx.pending_newlines > 0 then (
    if ctx.pending_newlines >= 2 then Format.pp_print_as ctx.fmt 0 "\n";
    Format.pp_print_break ctx.fmt 1000 ctx.indent;
    ctx.pending_newlines <- 0;
    ctx.pending_space <- false)
  else if ctx.pending_space then (
    if not skip_space then Format.pp_print_break ctx.fmt 1 ctx.indent;
    ctx.pending_space <- false)

let string ctx s =
  flush ctx;
  ctx.has_emitted <- true;

  Format.pp_print_string ctx.fmt s

let space ctx () =
  if ctx.pending_newlines == 0 && ctx.has_emitted then ctx.pending_space <- true

let newline ctx () =
  ctx.pending_newlines <- max ctx.pending_newlines 1;
  ctx.pending_space <- false

let blank_line ctx () =
  ctx.pending_newlines <- 2;
  ctx.pending_space <- false

let open_box ctx ?(skip_space = false) indent =
  flush ~skip_space ctx;

  Format.pp_open_box ctx.fmt indent;
  ctx.indent_stack <- ctx.indent :: ctx.indent_stack;
  ctx.indent <- 0

let open_hvbox ctx ?(skip_space = false) indent =
  flush ~skip_space ctx;

  Format.pp_open_hvbox ctx.fmt indent;
  ctx.indent_stack <- ctx.indent :: ctx.indent_stack;
  ctx.indent <- 0

let open_vbox ctx ?(skip_space = false) indent =
  flush ~skip_space ctx;

  Format.pp_open_vbox ctx.fmt indent;
  ctx.indent_stack <- ctx.indent :: ctx.indent_stack;
  ctx.indent <- 0

let close_box ctx () =
  (* We don't flush spaces/newlines here, so that they are moved
       outside of the box *)
  Format.pp_close_box ctx.fmt ();
  match ctx.indent_stack with
  | [] -> assert false (* Should not happen if boxes are balanced *)
  | h :: t ->
      ctx.indent <- h;
      ctx.indent_stack <- t

let finish ctx = Format.pp_print_newline ctx.fmt ()

(* --- CONSUMPTION ENGINE --- *)

let rec consume_while ctx predicate =
  match ctx.trivia with
  | [] -> ()
  | entry :: rem ->
      let entry_cnum =
        match entry with Blank_line { cnum } | Comment { cnum; _ } -> cnum
      in
      if predicate entry_cnum entry then (
        ctx.trivia <- rem;
        (match entry with
        | Blank_line _ -> blank_line ctx ()
        | Comment { comment; typ; kind; _ } -> (
            (match kind with
            | Line_start -> if ctx.has_emitted then newline ctx ()
            | Inline -> space ctx ());
            string ctx comment;
            match typ with Line -> newline ctx () | Block -> space ctx ()));
        consume_while ctx predicate)

(* --- API --- *)

let inline_comments ctx pos_cnum =
  consume_while ctx (fun cnum entry ->
      match entry with
      | Comment { kind = Inline; _ } -> cnum < pos_cnum
      | _ -> false)

let catchup ctx p_start_cnum =
  consume_while ctx (fun cnum _ -> cnum < p_start_cnum)

let node ctx pos_cnum print_content_fn =
  catchup ctx pos_cnum;
  print_content_fn ()

let remaining_comments ctx = catchup ctx max_int
