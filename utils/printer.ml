type position = Line_start | Inline
type comment_type = Line | Block
type trivia = Comment of { comment : string; typ : comment_type } | Blank_line
type entry = { cnum : int; position : position; trivia : trivia }

module Comments = struct
  type t = {
    mutable entries : entry list;
    mutable last_line : int;
    mutable last_cnum : int; (* End position of the last meaningful token *)
  }

  let create () = { entries = []; last_line = 0; last_cnum = 0 }

  let push t start_line end_line trivia =
    let position = if start_line > t.last_line then Line_start else Inline in
    t.entries <- { cnum = t.last_cnum; position; trivia } :: t.entries;
    t.last_line <- end_line

  let add_line_comment t comment (span : Ast.location) =
    push t span.loc_start.pos_lnum span.loc_end.pos_lnum
      (Comment { comment; typ = Line })

  let add_block_comment t comment (span : Ast.location) =
    push t span.loc_start.pos_lnum span.loc_end.pos_lnum
      (Comment { comment; typ = Block })

  let add_newline t (span : Ast.location) =
    let line = span.loc_start.pos_lnum in
    let is_line_start = line > t.last_line in
    if is_line_start then push t line line Blank_line

  let visit_token t ({ loc_end; _ } : Ast.location) =
    t.last_line <- loc_end.pos_lnum;
    t.last_cnum <- loc_end.pos_cnum

  let contents t = List.rev t.entries
end

type t = {
  fmt : Format.formatter;
  mutable trivia : entry list;
  mutable started : bool;
  mutable pending_newlines : int; (* Number of newlines queued for output *)
  mutable pending_space : bool; (* A space is queued for output *)
  mutable has_emitted : bool;
      (* Any output has been generated on the current line *)
  mutable indent : int; (* Current indentation level (for breaks) *)
  mutable indent_stack : int list;
}

(* --- PRIMITIVES --- *)

let indent ctx indent f =
  let prev_indent = ctx.indent in
  ctx.indent <- indent;
  f ();
  ctx.indent <- prev_indent

let flush ?(skip_space = false) ctx =
  if ctx.pending_newlines > 0 then (
    if ctx.started && ctx.pending_newlines >= 2 then
      Format.pp_print_as ctx.fmt 0 "\n";
    Format.pp_print_break ctx.fmt 1000 ctx.indent;
    ctx.pending_newlines <- 0;
    ctx.pending_space <- false)
  else if ctx.pending_space then (
    if not skip_space then Format.pp_print_break ctx.fmt 1 ctx.indent;
    ctx.pending_space <- false)

let string ctx s =
  flush ctx;
  ctx.started <- true;
  ctx.has_emitted <- true;
  Format.pp_print_string ctx.fmt s

let string_as ctx len s =
  flush ctx;
  ctx.started <- true;
  ctx.has_emitted <- true;
  Format.pp_print_as ctx.fmt len s

let space ctx () =
  if ctx.pending_newlines == 0 && ctx.has_emitted then ctx.pending_space <- true

let newline ctx () =
  ctx.pending_newlines <- max ctx.pending_newlines 1;
  ctx.pending_space <- false

let blank_line ctx () =
  ctx.pending_newlines <- 2;
  ctx.pending_space <- false

let cut ctx () =
  flush ctx;
  Format.pp_print_cut ctx.fmt ()

let generic_box pp_open_box ctx skip_space indent f =
  flush ~skip_space ctx;
  pp_open_box ctx.fmt indent;
  ctx.indent_stack <- ctx.indent :: ctx.indent_stack;
  ctx.indent <- 0;
  f ();
  (* We don't flush spaces/newlines here, so that they are moved
       outside of the box *)
  Format.pp_close_box ctx.fmt ();
  match ctx.indent_stack with
  | [] -> assert false (* Should not happen if boxes are balanced *)
  | h :: t ->
      ctx.indent <- h;
      ctx.indent_stack <- t

let box ctx ?(skip_space = false) ?(indent = 0) f =
  generic_box Format.pp_open_box ctx skip_space indent f

let hvbox ctx ?(skip_space = false) ?(indent = 0) f =
  generic_box Format.pp_open_hvbox ctx skip_space indent f

let vbox ctx ?(skip_space = false) ?(indent = 0) f =
  generic_box Format.pp_open_vbox ctx skip_space indent f

(* --- CONSUMPTION ENGINE --- *)

let rec consume_while ctx predicate =
  match ctx.trivia with
  | [] -> ()
  | entry :: rem ->
      if predicate entry.cnum entry then (
        ctx.trivia <- rem;
        (match entry.trivia with
        | Blank_line -> blank_line ctx ()
        | Comment { comment; typ } -> (
            (match entry.position with
            | Line_start -> if ctx.has_emitted then newline ctx ()
            | Inline -> space ctx ());
            string ctx comment;
            match typ with Line -> newline ctx () | Block -> space ctx ()));
        consume_while ctx predicate)

(* --- API --- *)

let inline_comments ctx pos_cnum =
  consume_while ctx (fun cnum entry ->
      match entry.position with
      | Inline -> cnum <= pos_cnum
      | Line_start -> false)

let catchup ctx p_start_cnum =
  consume_while ctx (fun cnum _ -> cnum < p_start_cnum)

let node ctx pos_cnum print_content_fn =
  catchup ctx pos_cnum;
  print_content_fn ()

let remaining_comments ctx = catchup ctx max_int

let run ?(comments = Comments.create ()) fmt f =
  let p =
    {
      fmt;
      trivia = Comments.contents comments;
      started = false;
      pending_space = false;
      pending_newlines = 0;
      has_emitted = false;
      indent = 0;
      indent_stack = [];
    }
  in
  f p;
  flush p
