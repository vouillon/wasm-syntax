type comment =
  | Comment of {
      loc : Ast.location;
      kind : [ `Line | `Block ];
      at_start_of_line : bool;
      content : string;
      prev_token_end : int;
    }
  | Annotation of {
      loc : Ast.location;
      at_start_of_line : bool;
      prev_token_end : int;
    }
  | BlankLine of Lexing.position

type context = {
  mutable comments : comment list;
  mutable at_start_of_line : bool;
  mutable last_loc : Ast.location option;
  mutable prev_token_end : int;
}

let make () =
  {
    comments = [];
    at_start_of_line = true;
    last_loc = None;
    prev_token_end = 0;
  }

let add_comment ctx cmd = ctx.comments <- cmd :: ctx.comments

let report_comment ctx loc kind content =
  add_comment ctx
    (Comment
       {
         loc;
         kind;
         at_start_of_line = ctx.at_start_of_line;
         content;
         prev_token_end = ctx.prev_token_end;
       })

let report_annotation ctx loc =
  add_comment ctx
    (Annotation
       {
         loc;
         at_start_of_line = ctx.at_start_of_line;
         prev_token_end = ctx.prev_token_end;
       })

let report_newline ctx pos =
  if ctx.at_start_of_line then add_comment ctx (BlankLine pos);
  ctx.at_start_of_line <- true

let report_token ctx pos =
  ctx.at_start_of_line <- false;
  ctx.prev_token_end <- pos

let check_loc ctx (loc : Ast.location) =
  if false then
    let start_pos = loc.loc_start in
    let end_pos = loc.loc_end in
    match ctx.last_loc with
    | None -> ()
    | Some last ->
        let last_start = last.loc_start in
        let last_end = last.loc_end in
        let strictly_before =
          last_end.Lexing.pos_cnum <= start_pos.Lexing.pos_cnum
        in
        let within =
          start_pos.Lexing.pos_cnum <= last_start.Lexing.pos_cnum
          && last_end.Lexing.pos_cnum <= end_pos.Lexing.pos_cnum
        in
        if not (strictly_before || within) then
          Printf.eprintf
            "Location check failed: previous (%d-%d), current (%d-%d)\n%!"
            last_start.Lexing.pos_cnum last_end.Lexing.pos_cnum
            start_pos.Lexing.pos_cnum end_pos.Lexing.pos_cnum

let with_pos ctx info desc =
  check_loc ctx info;
  ctx.last_loc <- Some info;
  { Ast.desc; info }
