type comment =
  | Comment of {
      kind : [ `Line | `Block ];
      at_start_of_line : bool;
      content : string;
      prev_token_end : int;
    }
  | Annotation of { at_start_of_line : bool; prev_token_end : int }
  | BlankLine of { prev_token_end : int }

type associated = {
  before : comment list;
  within : comment list;
  after : comment list;
}

type context = {
  mutable comments : comment list;
  mutable at_start_of_line : bool;
  mutable last_loc : Ast.location option;
  mutable prev_token_end : int;
  mutable locations : Ast.location list;
}

let make () =
  {
    comments = [];
    at_start_of_line = true;
    last_loc = None;
    prev_token_end = 0;
    locations = [];
  }

let add_comment ctx cmd = ctx.comments <- cmd :: ctx.comments

let report_comment ctx kind content =
  add_comment ctx
    (Comment
       {
         kind;
         at_start_of_line = ctx.at_start_of_line;
         content;
         prev_token_end = ctx.prev_token_end;
       })

let report_annotation ctx =
  add_comment ctx
    (Annotation
       {
         at_start_of_line = ctx.at_start_of_line;
         prev_token_end = ctx.prev_token_end;
       })

let report_newline ctx =
  if ctx.at_start_of_line then
    add_comment ctx (BlankLine { prev_token_end = ctx.prev_token_end });
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
  ctx.locations <- info :: ctx.locations;
  { Ast.desc; info }

let associate ctx =
  let tbl = Hashtbl.create (List.length ctx.locations) in
  let comments = List.rev ctx.comments in
  let locs =
    List.sort
      (fun a b ->
        let c =
          compare a.Ast.loc_start.Lexing.pos_cnum
            b.Ast.loc_start.Lexing.pos_cnum
        in
        if c <> 0 then c
        else compare b.Ast.loc_end.Lexing.pos_cnum a.Ast.loc_end.Lexing.pos_cnum)
      ctx.locations
  in
  let pos_of_comment = function
    | Comment { prev_token_end; _ }
    | Annotation { prev_token_end; _ }
    | BlankLine { prev_token_end; _ } ->
        prev_token_end
  in
  let split_before threshold comments =
    let rec aux acc = function
      | c :: rest when pos_of_comment c < threshold -> aux (c :: acc) rest
      | rest -> (List.rev acc, rest)
    in
    aux [] comments
  in
  let get_after parent_end comments =
    let rec aux acc = function
      | (Comment { prev_token_end; kind = `Line; at_start_of_line = false; _ }
         as c)
        :: rest
        when prev_token_end = parent_end ->
          (List.rev (c :: acc), rest)
      | (Comment { prev_token_end; kind = `Line; at_start_of_line = true; _ } as
         c)
        :: rest
        when prev_token_end = parent_end ->
          (List.rev acc, c :: rest)
      | (Comment { prev_token_end; _ } as c) :: rest
        when prev_token_end = parent_end ->
          aux (c :: acc) rest
      | (Annotation { prev_token_end; _ } as c) :: rest
        when prev_token_end = parent_end ->
          aux (c :: acc) rest
      | BlankLine _ :: rest -> (List.rev acc, rest)
      | rest -> (List.rev acc, rest)
    in
    aux [] comments
  in
  let rec process locs comments =
    match locs with
    | [] -> comments
    | loc :: rest_locs ->
        let is_child l =
          l.Ast.loc_end.Lexing.pos_cnum <= loc.Ast.loc_end.Lexing.pos_cnum
        in
        let rec span acc = function
          | l :: rs when is_child l -> span (l :: acc) rs
          | rs -> (List.rev acc, rs)
        in
        let children, siblings = span [] rest_locs in
        let before, rem1 =
          split_before loc.Ast.loc_start.Lexing.pos_cnum comments
        in
        let rem2 = process children rem1 in
        let within_candidates, rem3 =
          split_before loc.Ast.loc_end.Lexing.pos_cnum rem2
        in
        let steal_candidate =
          match List.rev children with
          | last_child :: _
            when last_child.Ast.loc_end.Lexing.pos_cnum
                 = loc.Ast.loc_end.Lexing.pos_cnum ->
              Some last_child
          | _ -> None
        in
        let final_after, rem4 =
          match steal_candidate with
          | Some last_child -> (
              match Hashtbl.find_opt tbl last_child with
              | Some assoc ->
                  let stolen = assoc.after in
                  Hashtbl.replace tbl last_child { assoc with after = [] };
                  (stolen, rem3)
              | None -> get_after loc.Ast.loc_end.Lexing.pos_cnum rem3)
          | None -> get_after loc.Ast.loc_end.Lexing.pos_cnum rem3
        in
        Hashtbl.add tbl loc
          { before; within = within_candidates; after = final_after };
        process siblings rem4
  in
  ignore (process locs comments);
  tbl
