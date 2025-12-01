type location = { loc_start : Lexing.position; loc_end : Lexing.position }
type ('desc, 'info) annotated = { desc : 'desc; info : 'info }

let no_loc desc =
  { desc; info = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos } }
