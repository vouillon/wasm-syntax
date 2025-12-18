(** Lexer for Wasm Text Format (WAT). *)

val token : Utils.Comment.context -> Sedlexing.lexbuf -> Tokens.token
val is_valid_identifier : string -> bool
