(** Lexer for Wasm Text Format (WAT). *)

val token : Sedlexing.lexbuf -> Parser.token
val is_valid_identifier : string -> bool
