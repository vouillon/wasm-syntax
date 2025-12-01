(** Lexer for Wasm Text Format (WAT). *)

val token : Sedlexing.lexbuf -> Parser.token
