(** Lexer for Wax. *)

val is_valid_identifier : string -> bool
(** Checks if a string is a valid Wax identifier. *)

val token : Utils.Comment.context -> Sedlexing.lexbuf -> Tokens.token
