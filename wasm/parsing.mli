exception Syntax_error of (Lexing.position * Lexing.position) * string

module Make_parser (Output : sig
  type t = string option * Ast.modulefield list
end) (Parser : sig
  type token

  module MenhirInterpreter :
    MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token

  module Incremental : sig
    val parse : Lexing.position -> Output.t MenhirInterpreter.checkpoint
  end
end) (_ : sig
  exception Error

  val parse : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Output.t
end) (_ : sig
  val token : Sedlexing.lexbuf -> Parser.token
end) : sig
  val parse : filename:string -> Output.t
end
