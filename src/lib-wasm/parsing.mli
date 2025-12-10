(** Generic parsing utilities. *)

exception Syntax_error of (Lexing.position * Lexing.position) * string
(** Exception raised when a syntax error occurs, with location range and
    message. *)

(** Functor to create a parser from a Menhir incremental API. *)
module Make_parser (Output : sig
  type t
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
  val parse : ?color:Utils.Colors.flag -> filename:string -> unit -> Output.t
  (** Parse a file from a filename (reads from stdin if filename is empty or
      "-"). *)

  val parse_from_string :
    ?color:Utils.Colors.flag -> filename:string -> string -> Output.t
  (** Parse from a string. *)
end
