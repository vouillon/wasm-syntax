exception Syntax_error of (Lexing.position * Lexing.position) * string

module Make_parser (Output : sig
  type t
end) (Parser : sig
  type token

  module MenhirInterpreter :
    MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token

  module Incremental : sig
    val parse : Lexing.position -> Output.t MenhirInterpreter.checkpoint
  end
end) (Fast_parser : sig
  exception Error

  val parse : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Output.t
end) (Lexer : sig
  val token : Sedlexing.lexbuf -> Parser.token
end) =
struct
  module E = MenhirLib.ErrorReports
  module L = MenhirLib.LexerUtil

  let succeed v = v

  let show text positions =
    E.extract text positions |> E.sanitize |> E.compress
    |> E.shorten 20 (* max width 43 *)

  let env checkpoint =
    match checkpoint with
    | Parser.MenhirInterpreter.HandlingError env -> env
    | _ -> assert false

  let state checkpoint : int =
    match Parser.MenhirInterpreter.top (env checkpoint) with
    | Some (Element (s, _, _, _)) -> Parser.MenhirInterpreter.number s
    | None -> 0

  let get text checkpoint i =
    match Parser.MenhirInterpreter.get i (env checkpoint) with
    | Some (Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
    | None -> (* should not happen *) "???"

  let fail text buffer checkpoint =
    (* Indicate where in the input file the error occurred. *)
    let location = L.range (E.last buffer) in
    (* Show the tokens just before and just after the error. *)
    let indication =
      Format.sprintf "Syntax error %s.\n" (E.show (show text) buffer)
    in
    (* Fetch an error message from the database. *)
    let message = Parser_messages.message (state checkpoint) in
    let message =
      if message = "<YOUR SYNTAX ERROR MESSAGE HERE>\n" then
        Printf.sprintf "Syntax error (%d)\n" (state checkpoint)
      else message
    in
    (* Expand away the $i keywords that might appear in the message. *)
    let message = E.expand (get text checkpoint) message in
    (* Show these three components. *)
    Format.eprintf "%s%s%s%!" location indication message;
    exit 1

  let report_syntax_error loc msg =
    let location = MenhirLib.LexerUtil.range loc in
    Format.eprintf "%s%s%!" location msg;
    exit 1

  let read filename = In_channel.with_open_bin filename In_channel.input_all

  let initialize_lexing filename text =
    let lexbuf = Sedlexing.Utf8.from_string text in
    Sedlexing.set_filename lexbuf filename;
    lexbuf

  let lexer_lexbuf_to_supplier lexer (lexbuf : Sedlexing.lexbuf) () =
    let token = lexer lexbuf in
    let startp, endp = Sedlexing.lexing_positions lexbuf in
    (token, startp, endp)

  let parse_with_errors filename text =
    let lexbuf = initialize_lexing filename text in
    let supplier = lexer_lexbuf_to_supplier Lexer.token lexbuf in
    let buffer, supplier = E.wrap_supplier supplier in
    let checkpoint =
      Parser.Incremental.parse (snd (Sedlexing.lexing_positions lexbuf))
    in
    try
      Parser.MenhirInterpreter.loop_handle succeed (fail text buffer) supplier
        checkpoint
    with Syntax_error (loc, msg) -> report_syntax_error loc msg

  let parse ~filename =
    let text = read filename in
    try
      let lexbuf = initialize_lexing filename text in
      let supplier = lexer_lexbuf_to_supplier Lexer.token lexbuf in
      let revised_parser =
        MenhirLib.Convert.Simplified.traditional2revised Fast_parser.parse
      in
      revised_parser supplier
    with
    | Fast_parser.Error -> parse_with_errors filename text
    | Syntax_error (loc, msg) -> report_syntax_error loc msg
end
