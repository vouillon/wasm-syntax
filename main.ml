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
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0

let get text checkpoint i =
  match Parser.MenhirInterpreter.get i (env checkpoint) with
  | Some (Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

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

let parse filename =
  let text, lexbuf = L.read filename in
  (*
  let lexbuf = L.init filename (Lexing.from_channel ch) in
*)
  let supplier =
    Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf
  in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.module_ lexbuf.lex_curr_p in
  try
    Parser.MenhirInterpreter.loop_handle succeed (fail text buffer) supplier
      checkpoint
  with Lexer.Error msg ->
    Format.eprintf "%s%!" msg;
    exit 1

let _ = parse "test.txt"
