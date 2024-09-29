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

let read file =
  let text = In_channel.with_open_bin file In_channel.input_all in
  let lexbuf = Sedlexing.Utf8.from_string text in
  Sedlexing.set_filename lexbuf file;
  (text, lexbuf)

let lexer_lexbuf_to_supplier lexer (lexbuf : Sedlexing.lexbuf) () =
  let token = lexer lexbuf in
  let startp, endp = Sedlexing.lexing_positions lexbuf in
  (token, startp, endp)

let parse filename =
  let text, lexbuf = read filename in
  let supplier = lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint =
    Parser.Incremental.module_ (snd (Sedlexing.lexing_positions lexbuf))
  in
  try
    Parser.MenhirInterpreter.loop_handle succeed (fail text buffer) supplier
      checkpoint
  with Misc.Syntax_error (loc, msg) ->
    let location = MenhirLib.LexerUtil.range loc in
    Format.eprintf "%s%s%!" location msg;
    exit 1

let _ =
  let p = "/home/jerome/wasm_of_ocaml/runtime/wasm" in
  (* f32 *)
  let lst = [] in
  let l = Sys.readdir p in
  Array.iter
    (fun nm ->
      if Filename.check_suffix nm ".wat" && not (List.mem nm lst) then
        ignore (parse (Filename.concat p nm)))
    l;
  ignore
    (parse
       "/home/jerome/tmp/jane-street/_build/default/lib/bonsai/ppx_bonsai/test/inline/.ppx_bonsai_test.inline-tests/inline_test_runner_ppx_bonsai_test.bc.wasm.wat")
