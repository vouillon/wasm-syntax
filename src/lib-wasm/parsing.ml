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
    let message =
      try Parser_messages.message (state checkpoint)
      with Not_found ->
        Printf.sprintf "Syntax error (%d)\n" (state checkpoint)
    in
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

  let visual_width s =
    let len = String.length s in
    let rec loop i width =
      if i >= len then width
      else
        let decode = String.get_utf_8_uchar s i in
        let char_width =
          let u = Uchar.utf_decode_uchar decode in
          if Uchar.equal u (Uchar.of_int 0x09) then
            let current_visual_col = width in
            (((current_visual_col / 8) + 1) * 8) - current_visual_col
          else 1
        in
        loop (i + Uchar.utf_decode_length decode) (width + char_width)
    in
    loop 0 0

  (* Helper: Scan string to find the position of the next newline *)
  let find_eol text start_pos =
    try String.index_from text start_pos '\n'
    with Not_found -> String.length text

  let report_syntax_error text (start_p, end_p) msg =
    let start_line = start_p.Lexing.pos_lnum in
    let end_line = end_p.Lexing.pos_lnum in
    let filename = start_p.Lexing.pos_fname in

    (* Calculate Start Column (Visual) *)
    let s_bol = start_p.Lexing.pos_bol in
    let s_cnum = start_p.Lexing.pos_cnum in
    let s_prefix = String.sub text s_bol (s_cnum - s_bol) in
    let start_col = visual_width s_prefix in

    (* Calculate End Column (Visual) *)
    let e_bol = end_p.Lexing.pos_bol in
    let e_cnum = end_p.Lexing.pos_cnum in
    let e_prefix = String.sub text e_bol (e_cnum - e_bol) in
    let end_col = visual_width e_prefix in

    (* --- PRINT HEADER --- *)
    if start_line = end_line then
      Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" filename
        start_line start_col end_col
    else
      Printf.eprintf
        "File \"%s\", line %d, character %d to line %d, character %d:\n"
        filename start_line start_col end_line end_col;

    Printf.eprintf "Error: %s\n" msg;

    (* --- PRINT CODE CONTEXT --- *)
    (if start_line = end_line then (
       (* CASE 1: Single Line (Print Code + Underline) *)
       let line_end = find_eol text s_bol in
       let line_code = String.sub text s_bol (line_end - s_bol) in

       Printf.eprintf "%5d | %s\n" start_line line_code;

       (* Calculate underline width *)
       let error_len_bytes = e_cnum - s_cnum in
       let error_text = String.sub text s_cnum error_len_bytes in
       let visual_len = max 1 (visual_width error_text) in

       let padding = String.make (8 + start_col) ' ' in
       let underline = String.make visual_len '^' in
       Printf.eprintf "%s%s\n\n" padding underline)
     else
       (* CASE 2: Multi-line (Print Block of Lines) *)
       let curr_pos = ref s_bol in

       (* Optional: Limit huge errors to avoid flooding the terminal *)
       let max_lines_to_print = 5 in
       let total_lines = end_line - start_line + 1 in

       for i = 0 to total_lines - 1 do
         if i < max_lines_to_print then begin
           let line_num = start_line + i in
           let line_end = find_eol text !curr_pos in
           let line_len = line_end - !curr_pos in
           let line_content = String.sub text !curr_pos line_len in

           Printf.eprintf "%5d | %s\n" line_num line_content;

           (* Advance cursor *)
           curr_pos := line_end + 1
         end
         else if i = max_lines_to_print then Printf.eprintf "      | ...\n"
       done;
       print_newline ());
    exit 123
  (*
  let report_syntax_error loc msg =
    let location = MenhirLib.LexerUtil.range loc in
    Format.eprintf "%s%s%!" location msg;
    exit 1
*)

  let read filename = In_channel.with_open_bin filename In_channel.input_all

  let initialize_lexing filename text =
    let lexbuf = Sedlexing.Utf8.from_string text in
    Sedlexing.set_filename lexbuf filename;
    lexbuf

  let lexer_lexbuf_to_supplier lexer (lexbuf : Sedlexing.lexbuf) () =
    let token = lexer lexbuf in
    let startp, endp = Sedlexing.lexing_bytes_positions lexbuf in
    (token, startp, endp)

  let parse_with_errors filename text =
    let lexbuf = initialize_lexing filename text in
    let supplier = lexer_lexbuf_to_supplier Lexer.token lexbuf in
    let buffer, supplier = E.wrap_supplier supplier in
    let checkpoint =
      Parser.Incremental.parse (snd (Sedlexing.lexing_bytes_positions lexbuf))
    in
    try
      Parser.MenhirInterpreter.loop_handle succeed (fail text buffer) supplier
        checkpoint
    with Syntax_error (loc, msg) -> report_syntax_error text loc msg

  let parse_from_string ~filename text =
    let lexbuf = initialize_lexing filename text in
    try
      let supplier = lexer_lexbuf_to_supplier Lexer.token lexbuf in
      let revised_parser =
        MenhirLib.Convert.Simplified.traditional2revised Fast_parser.parse
      in
      revised_parser supplier
    with
    | Fast_parser.Error -> parse_with_errors filename text
    | Syntax_error (loc, msg) -> report_syntax_error text loc msg
    | Sedlexing.InvalidCodepoint _ | Sedlexing.MalFormed ->
        report_syntax_error text
          (Sedlexing.lexing_bytes_positions lexbuf)
          "Input file contains malformed UTF-8 byte sequences\n"

  let parse ~filename = parse_from_string ~filename (read filename)
end
