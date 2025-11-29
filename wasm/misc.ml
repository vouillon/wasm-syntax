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

let is_hex c = ('0' <= c && c <= '9') || ('A' <= c && c <= 'F')
let is_exp hex c = c = if hex then 'P' else 'E'
let at_end hex s i = i = String.length s || is_exp hex s.[i]

let rec skip_non_hex s i =
  (* to skip sign, 'x', '.', '_', etc. *)
  if at_end true s i || is_hex s.[i] then i else skip_non_hex s (i + 1)

let rec skip_zeroes s i =
  let i' = skip_non_hex s i in
  if at_end true s i' || s.[i'] <> '0' then i' else skip_zeroes s (i' + 1)

let rec compare_mantissa_str' hex s1 i1 s2 i2 =
  let i1' = skip_non_hex s1 i1 in
  let i2' = skip_non_hex s2 i2 in
  match (at_end hex s1 i1', at_end hex s2 i2') with
  | true, true -> 0
  | true, false -> if at_end hex s2 (skip_zeroes s2 i2') then 0 else -1
  | false, true -> if at_end hex s1 (skip_zeroes s1 i1') then 0 else 1
  | false, false -> (
      match compare s1.[i1'] s2.[i2'] with
      | 0 -> compare_mantissa_str' hex s1 (i1' + 1) s2 (i2' + 1)
      | n -> n)

let compare_mantissa_str hex s1 s2 =
  let s1' = String.uppercase_ascii s1 in
  let s2' = String.uppercase_ascii s2 in
  compare_mantissa_str' hex s1' (skip_zeroes s1' 0) s2' (skip_zeroes s2' 0)

let float32_of_string s =
  (* First parse to a 64 bit float. *)
  let z = float_of_string s in
  (* If value is already infinite we are done. *)
  if abs_float z = 1.0 /. 0.0 then z
  else if abs_float z <= 0x1.fffffep+127 then z
  else
    (* Else, bit twiddling to see what rounding to target precision will do. *)
    let open Int64 in
    let bits = bits_of_float z in
    let lsb = shift_left 1L (52 - 23) in
    (* Check for tie, i.e. whether the bits right of target LSB are 10000... *)
    let tie = shift_right lsb 1 in
    let mask = lognot (shift_left (-1L) (52 - 23)) in
    (* If we have no tie, we are good. *)
    if logand bits mask <> tie then z
    else
      (* Else, define epsilon to be the value of the tie bit. *)
      let exp = float_of_bits (logand bits 0xfff0_0000_0000_0000L) in
      let eps = float_of_bits (logor tie (bits_of_float exp)) -. exp in
      (* Convert 64 bit float back to string to compare to input. *)
      let hex = String.contains s 'x' in
      let s' =
        if not hex then Printf.sprintf "%.*g" (String.length s) z
        else
          let m =
            logor (logand bits 0xf_ffff_ffff_ffffL) 0x10_0000_0000_0000L
          in
          (* Shift mantissa to match msb position in most significant hex digit *)
          let i = skip_zeroes (String.uppercase_ascii s) 0 in
          if i = String.length s then Printf.sprintf "%.*g" (String.length s) z
          else
            let sh =
              match s.[i] with
              | '1' -> 0
              | '2' .. '3' -> 1
              | '4' .. '7' -> 2
              | _ -> 3
            in
            Printf.sprintf "%Lx" (shift_left m sh)
      in
      (* - If mantissa became larger, float was rounded up to tie already;
       *   round-to-even might round up again: sub epsilon to round down.
       * - If mantissa became smaller, float was rounded down to tie already;
       *   round-to-even migth round down again: add epsilon to round up.
       * - If tie is not the result of prior rounding, then we are good.
       *)
      match compare_mantissa_str hex s s' with
      | -1 -> z -. eps
      | 1 -> z +. eps
      | _ -> z

let rec is_int conv sub s =
  if String.starts_with ~prefix:"-0x" s then
    let s = String.sub s 1 (String.length s - 1) in
    is_int conv sub s
    &&
    let i = conv s in
    i >= conv "0" || sub i (conv "1") >= conv "0"
  else
    try
      ignore (conv s);
      true
    with Failure _ -> (
      try
        ignore (conv ("0u" ^ s));
        true
      with Failure _ -> false)

let is_int32 s = is_int Int32.of_string Int32.sub s
let is_int64 s = is_int Int64.of_string Int64.sub s

let check_float s w f =
  if String.length s <= 2 then f s
  else
    let i = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
    match s.[i] with
    | 'n' -> (
        String.length s = i + 3
        ||
          try
            let exp =
              Int64.of_string (String.sub s (i + 4) (String.length s - i - 4))
            in
            exp > 0L && exp < Int64.shift_left 1L w
          with Failure _ -> false)
    | 'i' -> true
    | _ -> f s

let is_float32 s =
  check_float s 23 (fun s ->
      float32_of_string s < 0x1.ffffffp127
      && float32_of_string s > -0x1.ffffffp127)

let is_float64 s = check_float s 52 (fun s -> Float.(is_finite (of_string s)))
