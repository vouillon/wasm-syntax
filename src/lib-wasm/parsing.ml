exception Syntax_error of (Lexing.position * Lexing.position) * string

module Make_parser (Output : sig
  type t
end) (Tokens : sig
  type token
end) (Parser : sig
  module Make (_ : sig
    type t = Utils.Trivia.context

    val context : t
  end) : sig
    type token = Tokens.token

    module MenhirInterpreter :
      MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token

    module Incremental : sig
      val parse : Lexing.position -> Output.t MenhirInterpreter.checkpoint
    end
  end
end) (Fast_parser : sig
  module Make (_ : sig
    type t = Utils.Trivia.context

    val context : t
  end) : sig
    type token = Tokens.token

    exception Error

    val parse : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Output.t
  end
end) (Parser_messages : sig
  val message : int -> string
end) (Lexer : sig
  val token : Utils.Trivia.context -> Sedlexing.lexbuf -> Tokens.token
end) =
struct
  module E = MenhirLib.ErrorReports
  module Lu = MenhirLib.LexerUtil

  let succeed v = v

  let show text positions =
    E.extract text positions |> E.sanitize |> E.compress
    |> E.shorten 20 (* max width 43 *)

  let report_syntax_error ~color source (loc_start, loc_end) msg =
    let theme = Utils.Diagnostic.get_theme ?color () in
    Utils.Diagnostic.output_error_with_source ~theme ~source ~severity:Error
      ~location:{ loc_start; loc_end } (fun f () -> Format.fprintf f "%s" msg);
    exit 123

  let read filename = In_channel.with_open_bin filename In_channel.input_all

  let initialize_lexing filename text =
    let lexbuf = Sedlexing.Utf8.from_string text in
    Sedlexing.set_filename lexbuf filename;
    lexbuf

  let lexer_lexbuf_to_supplier lexer ctx (lexbuf : Sedlexing.lexbuf) () =
    let token = lexer ctx lexbuf in
    let startp, endp = Sedlexing.lexing_bytes_positions lexbuf in
    (token, startp, endp)

  module Inner (Context : sig
    type t = Utils.Trivia.context

    val context : t
  end) =
  struct
    module P = Parser.Make (Context)
    module F = Fast_parser.Make (Context)

    let state checkpoint : int =
      match checkpoint with
      | P.MenhirInterpreter.HandlingError env -> (
          match P.MenhirInterpreter.top env with
          | Some (Element (s, _, _, _)) -> P.MenhirInterpreter.number s
          | None -> 0)
      | _ -> assert false

    let rec positions_in_stack env i =
      match P.MenhirInterpreter.get i env with
      | Some (Element (_, _, pos1, pos2)) ->
          if false then Format.eprintf "%d--%d@." pos1.pos_cnum pos2.pos_cnum;
          positions_in_stack env (i + 1)
      | None -> ()

    let get text checkpoint i =
      match checkpoint with
      | P.MenhirInterpreter.HandlingError env -> (
          match P.MenhirInterpreter.get i env with
          | Some (Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
          | None -> "???")
      | _ -> assert false

    let fail ~color text buffer checkpoint =
      let env =
        match checkpoint with
        | P.MenhirInterpreter.HandlingError env -> env
        | _ -> assert false
      in
      positions_in_stack env 0;

      let location = E.last buffer in
      let s = state checkpoint in
      let message =
        try Parser_messages.message s
        with Not_found -> Printf.sprintf "Syntax error (%d)\n" s
      in
      let message =
        if message = "<YOUR SYNTAX ERROR MESSAGE HERE>\n" then
          Printf.sprintf "Syntax error (%d)\n" s
        else message
      in
      let message = E.expand (get text checkpoint) message in
      let message = Printf.sprintf "%s (%d)" message s in
      report_syntax_error ~color text location message

    let parse_from_string ?color ~filename text =
      let lexbuf = initialize_lexing filename text in
      try
        let supplier =
          lexer_lexbuf_to_supplier Lexer.token Context.context lexbuf
        in
        let revised_parser =
          MenhirLib.Convert.Simplified.traditional2revised F.parse
        in
        revised_parser supplier
      with
      | F.Error ->
          let lexbuf = initialize_lexing filename text in
          let supplier =
            lexer_lexbuf_to_supplier Lexer.token Context.context lexbuf
          in
          let buffer, supplier = E.wrap_supplier supplier in
          let checkpoint =
            P.Incremental.parse (snd (Sedlexing.lexing_bytes_positions lexbuf))
          in
          P.MenhirInterpreter.loop_handle succeed (fail ~color text buffer)
            supplier checkpoint
      | Syntax_error (loc, msg) -> report_syntax_error ~color text loc msg
      | Sedlexing.InvalidCodepoint _ | Sedlexing.MalFormed ->
          report_syntax_error text ~color
            (Sedlexing.lexing_bytes_positions lexbuf)
            "Input file contains malformed UTF-8 byte sequences\n"
  end

  let parse_from_string ?color ~filename text =
    let ctx = Utils.Trivia.make () in
    let module Context = struct
      type t = Utils.Trivia.context

      let context = ctx
    end in
    let module I = Inner (Context) in
    (I.parse_from_string ?color ~filename text, ctx)

  let parse ?color ~filename () =
    parse_from_string ?color ~filename (read filename)
end
