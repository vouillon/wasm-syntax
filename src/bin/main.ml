open Cmdliner
open Term.Syntax

module Wat_parser =
  Wasm.Parsing.Make_parser
    (struct
      type t = string option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
    end)
    (Wasm.Parser)
    (Wasm.Fast_parser)
    (Wasm.Lexer)

module Wax_parser =
  Wasm.Parsing.Make_parser
    (struct
      type t = Wax.Ast.location Wax.Ast.modulefield list
    end)
    (Wax.Parser)
    (Wax.Fast_parser)
    (Wax.Lexer)

let with_open_in file f =
  match file with
  | Some file -> In_channel.with_open_bin file f
  | None -> f stdin

let with_open_out file f =
  match file with
  | Some file -> Out_channel.with_open_bin file f
  | None -> f stdout

let wat_to_wat ~input_file ~output_file =
  let text = with_open_in input_file In_channel.input_all in
  let ast =
    Wat_parser.parse_from_string
      ~filename:(Option.value ~default:"-" input_file)
      text
  in
  Wasm.Validation.f ast;
  let print_wat f m = Utils.Printer.run f (fun p -> Wasm.Output.module_ p m) in
  with_open_out output_file (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@." print_wat ast)

let wat_to_wax ~input_file ~output_file =
  let text = with_open_in input_file In_channel.input_all in
  let ast =
    Wat_parser.parse_from_string
      ~filename:(Option.value ~default:"-" input_file)
      text
  in
  Wasm.Validation.f ast;
  let ast = Conversion.From_wasm.module_ ast in
  Wax.Typing.f ast;
  let print_wax f m = Utils.Printer.run f (fun p -> Wax.Output.module_ p m) in
  with_open_out output_file (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@." print_wax ast)

let wax_to_wax ~input_file ~output_file =
  let text = with_open_in input_file In_channel.input_all in
  let ast =
    Wax_parser.parse_from_string
      ~filename:(Option.value ~default:"-" input_file)
      text
  in
  Wax.Typing.f ast;
  let print_wax f m = Utils.Printer.run f (fun p -> Wax.Output.module_ p m) in
  with_open_out output_file (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@." print_wax ast)

type format = Wat | Wasm | Wax

let string_of_format = function Wat -> "wat" | Wasm -> "wasm" | Wax -> "wax"

let format_description = function
  | Wat -> "Wasm text format"
  | Wasm -> "Wasm binary format"
  | Wax -> "Wax language"

let format_of_string = function
  | "wat" -> Ok Wat
  | "wasm" -> Ok Wasm
  | "wax" -> Ok Wax
  | s -> Error (`Msg (Printf.sprintf "Unknown format: %s" s))

let detect_format filename =
  if Filename.check_suffix filename ".wat" then Some Wat
  else if Filename.check_suffix filename ".wasm" then Some Wasm
  else if Filename.check_suffix filename ".wax" then Some Wax
  else None

let resolve_format file_opt format_opt ~default =
  match (file_opt, format_opt) with
  | _, Some fmt -> fmt
  | Some file, None -> (
      match detect_format file with Some fmt -> fmt | None -> default)
  | None, None -> default

let convert input_file output_file input_format_opt output_format_opt =
  let std file = Option.bind file (fun f -> if f = "-" then None else Some f) in
  let input_file = std input_file in
  let output_file = std output_file in
  let input_format = resolve_format input_file input_format_opt ~default:Wax in
  let output_format =
    resolve_format output_file output_format_opt ~default:Wasm
  in
  match (input_format, output_format) with
  | Wat, Wat -> wat_to_wat ~input_file ~output_file
  | Wat, Wax -> wat_to_wax ~input_file ~output_file
  | Wax, Wax -> wax_to_wax ~input_file ~output_file
  | _ ->
      Printf.eprintf "Error: Conversion from '%s' (%s) to '%s' (%s) not supported yet. Run 'wax --help' for supported formats and their descriptions.\n"
        (string_of_format input_format)
        (format_description input_format)
        (string_of_format output_format)
        (format_description output_format);
      exit 1

(* Define the input file argument (optional for stdin) *)
let input_file =
  let doc =
    "Input file (.wat, .wasm, or .wax). Reads from stdin if not specified."
  in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

(* Define the --output/-o option *)
let output_file =
  let doc = "Output file. Writes to stdout if not specified." in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

(* Define the --input-format option *)
let input_format =
  let doc =
    "Input format: wat (Wasm text format), wasm (Wasm binary format), or wax \
     (Wax language). If not specified, auto-detected from filename or defaults \
     to wax."
  in
  let format_conv =
    Arg.conv
      ( format_of_string,
        fun ppf fmt -> Format.fprintf ppf "%s" (string_of_format fmt) )
  in
  Arg.(
    value
    & opt (some format_conv) None
    & info [ "i"; "input-format" ] ~docv:"FORMAT" ~doc)

(* Define the --output-format option *)
let output_format =
  let doc =
    "Output format: wat (Wasm text format), wasm (Wasm binary format), or wax \
     (Wax language). If not specified, defaults to wasm."
  in
  let format_conv =
    Arg.conv
      ( format_of_string,
        fun ppf fmt -> Format.fprintf ppf "%s" (string_of_format fmt) )
  in
  Arg.(
    value
    & opt (some format_conv) None
    & info [ "f"; "format"; "output-format" ] ~docv:"FORMAT" ~doc)

(* Combine into command *)
let convert_term =
  let+ input = input_file
  and+ output = output_file
  and+ in_fmt = input_format
  and+ out_fmt = output_format in
  convert input output in_fmt out_fmt

let convert_cmd =
  let doc = "Convert between WebAssembly formats (.wat, .wasm, .wax)" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Convert between different WebAssembly formats: .wat (text), .wasm \
         (binary), and .wax.";
      `P "Supports reading from stdin and writing to stdout.";
      `P "Default conversion: wax -> wasm";
      `S Manpage.s_examples;
      `P "Convert file with auto-detected formats:";
      `Pre "  $(tname) input.wasm -o output.wat";
      `P "Read from stdin, write to stdout:";
      `Pre "  cat input.wasm | $(tname) -i wasm -f wat > output.wat";
      `P "Read from stdin, write to file:";
      `Pre "  cat input.wat | $(tname) -i wat -o output.wasm";
      `P "Explicit format specification:";
      `Pre "  $(tname) input -i wasm -o output -f wat";
      `S Manpage.s_options;
    ]
  in
  let info = Cmd.info "converter" ~version:"1.0" ~doc ~man in
  Cmd.v info convert_term

let () = exit (Cmd.eval convert_cmd)
