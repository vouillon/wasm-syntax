(*
- perform parsing and validation tests
- output the tests and read them back to test the text output
  ==> we need to isolate the tests from what is parsed
- fix remaining issues
- conversion to rust-like format and typing
*)

let iter_files dirs skip suffix f =
  let rec visit dir =
    let entries = Sys.readdir dir in
    Array.iter
      (fun entry ->
        let path = Filename.concat dir entry in
        if not (skip entry) then
          if Sys.is_directory path then visit path
          else if Filename.check_suffix entry suffix then f path)
      entries
  in
  List.iter visit dirs

let dirs =
  [
    "/home/jerome/sources/Wasm/test/core";
    "/home/jerome/sources/Wasm/spectec/test-interpreter";
  ]

type script =
  ([ `Valid | `Invalid | `Malformed ]
  * [ `Parsed of string option * Ast.location Wasm.Ast.Text.modulefield list
    | `Text of string ])
  list

module Parser = struct
  include Wasm.Parser

  module Incremental = struct
    let parse = Wasm.Parser.Incremental.parse_script
  end
end

module Fast_parser = struct
  include Wasm.Fast_parser

  let parse = Wasm.Fast_parser.parse_script
end

module ModuleParser =
  Wasm.Parsing.Make_parser
    (struct
      type t = string option * Ast.location Wasm.Ast.Text.modulefield list
    end)
    (Wasm.Parser)
    (Wasm.Fast_parser)
    (Wasm.Lexer)

module ScriptParser =
  Wasm.Parsing.Make_parser
    (struct
      type t = script
    end)
    (Parser)
    (Fast_parser)
    (Wasm.Lexer)

let in_child_process f =
  match Unix.fork () with
  | 0 ->
      f ();
      exit 0
  | pid ->
      (* Parent process *)
      let _, status = Unix.waitpid [] pid in
      status = Unix.WEXITED 0

let runtest filename =
  prerr_endline filename;
  let _ =
    in_child_process (fun () ->
        let lst = ScriptParser.parse ~filename in
        (* Parsing *)
        let lst =
          List.filter_map
            (fun (status, m) ->
              match (status, m) with
              | ((`Valid | `Invalid) as status), `Parsed m -> Some (status, m)
              | ((`Valid | `Invalid) as status), `Text txt ->
                  Some (status, ModuleParser.parse_from_string ~filename txt)
              | `Malformed, `Parsed _ -> assert false
              | `Malformed, `Text _ ->
                  (* parse (should fail) *)
                  None)
            lst
        in
        List.iter
          (fun (_, m) ->
            let text = Format.asprintf "%a@." Wasm.Output.module_ m in
            if false then prerr_endline text;
            let _ast = ModuleParser.parse_from_string ~filename text in
            ())
          lst;
        if false then
          List.iter
            (fun (status, m) ->
              match (status, m) with
              | `Valid, m -> Wasm.Validation.f m
              | `Invalid, _ -> (* validate (should fail) *) ())
            lst)
  in
  ()

let () =
  iter_files dirs
    (fun p ->
      List.mem p
        [
          "spec-test-1" (* invalid syntax *); "memory64"; "simd"; "relaxed-simd";
        ])
    ".wast" runtest
