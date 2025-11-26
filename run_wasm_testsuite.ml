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

module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = unit list
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
        let _ast = P.parse ~filename in
        ())
  in
  ()

let () =
  iter_files dirs
    (fun p ->
      List.mem p
        [ "spec-test-1"; "memory64"; "simd"; "multi-memory"; "relaxed-simd" ])
    ".wast" runtest
