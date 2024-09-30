open Wasm

module P =
  Parsing.Make_parser
    (struct
      type t = string option * Ast.Text.modulefield list
    end)
    (Parser)
    (Fast_parser)
    (Lexer)

let _ =
  let p = "/home/jerome/wasm_of_ocaml/runtime/wasm" in
  let ast = P.parse ~filename:(Filename.concat p "int32.wat") in
  Format.printf "%a" Output.module_
    (List.filter_map (fun x -> x) (From_wasm.module_ ast))
