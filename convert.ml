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
  ignore (From_wasm.module_ ast)
