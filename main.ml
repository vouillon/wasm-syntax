module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = Ast.modulefield list
    end)
    (Parser)
    (Fast_parser)
    (Lexer)

let () = Format.printf "%a" Output.module_ (P.parse ~filename:"test.txt")
