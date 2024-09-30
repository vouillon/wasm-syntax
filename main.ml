module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = Ast.modulefield list
    end)
    (Parser)
    (Fast_parser)
    (Lexer)

let _ = P.parse ~filename:"test.txt"
