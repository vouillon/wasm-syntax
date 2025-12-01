module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = Wax.Ast.location Wax.Ast.modulefield list
    end)
    (Wax.Parser)
    (Wax.Fast_parser)
    (Wax.Lexer)

let () = Format.printf "%a" Wax.Output.module_ (P.parse ~filename:"test.txt")
