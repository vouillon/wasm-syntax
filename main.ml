
let parse filename =
  let ch = In_channel.open_bin filename in
  let lexbuf = Lexing.from_channel ch in
  Parser.module_ Lexer.read lexbuf


let _ = parse "test.txt"
