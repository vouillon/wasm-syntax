{
open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let ident = ['a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '_' '0' - '9']*
let string = '"' [^ '"']* '"'
let int = ['0' - '9'] +

rule read =
  parse
  | white { read lexbuf }
  | newline { read lexbuf }
  | '&' { AMPERSAND }
  | '?' { QUESTIONMARK }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  | ':' { COLON }
  | "->" { ARROW }
  | '=' { EQUAL }
  | "'" { QUOTE }
  | "." { DOT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "<u" { LTU }
  | ">u" { GTU }
(*
  | ';' { SEMI }
*)
  | "i32" { I32 }
  | "i64" { I64 }
  | "f32" { F32 }
  | "f64" { F64 }
  | "v128" { V128 }
  | "i8" { I8 }
  | "i16" { I16 }
  | "func" { FUNC }
  | "nofunc" { NOFUNC }
  | "extern" { EXTERN }
  | "noextern" { NOEXTERN }
  | "any" { ANY }
  | "eq" { EQ }
  | "i31" { I31 }
  | "struct" { STRUCT }
  | "array" { ARRAY }
  | "none" { NONE }
  | "fn" { FN }
  | "mut" { MUT }
  | "type" { TYPE }
  | "rec" { REC }
  | "open" { OPEN }
  | "nop" { NOP }
  | "unreachable" { UNREACHABLE }
  | "loop" { LOOP }
  | "if" { IF }
  | "else" { ELSE }
  | "let" { LET }
  | "as" { AS }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | int { INT (Lexing.lexeme lexbuf) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | eof { EOF }
