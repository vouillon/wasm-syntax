{
open Parser
exception Error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let ident = ['a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '_' '0' - '9']*
let string = '"' [^ '"']* '"'
let int = ['0' - '9'] +

rule read =
  parse
  | white { read lexbuf }
  | newline { MenhirLib.LexerUtil.newline lexbuf; read lexbuf }
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
  | "_" { UNDERSCORE }
  | "<u" { LTU }
  | ">u" { GTU }
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
  | _
      { let location =
          MenhirLib.LexerUtil.range
            (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
        raise (Error (Printf.sprintf "%sUnexpected character '%s'.\n"
                        location (Lexing.lexeme lexbuf))) }
