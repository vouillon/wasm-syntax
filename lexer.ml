open Parser

let white = [%sedlex.regexp? Plus (' ' | '\t')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let ident =
  [%sedlex.regexp?
    ('a' .. 'z' | 'A' .. 'Z'), Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9')]

let string = [%sedlex.regexp? '"', Star (Sub (any, '"')), '"']

let int =
  [%sedlex.regexp?
    Plus '0' .. '9' | "0x", Plus ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F')]

let rec token lexbuf =
  match%sedlex lexbuf with
  | white | newline -> token lexbuf
  | ';' -> SEMI
  | '&' -> AMPERSAND
  | '|' -> PIPE
  | '?' -> QUESTIONMARK
  | '(' -> LPAREN
  | ')' -> RPAREN
  | "{" -> LBRACE
  | '}' -> RBRACE
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | ',' -> COMMA
  | ':' -> COLON
  | "->" -> ARROW
  | '=' -> EQUAL
  | ":=" -> COLONEQUAL
  | "'" -> QUOTE
  | "." -> DOT
  | "+" -> PLUS
  | "-" -> MINUS
  | "_" -> UNDERSCORE
  | "<s" -> LTS
  | "<u" -> LTU
  | ">s" -> GTS
  | ">u" -> GTU
  | "fn" -> FN
  | "mut" -> MUT
  | "type" -> TYPE
  | "rec" -> REC
  | "open" -> OPEN
  | "nop" -> NOP
  | "unreachable" -> UNREACHABLE
  | "loop" -> LOOP
  | "if" -> IF
  | "else" -> ELSE
  | "let" -> LET
  | "as" -> AS
  | "is" -> IS
  | "br" -> BR
  | "br_if" -> BR_IF
  | "br_table" -> BR_TABLE
  | "br_on_null" -> BR_ON_NULL
  | "br_on_non_null" -> BR_ON_NON_NULL
  | "br_on_cast" -> BR_ON_CAST
  | "br_on_cast_fail" -> BR_ON_CAST_FAIL
  | "return" -> RETURN
  | ident -> IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | int -> INT (Sedlexing.Utf8.lexeme lexbuf)
  | string -> STRING (Sedlexing.Utf8.lexeme lexbuf)
  | eof -> EOF
  | _ ->
      raise
        (Misc.Syntax_error
           ( Sedlexing.lexing_positions lexbuf,
             Printf.sprintf "Unexpected character '%s'.\n"
               (Sedlexing.Utf8.lexeme lexbuf) ))
