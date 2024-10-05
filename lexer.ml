open Parser

let white = [%sedlex.regexp? Plus (' ' | '\t')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let ident =
  [%sedlex.regexp?
    ('a' .. 'z' | 'A' .. 'Z'), Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9')]

let string = [%sedlex.regexp? '"', Star (Sub (any, '"')), '"']
let sign = [%sedlex.regexp? Opt ('+' | '-')]
let digit = [%sedlex.regexp? '0' .. '9']
let hexdigit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let num = [%sedlex.regexp? digit, Star (Opt '_', digit)]
let hexnum = [%sedlex.regexp? hexdigit, Star (Opt '_', hexdigit)]
let int = [%sedlex.regexp? num | "0x", hexnum]

let decfloat =
  [%sedlex.regexp?
    num, Opt ('.', Opt num), (('e' | 'E'), sign, num) | num, '.', Opt num]

let hexfloat =
  [%sedlex.regexp?
    ( "0x", hexnum, Opt ('.', Opt hexnum), (('p' | 'P'), sign, num)
    | "0x", hexnum, '.', Opt hexnum )]

let float =
  [%sedlex.regexp? decfloat | hexfloat | "inf" | "nan" | "nan:", hexnum]

let string_buffer = Buffer.create 256

let rec string lexbuf =
  match%sedlex lexbuf with
  | '"' ->
      let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      s
  | Plus (Sub (any, (0 .. 31 | 0x7f | '"' | '\\'))) ->
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      string lexbuf
  | "\\t" ->
      Buffer.add_char string_buffer '\t';
      string lexbuf
  | "\\n" ->
      Buffer.add_char string_buffer '\n';
      string lexbuf
  | "\\r" ->
      Buffer.add_char string_buffer '\r';
      string lexbuf
  | "\\'" ->
      Buffer.add_char string_buffer '\'';
      string lexbuf
  | "\\\"" ->
      Buffer.add_char string_buffer '"';
      string lexbuf
  | "\\\\" ->
      Buffer.add_char string_buffer '\\';
      string lexbuf
  | '\\', hexdigit, hexdigit ->
      let s = String.sub (Sedlexing.Utf8.lexeme lexbuf) 1 2 in
      Buffer.add_char string_buffer (Char.chr (int_of_string ("0x" ^ s)));
      string lexbuf
  | "\\u{", hexnum, "}" ->
      let n =
        Sedlexing.Utf8.sub_lexeme lexbuf 3 (Sedlexing.lexeme_length lexbuf - 4)
      in
      Buffer.add_utf_8_uchar string_buffer
        (Uchar.unsafe_of_int (int_of_string ("0x" ^ n)));
      string lexbuf
  | _ ->
      raise
        (Wasm.Parsing.Syntax_error
           ( Sedlexing.lexing_positions lexbuf,
             Printf.sprintf "Malformed string.\n" ))

let rec token lexbuf =
  match%sedlex lexbuf with
  | white | newline -> token lexbuf
  | ';' -> SEMI
  | '#' -> SHARP
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
  | "*" -> STAR
  | "/" -> SLASH
  | "/s" -> SLASHS
  | "/u" -> SLASHU
  | "%s" -> PERCENTS
  | "%u" -> PERCENTU
  | '&' -> AMPERSAND
  | '|' -> PIPE
  | '^' -> CARET
  | "<<" -> SHL
  | ">>s" -> SHRS
  | ">>u" -> SHRU
  | "==" -> EQUALEQUAL
  | "!=" -> BANGEQUAL
  | "_" -> UNDERSCORE
  | ">" -> GT
  | ">s" -> GTS
  | ">u" -> GTU
  | "<" -> LT
  | "<s" -> LTS
  | "<u" -> LTU
  | ">=" -> GE
  | ">=s" -> GES
  | ">=u" -> GEU
  | "<=" -> LE
  | "<=s" -> LES
  | "<=u" -> LEU
  | "null" -> NULL
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
  | "throw" -> THROW
  | ident -> IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | int -> INT (Sedlexing.Utf8.lexeme lexbuf)
  | float -> FLOAT (Sedlexing.Utf8.lexeme lexbuf)
  | '"' -> STRING (string lexbuf)
  | eof -> EOF
  | _ ->
      raise
        (Wasm.Parsing.Syntax_error
           ( Sedlexing.lexing_positions lexbuf,
             Printf.sprintf "Unexpected character '%s'.\n"
               (Sedlexing.Utf8.lexeme lexbuf) ))
