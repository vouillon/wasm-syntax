let sign = [%sedlex.regexp? Opt ('+' | '-')]
let digit = [%sedlex.regexp? '0' .. '9']
let hexdigit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let num = [%sedlex.regexp? digit, Star (Opt '_', digit)]
let hexnum = [%sedlex.regexp? hexdigit, Star (Opt '_', hexdigit)]
let uN = [%sedlex.regexp? num | "0x", hexnum]
let sN = [%sedlex.regexp? sign, uN]
let iN = [%sedlex.regexp? uN | sN]

let float =
  [%sedlex.regexp? num, Opt ('.', Opt num), Opt (('e' | 'E'), sign, num)]

let hexfloat =
  [%sedlex.regexp? hexnum, Opt ('.', Opt hexnum), Opt (('p' | 'P'), sign, num)]

let fN =
  [%sedlex.regexp? sign, (float | hexfloat | "inf" | "nan" | "nan:", hexnum)]

let idchar =
  [%sedlex.regexp?
    ( '0' .. '9'
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/' | ':'
    | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~' )]

let id = [%sedlex.regexp? '$', Plus idchar]
let linechar = [%sedlex.regexp? Sub (any, (10 | 13))]
let newline = [%sedlex.regexp? 10 | 13 | 13, 10]
let linecomment = [%sedlex.regexp? ";;", Star linechar, (newline | eof)]
let format = [%sedlex.regexp? '\n' | 9]
(*
let space = [%sedlex.regexp? ' ' | format | comment]
*)

type token =
  | NAT of string
  | INT of string
  | FLOAT of string
  | STRING of string
  | ID of string
  | LPAREN
  | RPAREN

let rec comment lexbuf =
  match%sedlex lexbuf with
  | ";)" -> ()
  | "(;" ->
      comment lexbuf;
      comment lexbuf
  | ';' | '(' | Plus (Sub (any, (';' | '('))) -> comment lexbuf
  | _ -> assert false (*ZZZ*)

let string_buffer = Buffer.create 256

let stringchar =
  [%sedlex.regexp?
    ( Sub (any, (0 .. 31 | 0x7f | '"' | '\''))
    | "\t" | "\\n" | "\\r" | "\\\"" | "\\'" | "\\\\"
    | "\\u{", hexnum, "}" )]

let stringelem = [%sedlex.regexp? stringchar | '\\', hexdigit, hexdigit]
let string = [%sedlex.regexp? '"', Star stringelem, '"']

let rec string lexbuf =
  match%sedlex lexbuf with
  | '"' ->
      let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      s
  | Plus (Sub (any, (0 .. 31 | 0x7f | '"' | '\''))) ->
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
  | _ -> assert false

(* ZZZ names should be well-formed utf strings... *)

let rec token lexbuf =
  match%sedlex lexbuf with
  | '(' -> LPAREN
  | ')' -> RPAREN
  | uN -> NAT (Sedlexing.Utf8.lexeme lexbuf)
  | sN -> INT (Sedlexing.Utf8.lexeme lexbuf)
  | float -> FLOAT (Sedlexing.Utf8.lexeme lexbuf)
  | '"' -> STRING (string lexbuf)
  | newline | linecomment -> (*ZZZ new line *) token lexbuf
  | Plus (' ' | '\t') -> token lexbuf
  | "(;" ->
      comment lexbuf;
      token lexbuf
  | id ->
      ID
        (Sedlexing.Utf8.sub_lexeme lexbuf 1
           (Sedlexing.lexeme_length lexbuf - 1))
  | _ -> assert false
