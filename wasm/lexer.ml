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
  let open Parser in
  match%sedlex lexbuf with
  | '(' -> LPAREN
  | ')' -> RPAREN
  | uN -> NAT (Sedlexing.Utf8.lexeme lexbuf)
  | sN -> INT (Sedlexing.Utf8.lexeme lexbuf)
  | float -> FLOAT (Sedlexing.Utf8.lexeme lexbuf)
  | '"' -> STRING (string lexbuf)
  | newline | linecomment -> token lexbuf
  | Plus (' ' | '\t') -> token lexbuf
  | "(;" ->
      comment lexbuf;
      token lexbuf
  | id ->
      ID
        (Sedlexing.Utf8.sub_lexeme lexbuf 1
           (Sedlexing.lexeme_length lexbuf - 1))
  | eof -> EOF
  | "i32" -> VALTYPE I32
  | "i64" -> VALTYPE I64
  | "f32" -> VALTYPE F32
  | "f64" -> VALTYPE F64
  | "v128" -> VALTYPE V128
  | "i8" -> PACKEDTYPE I8
  | "i6" -> PACKEDTYPE I16
  | "any" -> ANY
  | "eq" -> EQ
  | "i31" -> I31
  | "struct" -> STRUCT
  | "array" -> ARRAY
  | "none" -> NONE
  | "func" -> FUNC
  | "nofunc" -> NOFUNC
  | "extern" -> EXTERN
  | "noextern" -> NOEXTERN
  | "anyref" -> ANYREF
  | "eqref" -> EQREF
  | "i31ref" -> I31REF
  | "structref" -> STRUCTREF
  | "arrayref" -> ARRAYREF
  | "nullref" -> NULLREF
  | "funcref" -> FUNCREF
  | "nullfuncref" -> NULLFUNCREF
  | "externref" -> EXTERNREF
  | "nullexternref" -> NULLEXTERNREF
  | "ref" -> REF
  | "null" -> NULL
  | "param" -> PARAM
  | "result" -> RESULT
  | "mut" -> MUT
  | "field" -> FIELD
  | "rec" -> REC
  | "type" -> TYPE
  | "sub" -> SUB
  | "final" -> FINAL
  | "import" -> IMPORT
  | "export" -> EXPORT
  | "local" -> LOCAL
  | "global" -> GLOBAL
  | "memory" -> MEMORY
  | "data" -> DATA
  | "offset" -> OFFSET
  | "module" -> MODULE
  | "block" -> BLOCK
  | "loop" -> LOOP
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "br" -> BR
  | "br_if" -> BR_IF
  | "br_table" -> BR_TABLE
  | "br_on_null" -> BR_ON_NULL
  | "br_on_non_null" -> BR_ON_NON_NULL
  | "br_on_cast" -> BR_ON_CAST
  | "br_on_cast_fail" -> BR_ON_CAST_FAIL
  | "return" -> RETURN
  | "call" -> CALL
  | "call_ref" -> CALL_REF
  | "call_indirect" -> CALL_INDIRECT
  | "return_call" -> RETURN_CALL
  | "return_call_ref" -> RETURN_CALL_REF
  | "return_call_indirect" -> RETURN_CALL_INDIRECT
  | "drop" -> DROP
  | "select" -> SELECT
  | "local.get" -> LOCAL_GET
  | "local.set" -> LOCAL_SET
  | "local.tee" -> LOCAL_TEE
  | "global.get" -> GLOBAL_GET
  | "global.set" -> GLOBAL_SET
  | "ref.null" -> REF_NULL
  | "ref.func" -> REF_FUNC
  | "ref.is_null" -> REF_IS_NULL
  | "ref.as_non_null" -> REF_AS_NON_NULL
  | "ref.eq" -> REF_EQ
  | "ref.test" -> REF_TEST
  | "ref.cast" -> REF_CAST
  | "struct.new" -> STRUCT_NEW
  | "struct.new_default" -> STRUCT_NEW_DEFAULT
  | "struct.get" -> STRUCT_GET None
  | "struct.get_u" -> STRUCT_GET (Some Unsigned)
  | "struct.get_s" -> STRUCT_GET (Some Signed)
  | "struct.set" -> STRUCT_SET
  | "array.new" -> ARRAY_NEW
  | "array.new_default" -> ARRAY_NEW_DEFAULT
  | "array.new_fixed" -> ARRAY_NEW_FIXED
  | "array.new_data" -> ARRAY_NEW_DATA
  | "array.new_elem" -> ARRAY_NEW_ELEM
  | "array.get" -> ARRAY_GET None
  | "array.get_u" -> ARRAY_GET (Some Unsigned)
  | "array.get_s" -> ARRAY_GET (Some Signed)
  | "array.set" -> ARRAY_SET
  | "array.len" -> ARRAY_LEN
  | "array.fill" -> ARRAY_FILL
  | "array.copy" -> ARRAY_COPY
  | "ref.i31" -> REF_I31
  | "i31.get_s" -> I31_GET Signed
  | "i31.get_u" -> I31_GET Unsigned
  | "i32.const" -> I32_CONST
  | "i64.const" -> I64_CONST
  | "f32.const" -> F32_CONST
  | "f64.const" -> F64_CONST
  | "i32.add" -> I32_ADD
  | "i32.sub" -> I32_SUB
  | "i32.mul" -> I32_MUL
  | "i32.div_s" -> I32_DIV Signed
  | "i32.div_u" -> I32_DIV Unsigned
  | "i32.rem_s" -> I32_REM Signed
  | "i32.rem_u" -> I32_REM Unsigned
  | "i32.and" -> I32_AND
  | "i32.or" -> I32_OR
  | "i32.xor" -> I32_XOR
  | "i32.shl" -> I32_SHL
  | "i32.shr_s" -> I32_SHR Signed
  | "i32.shr_u" -> I32_SHR Unsigned
  | "i32.rotl" -> I32_ROTL
  | "i32.rotr" -> I32_ROTR
  | "i32.eqz" -> I32_EQZ
  | "i32.eq" -> I32_EQ
  | "i32.ne" -> I32_NE
  | "i32.lt_s" -> I32_LT Signed
  | "i32.lt_u" -> I32_LT Unsigned
  | "i32.gt_s" -> I32_GT Signed
  | "i32.gt_u" -> I32_GT Unsigned
  | "i32.le_s" -> I32_LE Signed
  | "i32.le_u" -> I32_LE Unsigned
  | "i32.ge_s" -> I32_GE Signed
  | "i32.ge_u" -> I32_GE Unsigned
  | "tuple.make" -> TUPLE_MAKE
  | _ ->
      raise
        (Misc.Syntax_error
           (Sedlexing.lexing_positions lexbuf, Printf.sprintf "Syntax error.\n"))
