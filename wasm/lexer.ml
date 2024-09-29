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
  [%sedlex.regexp?
    "0x", hexnum, Opt ('.', Opt hexnum), Opt (('p' | 'P'), sign, num)]

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
  | fN -> FLOAT (Sedlexing.Utf8.lexeme lexbuf)
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
  | "start" -> START
  | "memory" -> MEMORY
  | "data" -> DATA
  | "offset" -> OFFSET
  | "module" -> MODULE
  | "block" -> BLOCK
  | "loop" -> LOOP
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "unreachable" -> INSTR Unreachable
  | "nop" -> INSTR Nop
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
  | "i32.clz" -> INSTR (UnOp (I32 Clz))
  | "i32.ctz" -> INSTR (UnOp (I32 Ctz))
  | "i32.popcnt" -> INSTR (UnOp (I32 Popcnt))
  | "i32.eqz" -> INSTR (UnOp (I32 Eqz))
  | "i32.add" -> INSTR (BinOp (I32 Add))
  | "i32.sub" -> INSTR (BinOp (I32 Sub))
  | "i32.mul" -> INSTR (BinOp (I32 Mul))
  | "i32.div_s" -> INSTR (BinOp (I32 (Div Signed)))
  | "i32.div_u" -> INSTR (BinOp (I32 (Div Unsigned)))
  | "i32.rem_s" -> INSTR (BinOp (I32 (Rem Signed)))
  | "i32.rem_u" -> INSTR (BinOp (I32 (Rem Unsigned)))
  | "i32.and" -> INSTR (BinOp (I32 And))
  | "i32.or" -> INSTR (BinOp (I32 Or))
  | "i32.xor" -> INSTR (BinOp (I32 Xor))
  | "i32.shl" -> INSTR (BinOp (I32 Shl))
  | "i32.shr_s" -> INSTR (BinOp (I32 (Shr Signed)))
  | "i32.shr_u" -> INSTR (BinOp (I32 (Shr Unsigned)))
  | "i32.rotl" -> INSTR (BinOp (I32 Rotl))
  | "i32.rotr" -> INSTR (BinOp (I32 Rotr))
  | "i32.eq" -> INSTR (BinOp (I32 Eq))
  | "i32.ne" -> INSTR (BinOp (I32 Ne))
  | "i32.lt_s" -> INSTR (BinOp (I32 (Lt Signed)))
  | "i32.lt_u" -> INSTR (BinOp (I32 (Lt Unsigned)))
  | "i32.gt_s" -> INSTR (BinOp (I32 (Gt Signed)))
  | "i32.gt_u" -> INSTR (BinOp (I32 (Gt Unsigned)))
  | "i32.le_s" -> INSTR (BinOp (I32 (Le Signed)))
  | "i32.le_u" -> INSTR (BinOp (I32 (Le Unsigned)))
  | "i32.ge_s" -> INSTR (BinOp (I32 (Ge Signed)))
  | "i32.ge_u" -> INSTR (BinOp (I32 (Ge Unsigned)))
  | "i64.clz" -> INSTR (UnOp (I64 Clz))
  | "i64.ctz" -> INSTR (UnOp (I64 Ctz))
  | "i64.popcnt" -> INSTR (UnOp (I64 Popcnt))
  | "i64.eqz" -> INSTR (UnOp (I64 Eqz))
  | "i64.add" -> INSTR (BinOp (I64 Add))
  | "i64.sub" -> INSTR (BinOp (I64 Sub))
  | "i64.mul" -> INSTR (BinOp (I64 Mul))
  | "i64.div_s" -> INSTR (BinOp (I64 (Div Signed)))
  | "i64.div_u" -> INSTR (BinOp (I64 (Div Unsigned)))
  | "i64.rem_s" -> INSTR (BinOp (I64 (Rem Signed)))
  | "i64.rem_u" -> INSTR (BinOp (I64 (Rem Unsigned)))
  | "i64.and" -> INSTR (BinOp (I64 And))
  | "i64.or" -> INSTR (BinOp (I64 Or))
  | "i64.xor" -> INSTR (BinOp (I64 Xor))
  | "i64.shl" -> INSTR (BinOp (I64 Shl))
  | "i64.shr_s" -> INSTR (BinOp (I64 (Shr Signed)))
  | "i64.shr_u" -> INSTR (BinOp (I64 (Shr Unsigned)))
  | "i64.rotl" -> INSTR (BinOp (I64 Rotl))
  | "i64.rotr" -> INSTR (BinOp (I64 Rotr))
  | "i64.eq" -> INSTR (BinOp (I64 Eq))
  | "i64.ne" -> INSTR (BinOp (I64 Ne))
  | "i64.lt_s" -> INSTR (BinOp (I64 (Lt Signed)))
  | "i64.lt_u" -> INSTR (BinOp (I64 (Lt Unsigned)))
  | "i64.gt_s" -> INSTR (BinOp (I64 (Gt Signed)))
  | "i64.gt_u" -> INSTR (BinOp (I64 (Gt Unsigned)))
  | "i64.le_s" -> INSTR (BinOp (I64 (Le Signed)))
  | "i64.le_u" -> INSTR (BinOp (I64 (Le Unsigned)))
  | "i64.ge_s" -> INSTR (BinOp (I64 (Ge Signed)))
  | "i64.ge_u" -> INSTR (BinOp (I64 (Ge Unsigned)))
  | "f64.abs" -> INSTR (UnOp (F64 Abs))
  | "f64.neg" -> INSTR (UnOp (F64 Neg))
  | "f64.ceil" -> INSTR (UnOp (F64 Ceil))
  | "f64.floor" -> INSTR (UnOp (F64 Floor))
  | "f64.trunc" -> INSTR (UnOp (F64 Trunc))
  | "f64.nearest" -> INSTR (UnOp (F64 Nearest))
  | "f64.sqrt" -> INSTR (UnOp (F64 Sqrt))
  | "f64.add" -> INSTR (BinOp (F64 Add))
  | "f64.sub" -> INSTR (BinOp (F64 Sub))
  | "f64.mul" -> INSTR (BinOp (F64 Mul))
  | "f64.div" -> INSTR (BinOp (F64 Div))
  | "f64.min" -> INSTR (BinOp (F64 Min))
  | "f64.max" -> INSTR (BinOp (F64 Max))
  | "f64.copysign" -> INSTR (BinOp (F64 CopySign))
  | "f64.eq" -> INSTR (BinOp (F64 Eq))
  | "f64.ne" -> INSTR (BinOp (F64 Ne))
  | "f64.lt" -> INSTR (BinOp (F64 Lt))
  | "f64.gt" -> INSTR (BinOp (F64 Gt))
  | "f64.le" -> INSTR (BinOp (F64 Le))
  | "f64.ge" -> INSTR (BinOp (F64 Ge))
  | "i32.wrap_i64" -> INSTR I32WrapI64
  | "i32_trunc_f32_s" -> INSTR (UnOp (I32 (Trunc (`F32, Signed))))
  | "i32_trunc_f32_u" -> INSTR (UnOp (I32 (Trunc (`F32, Unsigned))))
  | "i32_trunc_f64_s" -> INSTR (UnOp (I32 (Trunc (`F64, Signed))))
  | "i32_trunc_f64_u" -> INSTR (UnOp (I32 (Trunc (`F64, Unsigned))))
  | "i32_trunc_sat_f32_s" -> INSTR (UnOp (I32 (TruncSat (`F32, Signed))))
  | "i32_trunc_sat_f32_u" -> INSTR (UnOp (I32 (TruncSat (`F32, Unsigned))))
  | "i32_trunc_sat_f64_s" -> INSTR (UnOp (I32 (TruncSat (`F64, Signed))))
  | "i32_trunc_sat_f64_u" -> INSTR (UnOp (I32 (TruncSat (`F64, Unsigned))))
  | "i64.extend_i32_s" -> INSTR (I64ExtendI32 Signed)
  | "i64.extend_i32_u" -> INSTR (I64ExtendI32 Unsigned)
  | "i64_trunc_f32_s" -> INSTR (UnOp (I64 (Trunc (`F32, Signed))))
  | "i64_trunc_f32_u" -> INSTR (UnOp (I64 (Trunc (`F32, Unsigned))))
  | "i64_trunc_f64_s" -> INSTR (UnOp (I64 (Trunc (`F64, Signed))))
  | "i64_trunc_f64_u" -> INSTR (UnOp (I64 (Trunc (`F64, Unsigned))))
  | "i64_trunc_sat_f32_s" -> INSTR (UnOp (I64 (TruncSat (`F32, Signed))))
  | "i64_trunc_sat_f32_u" -> INSTR (UnOp (I64 (TruncSat (`F32, Unsigned))))
  | "i64_trunc_sat_f64_s" -> INSTR (UnOp (I64 (TruncSat (`F64, Signed))))
  | "i64_trunc_sat_f64_u" -> INSTR (UnOp (I64 (TruncSat (`F64, Unsigned))))
  | "f32.convert_i32_s" -> INSTR (UnOp (F32 (Convert (`I32, Signed))))
  | "f32.convert_i32_u" -> INSTR (UnOp (F32 (Convert (`I32, Unsigned))))
  | "f32.convert_i64_s" -> INSTR (UnOp (F32 (Convert (`I64, Signed))))
  | "f32.convert_i64_u" -> INSTR (UnOp (F32 (Convert (`I64, Unsigned))))
  | "f32.demote_f64" -> INSTR F32DemoteF64
  | "f64.convert_i32_s" -> INSTR (UnOp (F64 (Convert (`I32, Signed))))
  | "f64.convert_i32_u" -> INSTR (UnOp (F64 (Convert (`I32, Unsigned))))
  | "f64.convert_i64_s" -> INSTR (UnOp (F64 (Convert (`I64, Signed))))
  | "f64.convert_i64_u" -> INSTR (UnOp (F64 (Convert (`I64, Unsigned))))
  | "f64.promote_f32" -> INSTR F64PromoteF32
  | "i32.reinterpret_f32" -> INSTR (UnOp (I32 Reinterpret))
  | "i64.reinterpret_f64" -> INSTR (UnOp (I64 Reinterpret))
  | "f32.reinterpret_i32" -> INSTR (UnOp (F32 Reinterpret))
  | "f64.reinterpret_i64" -> INSTR (UnOp (F64 Reinterpret))
  | "i32.extend8_s" -> INSTR (UnOp (I32 (ExtendS `_8)))
  | "i32.extend16_s" -> INSTR (UnOp (I32 (ExtendS `_8)))
  | "i64.extend8_s" -> INSTR (UnOp (I32 (ExtendS `_8)))
  | "i64.extend16_s" -> INSTR (UnOp (I32 (ExtendS `_16)))
  | "i64.extend32_s" -> INSTR (UnOp (I32 (ExtendS `_32)))
  | "extern.convert_any" -> INSTR ExternConvertAny
  | "any.convert_extern" -> INSTR AnyConvertExtern
  | "tuple" -> TUPLE
  | "tuple.make" -> TUPLE_MAKE
  | "tuple.extract" -> TUPLE_EXTRACT
  | _ ->
      raise
        (Misc.Syntax_error
           (Sedlexing.lexing_positions lexbuf, Printf.sprintf "Syntax error.\n"))
