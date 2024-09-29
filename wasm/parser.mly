%token <string> NAT
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID
%token <Ast.valtype> VALTYPE
%token <Ast.packedtype> PACKEDTYPE
%token LPAREN "("
%token RPAREN ")"
%token EOF
%token ANY
%token EQ
%token I31
%token STRUCT
%token ARRAY
%token NONE
%token FUNC
%token NOFUNC
%token EXTERN
%token NOEXTERN
%token ANYREF
%token EQREF
%token I31REF
%token STRUCTREF
%token ARRAYREF
%token NULLREF
%token FUNCREF
%token NULLFUNCREF
%token EXTERNREF
%token NULLEXTERNREF
%token REF
%token NULL
%token PARAM
%token RESULT
%token MUT
%token FIELD
%token REC
%token TYPE
%token SUB
%token FINAL
%token IMPORT
%token EXPORT
%token LOCAL
%token GLOBAL
%token MEMORY
%token DATA
%token OFFSET
%token MODULE

%token BLOCK
%token LOOP
%token END
%token IF
%token THEN
%token ELSE
%token BR
%token BR_IF
%token BR_TABLE
%token BR_ON_NULL
%token BR_ON_NON_NULL
%token BR_ON_CAST
%token BR_ON_CAST_FAIL
%token RETURN
%token CALL
%token CALL_REF
%token CALL_INDIRECT
%token RETURN_CALL
%token RETURN_CALL_REF
%token RETURN_CALL_INDIRECT
%token DROP
%token SELECT
%token LOCAL_GET
%token LOCAL_SET
%token LOCAL_TEE
%token GLOBAL_GET
%token GLOBAL_SET
%token REF_NULL
%token REF_FUNC
%token REF_IS_NULL
%token REF_AS_NON_NULL
%token REF_EQ
%token REF_TEST
%token REF_CAST
%token STRUCT_NEW
%token STRUCT_NEW_DEFAULT
%token <Ast.signage option> STRUCT_GET
%token STRUCT_SET
%token ARRAY_NEW
%token ARRAY_NEW_DEFAULT
%token ARRAY_NEW_FIXED
%token ARRAY_NEW_DATA
%token ARRAY_NEW_ELEM
%token <Ast.signage option> ARRAY_GET
%token ARRAY_SET
%token ARRAY_LEN
%token ARRAY_FILL
%token ARRAY_COPY
%token REF_I31
%token <Ast.signage> I31_GET
%token I32_CONST
%token I64_CONST
%token F32_CONST
%token F64_CONST
%token I32_ADD
%token I32_SUB
%token I32_MUL
%token <Ast.signage> I32_DIV
%token <Ast.signage> I32_REM
%token I32_AND
%token I32_OR
%token I32_XOR
%token I32_SHL
%token <Ast.signage> I32_SHR
%token I32_ROTL
%token I32_ROTR
%token I32_EQZ
%token I32_EQ
%token I32_NE
%token <Ast.signage> I32_LT
%token <Ast.signage> I32_GT
%token <Ast.signage> I32_LE
%token <Ast.signage> I32_GE
%token TUPLE_MAKE

%{
open Ast

let map_fst f (x, y) = (f x, y)
%}

%start <string option * Ast.modulefield list> module_

%%

u32: n = NAT { Int32.of_string n (*ZZZ overflow / unsigned*) }

i32:
  n = NAT { Int32.of_string n }
| i = INT { Int32.of_string i }

i64:
  n = NAT { Int64.of_string n }
| i = INT { Int64.of_string i }

f32:
  n = NAT { n }
| i = INT { i }
| f = FLOAT { f }

f64:
  n = NAT { n }
| i = INT { i }
| f = FLOAT { f }

idx:
| n = u32 { Num n }
| i = ID { Id i }

name: s = STRING { s (*ZZZ check well-formedness*) }

(* Types *)

heaptype:
| ANY { Any }
| EQ { Eq }
| I31 { I31 }
| STRUCT { Struct }
| ARRAY { Array }
| NONE { None_ }
| FUNC { Func_ }
| NOFUNC { NoFunc }
| EXTERN { Extern }
| NOEXTERN { NoExtern }
| i = idx { Type i }

reftype:
| "(" REF nullable = boption(NULL) typ = heaptype ")"
  { { nullable; typ } }
| ANYREF { {nullable = true; typ = Any} }
| EQREF { {nullable = true; typ = Eq} }
| I31REF { {nullable = true; typ = I31} }
| STRUCTREF { {nullable = true; typ = Struct} }
| ARRAYREF { {nullable = true; typ = Array} }
| NULLREF { {nullable = true; typ = None_} }
| FUNCREF { {nullable = true; typ = Func_} }
| NULLFUNCREF { {nullable = true; typ = NoFunc} }
| EXTERNREF { {nullable = true; typ = Extern} }
| NULLEXTERNREF { {nullable = true; typ = NoExtern} }

valtype:
| t = VALTYPE { t }
| t = reftype { Ref t }

functype:
| "(" FUNC r = params_and_results(")")
  { fst r }

params(cont):
| c = cont { [], c }
| "(" PARAM ID t = valtype ")" rem = params(cont)
  { map_fst (fun l -> t :: l) rem }
| "(" PARAM l = valtype * ")" rem = params(cont)
  { map_fst ((@) l) rem }

results(cont):
| c = cont { [], c }
| "(" RESULT l = valtype * ")" rem = results(cont)
  { map_fst ((@) l) rem }

params_and_results(cont):
| r = params(results(cont))
  { let (params, (result, c)) = r in ({params; result }, c) }

field:
| "(" FIELD i = ID t = fieldtype ")" { [(Some i, t)] }
| "(" FIELD l = fieldtype * ")" { List.map (fun t -> (None, t)) l }

fieldtype:
| typ = storagetype { {mut = false; typ} }
| "(" MUT typ = storagetype ")" { {mut = true; typ} }

storagetype:
| t = valtype { Value t }
| t = PACKEDTYPE { Packed t }

comptype:
| "(" ARRAY t = fieldtype ")" { Array t }
| "(" STRUCT l = field * ")" { Struct (List.flatten l) }
| t = functype { Func t }

rectype:
| "(" REC l = typedef * ")" { Types l }
| t = typedef { Types [t] }

typedef:
| "(" TYPE name = ID ? t = subtype ")" { t name }

subtype:
| "(" SUB final = boption(FINAL) supertype = idx ? typ = comptype
  ")"
  { fun name -> {name; final; supertype; typ} }
| typ = comptype { fun name -> {name; final = true; supertype = None; typ } }

(* Instructions *)

blockinstr:
| BLOCK label = label bti = blocktype(instrs(END))
  { let (typ, block) = bti in Block {label; typ; block} }
| LOOP label = label bti = blocktype(instrs(END))
  { let (typ, block) = bti in Loop {label; typ; block} }
| IF label = label bti = blocktype(instrs(ELSE))
  label else_block = instrs(END)
  label (*ZZZ labels must match *)
  { let (typ, if_block) = bti in
    If {label; typ; if_block; else_block } }
| IF label = label bti = blocktype(instrs(END))
  label (*ZZZ labels must match *)
  { let (typ, if_block) = bti in
    If {label; typ; if_block; else_block = [] } }

label:
| i = ID ? { i }

blocktype(cont):
| r = typeuse(cont)
  { map_fst (fun t -> Option.map (fun v -> Idx v) (fst t)) r }
  (*ZZZ single result / implicit? validate?*)

plaininstr:
| BR i = idx { Br i }
| BR_IF i = idx { Br_if i }
| BR_TABLE l = idx +
  { let l = List.rev l in Br_table (List.rev (List.tl l), List.hd l) }
| BR_ON_NULL i = idx { Br_on_null i }
| BR_ON_NON_NULL i = idx { Br_on_non_null i }
| BR_ON_CAST i = idx t1 = reftype t2 = reftype
  { Br_on_cast (i, t1, t2) }
| BR_ON_CAST_FAIL i = idx t1 = reftype t2 = reftype
  { Br_on_cast_fail (i, t1, t2) }
| RETURN { Return }
| CALL i = idx { Call i }
| CALL_REF i = idx { CallRef i }
| RETURN_CALL i = idx { ReturnCall i }
| RETURN_CALL_REF i = idx { ReturnCallRef i }
| DROP { Drop }
| LOCAL_GET i = idx { LocalGet i }
| LOCAL_SET i = idx { LocalSet i }
| LOCAL_TEE i = idx { LocalTee i }
| GLOBAL_GET i = idx { GlobalGet i }
| GLOBAL_SET i = idx { GlobalSet i }
| REF_NULL t = heaptype { RefNull t }
| REF_FUNC i = idx { RefFunc i }
| REF_IS_NULL { RefIsNull }
| REF_AS_NON_NULL { RefAsNonNull }
| REF_EQ { RefEq }
| REF_TEST t = reftype { RefTest t }
| REF_CAST t = reftype { RefCast t }
| STRUCT_NEW i = idx { StructNew i }
| STRUCT_NEW_DEFAULT i = idx { StructNewDefault i }
| s = STRUCT_GET i1 = idx i2 = idx { StructGet (s, i1, i2) }
| STRUCT_SET i1 = idx i2 = idx { StructSet (i1, i2) }
| ARRAY_NEW i = idx { ArrayNew i }
| ARRAY_NEW_DEFAULT i = idx { ArrayNewDefault i }
| ARRAY_NEW_FIXED i = idx l = u32 { ArrayNewFixed (i, l) }
| ARRAY_NEW_DATA i1 = idx i2 = idx { ArrayNewData (i1, i2) }
| ARRAY_NEW_ELEM i1 = idx i2 = idx { ArrayNewElem (i1, i2) }
| s = ARRAY_GET i = idx { ArrayGet (s, i) }
| ARRAY_SET i = idx { ArraySet i }
| ARRAY_LEN { ArrayLen }
| ARRAY_FILL i = idx { ArrayFill i }
| ARRAY_COPY i1 = idx i2 = idx { ArrayCopy (i1, i2) }
| REF_I31 { RefI31 }
| s = I31_GET { I31Get s }
| I32_CONST i = i32 { I32Const i }
| I64_CONST i = i64 { I64Const i }
| F32_CONST f = f32 { F32Const f }
| F64_CONST f = f64 { F64Const f }
| I32_ADD { I32Add }
| I32_SUB { I32Sub }
| I32_MUL { I32Mul }
| s = I32_DIV { I32Div s }
| s = I32_REM { I32Rem s }
| I32_AND { I32And }
| I32_OR { I32Or }
| I32_XOR { I32Xor }
| I32_SHL { I32Shl }
| s = I32_SHR { I32Shr s }
| I32_ROTL { I32Rotl }
| I32_ROTR { I32Rotr }
| I32_EQZ { I32Eqz }
| I32_EQ { I32Eq }
| I32_NE { I32Ne }
| s = I32_LT { I32Lt s }
| s = I32_GT { I32Gt s }
| s = I32_LE { I32Le s }
| s = I32_GE { I32Ge s }
| TUPLE_MAKE l = u32 { TupleMake l }

callindirect(cont):
| CALL_INDIRECT i = idx t = typeuse(cont) { CallIndirect (i, fst t) :: snd t }
| CALL_INDIRECT t = typeuse(cont) { CallIndirect (Num 0l, fst t) :: snd t }
| RETURN_CALL_INDIRECT i = idx t = typeuse(cont)
  { ReturnCallIndirect (i, fst t) :: snd t }
| RETURN_CALL_INDIRECT t = typeuse(cont)
  { ReturnCallIndirect (Num 0l, fst t) :: snd t }

select(cont):
| SELECT "(" RESULT typ = valtype ")" c = cont
  { Select (Some typ) :: c }
| SELECT c = cont
  { Select None :: c }

ambiguous_instr(cont):
| l = callindirect(cont)
| l = select(cont)
{ l }

instr:
| i = plaininstr { i }
| l = ambiguous_instr({[]}) { List.hd l }
| i = blockinstr { i }
| i = foldedinstr { i }

instrs (cont):
| cont { [] }
| i = plaininstr r = instrs(cont) { i :: r }
| l = ambiguous_instr(instrs(cont)) { l }
| i = blockinstr r = instrs(cont) { i :: r }
| i = foldedinstr r = instrs(cont) { i :: r }

foldedinstr:
| "(" i = plaininstr l = foldedinstr * ")"
  { Folded (i, l) }
| "(" l = ambiguous_instr(foldedinstr *) ")"
  { Folded (List.hd l, List.tl l) }
| "(" IF label = label
  btx = blocktype (foldedinstrs("(" THEN l =  instrs(")") {l}))
  else_block = option("(" ELSE l = instrs(")") { l })
  ")"
  { let (typ, (l, if_block)) = btx in
    Folded (If {label; typ; if_block;
                else_block = Option.value ~default:[] else_block },
            l) }
| "(" BLOCK label = label btx = blocktype (instrs(")"))
  { let (typ, block) = btx in
    Folded (Block {label; typ; block}, []) }
| "(" LOOP label = label btx = blocktype (instrs(")"))
  { let (typ, block) = btx in
    Folded (Loop {label; typ; block}, []) }

foldedinstrs(cont):
| c = cont { [], c }
| i = foldedinstr r = foldedinstrs(cont) { map_fst (fun l -> i :: l) r }

expr:
| l = instrs({}) { l }

(* Modules *)

typeuse(cont):
| "(" TYPE i = idx ")" rem = params_and_results(cont)
  { let (s, r) = rem in
    (match s with
     | {params = []; result = []} -> Some i, None
     | _ -> Some i, Some s),
    r }
| rem = params_and_results(cont) { let (s, r) = rem in (None, Some s), r }

import:
| "(" IMPORT module_ = name name = name desc = importdesc ")"
    { Import {module_; name; desc } }

importdesc:
| "(" FUNC i = ID ? t = typeuse(")")
    { Func (i, fst t) }
| "(" GLOBAL i = ID ? t = globaltype ")"
    { (Global (i, t) : importdesc) }
    (* ZZZ *)

func:
| "(" FUNC id = ID ? r = exports(typeuse(locals(instrs(")"))))
  { let (_, (typ, (locals, instrs))) = r in
    Func {id; typ; locals; instrs} }
| "(" FUNC id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = typeuse({}) ")"
  { let (_, (module_, name)) = r in
    Import {module_; name; desc = Func (id, fst t) } }

exports(cont):
| c = cont { [], c }
| "(" EXPORT n = name ")" r = exports(cont)
  { map_fst (fun l -> n :: l) r }

locals(cont):
| c = cont { [], c }
| "(" LOCAL i = ID t = valtype ")" r = locals(cont)
  { map_fst (fun l -> (Some i, t) :: l) r }
| "(" LOCAL l = valtype * ")" r = locals(cont)
  { map_fst ((@) (List.map (fun t -> (None, t)) l)) r }

globaltype:
| typ = valtype { {mut = false; typ} }
| "(" MUT typ = valtype ")" { {mut = true; typ} }

global:
| "(" GLOBAL i = ID ? d = globaldesc ")"
    { d i [] }

globaldesc:
| t = globaltype e = expr
  { fun i _ -> Global (i, t, e) }
| "(" IMPORT module_ = name name = name ")" t = globaltype
  { fun i _ -> Import {module_; name; desc = Global (i, t) } }
| "(" EXPORT n = name ")" d = globaldesc
  { fun i exp -> d i (n :: exp) }

export:
| "(" EXPORT n = name d = exportdesc ")" { Export (n, d) }

exportdesc:
| "(" FUNC i = idx ")" { (Func i : exportdesc) }
| "(" GLOBAL i = idx ")" { (Global i : exportdesc) }

data:
| "(" DATA id = ID ? init = datastring ")"
  { Data { id; init; mode = Passive } }
| "(" DATA id = ID ? m = memuse "(" OFFSET e = expr ")" init = datastring ")"
  { Data { id; init; mode = Active (m, e) } }
| "(" DATA id = ID ? m = memuse "(" instr = instr ")" init = datastring ")"
  { Data { id; init; mode = Active (m, [instr]) } }

datastring:
| l = STRING * { String.concat "" l }

%inline memuse:
| "(" MEMORY i = idx ")" { i }
| { Num 0l }

modulefield:
| t = rectype { t }
| i = import { i }
| g = global { g }
| f = func { f }
| e = export { e }
| d = data { d }

module_:
| "(" MODULE name = ID ? l = modulefield * ")" EOF
  { (name, l) }
| l = modulefield * EOF
  { (None, l) }

(*ZZZ
exports (in globals)
*)
