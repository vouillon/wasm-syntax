(*
ZZZ
- More memory instructions
- Tables
- Segments
*)

%token <string> NAT
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID
%token <Ast.Text.valtype> VALTYPE
%token <Ast.Text.packedtype> PACKEDTYPE
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
%token START
%token ELEM
%token DECLARE
%token ITEM
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
%token CALL
%token CALL_REF
%token CALL_INDIRECT
%token RETURN_CALL
%token RETURN_CALL_REF
%token RETURN_CALL_INDIRECT
%token SELECT
%token LOCAL_GET
%token LOCAL_SET
%token LOCAL_TEE
%token GLOBAL_GET
%token GLOBAL_SET
%token I32STORE8
%token <Ast.Text.signage> I32LOAD8
%token REF_NULL
%token REF_FUNC
%token REF_TEST
%token REF_CAST
%token STRUCT_NEW
%token STRUCT_NEW_DEFAULT
%token <Ast.Text.signage option> STRUCT_GET
%token STRUCT_SET
%token ARRAY_NEW
%token ARRAY_NEW_DEFAULT
%token ARRAY_NEW_FIXED
%token ARRAY_NEW_DATA
%token ARRAY_NEW_ELEM
%token <Ast.Text.signage option> ARRAY_GET
%token ARRAY_SET
%token ARRAY_FILL
%token ARRAY_COPY
%token ARRAY_INIT_DATA
%token ARRAY_INIT_ELEM
%token I32_CONST
%token I64_CONST
%token F32_CONST
%token F64_CONST
%token <Ast.Text.instr> INSTR
%token TAG
%token TRY
%token DO
%token CATCH
%token CATCH_ALL
%token THROW
%token <string> MEM_ALIGN
%token <string> MEM_OFFSET
(* Binaryen extensions *)
%token POP
%token TUPLE
%token TUPLE_MAKE
%token TUPLE_EXTRACT

%{
 (* To avoid references to module Wasm in the generated mli file *)
module Wasm = struct end

open Ast.Text

let map_fst f (x, y) = (f x, y)

let remove_bindings (id, sign) =
  (id,
   Option.map
     (fun (params, results) ->
        { params = Array.of_list (List.map snd params);
          results = Array.of_list results } )
     sign)
%}

%start <string option * Ast.Text.modulefield list> parse

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

name: s = STRING
  { if not (String.is_valid_utf_8 s) then
      raise
        (Parsing.Syntax_error
           ( $sloc,
             Printf.sprintf "Malformed name \"%s\".\n" s));
    s }

(* Types *)

heaptype:
| ANY { Any }
| EQ { Eq }
| I31 { I31 }
| STRUCT { Struct }
| ARRAY { Array }
| NONE { None_ }
| FUNC { Func }
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
| FUNCREF { {nullable = true; typ = Func} }
| NULLFUNCREF { {nullable = true; typ = NoFunc} }
| EXTERNREF { {nullable = true; typ = Extern} }
| NULLEXTERNREF { {nullable = true; typ = NoExtern} }

tupletype:
| "(" TUPLE l = list(valtype) ")" { l }

valtype:
| t = VALTYPE { t }
| t = reftype { Ref t }
| t = tupletype { Tuple t }

functype:
| "(" FUNC r = params_and_results_no_bindings(")")
  { fst r }

params(cont):
| c = cont { ([] , c) }
| "(" PARAM i = ID t = valtype ")" rem = params(cont)
  { map_fst (fun l -> (Some i, t) :: l) rem }
| "(" PARAM l = valtype * ")" rem = params(cont)
  { map_fst ((@) (List.map (fun t -> (None, t)) l)) rem }

params_no_bindings(cont):
| c = cont { [], c }
| "(" PARAM l = valtype * ")" rem = params_no_bindings(cont)
  { map_fst ((@) l) rem }

results(cont):
| c = cont { [], c }
| "(" RESULT l = valtype * ")" rem = results(cont)
  { map_fst ((@) l) rem }

params_and_results(cont):
| r = params(results(cont))
  { let (p, (r, c)) = r in ((p, r), c) }

params_and_results_no_bindings(cont):
| r = params_no_bindings(results(cont))
  { let (p, (r, c)) = r in
    ({params = Array.of_list p; results = Array.of_list r }, c) }

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
| "(" STRUCT l = field * ")" { Struct (Array.of_list (List.flatten l)) }
| t = functype { Func t }

rectype:
| "(" REC l = typedef * ")" { Types (Array.of_list l) }
| t = typedef { Types [|t|] }

typedef:
| "(" TYPE name = ID ? t = subtype ")" { (name, t) }

subtype:
| "(" SUB final = boption(FINAL) supertype = idx ? typ = comptype
  ")"
  { {final; supertype; typ} }
| typ = comptype { {final = true; supertype = None; typ } }

limits:
| mi = u32 { {mi; ma = None} }
| mi = u32 ma = u32 { {mi; ma = Some ma} }

memtype:
| l = limits { l }

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
| TRY label = label bti = blocktype(instrs({})) c = catches END
  { let (typ, block) = bti in
  let (catches, catch_all) = c in
  Try {label; typ; block; catches; catch_all} }

catches:
| END { [], None }
| CATCH_ALL l = instrs(END) { [], Some l }
| CATCH i = idx l = instrs({}) rem = catches
  { map_fst (fun r -> (i, l) :: r) rem }

label:
| i = ID ? { i }

blocktype(cont):
| r = typeuse_no_bindings(cont)
  { map_fst (fun t -> Option.map (fun v -> Idx v) (fst t)) r }
  (*ZZZ single result / implicit? validate?*)

plaininstr:
| THROW i = idx { Throw i }
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
| CALL i = idx { Call i }
| CALL_REF i = idx { CallRef i }
| RETURN_CALL i = idx { ReturnCall i }
| RETURN_CALL_REF i = idx { ReturnCallRef i }
| POP t = valtype { Pop t }
| LOCAL_GET i = idx { LocalGet i }
| LOCAL_SET i = idx { LocalSet i }
| LOCAL_TEE i = idx { LocalTee i }
| GLOBAL_GET i = idx { GlobalGet i }
| GLOBAL_SET i = idx { GlobalSet i }
| s = I32LOAD8 m = memarg { I32Load8 (s, m 1l) }
| I32STORE8 m = memarg { I32Store8 (m 1l) }
| REF_NULL t = heaptype { RefNull t }
| REF_FUNC i = idx { RefFunc i }
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
| ARRAY_FILL i = idx { ArrayFill i }
| ARRAY_COPY i1 = idx i2 = idx { ArrayCopy (i1, i2) }
| ARRAY_INIT_DATA i1 = idx i2 = idx { ArrayInitData (i1, i2) }
| ARRAY_INIT_ELEM i1 = idx i2 = idx { ArrayInitElem (i1, i2) }
| I32_CONST i = i32 { Const (I32 i) }
| I64_CONST i = i64 { Const (I64 i) }
| F32_CONST f = f32 { Const (F64 f) }
| F64_CONST f = f64 { Const (F64 f) }
| TUPLE_MAKE l = u32 { TupleMake l }
| TUPLE_EXTRACT l = u32 i = u32 { TupleExtract (l, i) }
| i = INSTR { i }

memarg:
| o = option(MEM_OFFSET) a = option(MEM_ALIGN)
  { fun width ->
    (* ZZZ overflows *)
    {offset = Option.value ~default:0l (Option.map Int32.of_string o);
     align = Option.value ~default:width  (Option.map Int32.of_string a)} }

callindirect(cont):
| CALL_INDIRECT i = idx t = typeuse_no_bindings(cont)
  { CallIndirect (i, fst t) :: snd t }
| CALL_INDIRECT t = typeuse_no_bindings(cont)
  { CallIndirect (Num 0l, fst t) :: snd t }
| RETURN_CALL_INDIRECT i = idx t = typeuse_no_bindings(cont)
  { ReturnCallIndirect (i, fst t) :: snd t }
| RETURN_CALL_INDIRECT t = typeuse_no_bindings(cont)
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
| "(" BLOCK label = label btx = blocktype (instrs(")"))
  { let (typ, block) = btx in
    Folded (Block {label; typ; block}, []) }
| "(" LOOP label = label btx = blocktype (instrs(")"))
  { let (typ, block) = btx in
    Folded (Loop {label; typ; block}, []) }
| "(" IF label = label
  btx = blocktype (foldedinstrs("(" THEN l =  instrs(")") {l}))
  else_block = option("(" ELSE l = instrs(")") { l })
  ")"
  { let (typ, (l, if_block)) = btx in
    Folded (If {label; typ; if_block;
                else_block = Option.value ~default:[] else_block },
            l) }
| "(" TRY label = label
  btb = blocktype("(" DO l = instrs(")") { l })
  c = foldedcatches ")"
{ let (typ, block) = btb in
  let (catches, catch_all) = c in
  Folded (Try {label; typ; block; catches; catch_all}, []) }

foldedcatches:
| { [], None }
| "(" CATCH_ALL l = instrs(")") { [], Some l }
| "(" CATCH i = idx l = instrs(")") rem = foldedcatches
  { map_fst (fun r -> (i, l) :: r) rem }

foldedinstrs(cont):
| c = cont { [], c }
| i = foldedinstr r = foldedinstrs(cont) { map_fst (fun l -> i :: l) r }

expr:
| l = instrs({}) { l }

(* Modules *)

typeuse(cont):
| "(" TYPE i = idx ")" rem = params_and_results(cont)
  { let ((params, results) as s, r) = rem in
    (match params, results with
     | [], [] -> Some i, None
     | _ -> Some i, Some s),
    r }
| rem = params_and_results(cont) { let (s, r) = rem in (None, Some s), r }

typeuse_no_bindings(cont):
| "(" TYPE i = idx ")" rem = params_and_results_no_bindings(cont)
  { let (s, r) = rem in
    (match s with
     | {params = [||]; results = [||]} -> Some i, None
     | _ -> Some i, Some s),
    r }
| rem = params_and_results_no_bindings(cont)
  { let (s, r) = rem in (None, Some s), r }

import:
| "(" IMPORT module_ = name name = name desc = importdesc ")"
    { let (id, desc) = desc in Import {module_; name; id; desc; exports = [] } }

importdesc:
| "(" FUNC i = ID ? t = typeuse(")")
    { (i, Func (remove_bindings (fst t))) }
| "(" MEMORY i = ID ? l = limits ")"
    { (i, (Memory l)) }
| "(" GLOBAL i = ID ? t = globaltype ")"
    { (i, (Global t)) }
| "(" TAG i = ID ? t = typeuse(")")
    { (i, (Tag (remove_bindings (fst t)) : importdesc)) }

func:
| "(" FUNC id = ID ? r = exports(typeuse(locals(instrs(")"))))
  { let (exports, (typ, (locals, instrs))) = r in
    Func {id; typ; locals; instrs; exports} }
| "(" FUNC id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = typeuse({}) ")"
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Func (remove_bindings (fst t)); exports } }

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

memory:
| "(" MEMORY id = ID? r = exports(memtype) ")"
  { let (exports, limits) = r in Memory {id; limits; init = None; exports} }
| "(" MEMORY id = ID? r = exports("(" DATA s = datastring ")" { s }) ")"
  { let (exports, s) = r in
    let sz = Int32.of_int ((String.length s + 65535) lsr 16) in
    Memory {id; limits = {mi = sz; ma = Some sz}; init = Some s; exports} }
| "(" MEMORY id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = memtype ")"
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Memory t; exports } }

tag:
| "(" TAG id = ID ? r = exports(typeuse (")"))
    { let (exports, (typ, _)) = r in
      Tag {id; typ = remove_bindings typ; exports} }
| "(" TAG id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = typeuse (")")
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Tag (remove_bindings (fst t)); exports } }

global:
| "(" GLOBAL id = ID ? r = exports(t = globaltype init = expr { t, init }) ")"
  { let (exports, (typ, init)) = r in Global {id; typ; init; exports} }
| "(" GLOBAL id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  typ = globaltype ")"
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Global typ; exports } }

globaltype:
| typ = valtype { {mut = false; typ} }
| "(" MUT typ = valtype ")" { {mut = true; typ} }

export:
| "(" EXPORT name = name d = exportdesc ")"
  { let (index, kind) = d in Export {name; kind; index} }

exportdesc:
| "(" FUNC i = idx ")" { (i, Func) }
| "(" GLOBAL i = idx ")" { (i, Global) }
| "(" MEMORY i = idx ")" { (i, Memory) }
| "(" TAG i = idx ")" { (i, (Tag : exportable)) }

start:
| "(" START i = idx ")" { Start i }

elem:
| "(" ELEM id = ID ? DECLARE l = elemlist ")"
  { let (typ, init) = l in Elem {id; typ; init} }

elemlist:
| t = reftype l = elemexpr * { (t, l) }
| FUNC l = idx *
  { ({nullable = false; typ = Func}, List.map (fun i -> [RefFunc i]) l) }

elemexpr:
| "(" ITEM  e = expr ")" {e}

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
| f = rectype
| f = import
| f = func
| f = tag
| f = memory
| f = global
| f = export
| f = start
| f = elem
| f = data
  { f }

parse:
| "(" MODULE name = ID ? l = modulefield * ")" EOF
  { (name, l) }
| l = modulefield * EOF
  { (None, l) }
