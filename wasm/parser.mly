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
%token EXN
%token NOEXN
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
%token EXNREF
%token NULLEXNREF
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
%token TABLE
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
%token <(unit, unit, unit, unit) Ast.Text.op> STORE
%token <[`I32|`I64] * [`I8 | `I16 | `I32]> STORES
%token <(unit, unit, unit, unit) Ast.Text.op> LOAD
%token <[`I32|`I64] * [`I8 | `I16 | `I32] * Ast.Text.signage> LOADS
%token MEMORY_SIZE
%token MEMORY_GROW
%token MEMORY_FILL
%token MEMORY_COPY
%token MEMORY_INIT
%token DATA_DROP
%token TABLE_GET
%token TABLE_SET
%token TABLE_SIZE
%token TABLE_GROW
%token TABLE_FILL
%token TABLE_COPY
%token TABLE_INIT
%token ELEM_DROP
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
%token <Ast.location Ast.Text.instr_desc> INSTR
%token TAG
%token TRY
%token TRY_TABLE
%token DO
%token CATCH
%token CATCH_REF
%token CATCH_ALL
%token CATCH_ALL_REF
%token THROW
%token <string> MEM_ALIGN
%token <string> MEM_OFFSET
(* Binaryen extensions *)
%token POP
%token TUPLE
%token TUPLE_MAKE
%token TUPLE_EXTRACT

%token DEFINITION
%token BINARY
%token QUOTE
%token INSTANCE
%token REGISTER
%token INVOKE
%token GET
%token NAN
%token V128
%token REF_HOST
%token I8X16
%token I16X8
%token I32X4
%token I64X2
%token F32X4
%token F64X2
%token ASSERT_RETURN
%token ASSERT_RETURN_NAN
%token ASSERT_EXCEPTION
%token ASSERT_TRAP
%token ASSERT_EXHAUSTION
%token ASSERT_MALFORMED
%token ASSERT_INVALID
%token ASSERT_UNLINKABLE
%token V128_CONST
%token REF_EXTERN
%token REF_STRUCT
%token REF_ARRAY
%token EITHER
%token SCRIPT
%token INPUT
%token OUTPUT

%{
 (* To avoid references to module Wasm in the generated mli file *)
module Wasm = struct end

open Ast.Text

let with_loc (loc_start, loc_end) desc =
  {Ast.desc; info = { Ast.loc_start; loc_end }}

let map_fst f (x, y) = (f x, y)
%}

%start <string option * Ast.location modulefield list> parse
%start <([`Valid | `Invalid | `Malformed ] *
         [`Parsed of string option * Ast.location modulefield list
         | `Text of string ]) list> parse_script

%%

u32: n = NAT { Uint32.of_string n }

u64: n = NAT { Uint64.of_string n }

i32:
  n = NAT { n }
| i = INT { i }

i64:
  n = NAT { n }
| i = INT { i }

f32:
  n = NAT { n }
| i = INT { i }
| f = FLOAT { f }

f64:
  n = NAT { n }
| i = INT { i }
| f = FLOAT { f }

idx:
| n = u32 { with_loc $sloc (Num n) }
| i = ID { with_loc $sloc (Id i) }

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
| EXN { Exn }
| NOEXN { NoExn }
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
| EXNREF { {nullable = true; typ = Exn} }
| NULLEXNREF { {nullable = true; typ = NoExn} }
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
| mi = u64 { {mi; ma = None} }
| mi = u64 ma = u64 { {mi; ma = Some ma} }

memtype:
| l = limits { l }

tabletype(cont):
| l = limits t = reftype c = cont { ({limits = l; reftype = t}, c) }

(* Instructions *)

blockinstr:
| BLOCK label = label bti = blocktype(instrs(END)) label
  { let (typ, block) = bti in with_loc $sloc (Block {label; typ; block}) }
| LOOP label = label bti = blocktype(instrs(END)) label
  { let (typ, block) = bti in with_loc $sloc (Loop {label; typ; block}) }
| IF label = label bti = blocktype(instrs(ELSE))
  label else_block = instrs(END)
  label (*ZZZ labels must match *)
  { let (typ, if_block) = bti in
    with_loc $sloc (If {label; typ; if_block; else_block }) }
| IF label = label bti = blocktype(instrs(END))
  label (*ZZZ labels must match *)
  { let (typ, if_block) = bti in
    with_loc $sloc (If {label; typ; if_block; else_block = [] }) }
| TRY_TABLE label = label bti = blocktype(tbl_catches(instrs({})))
  END label
   { let (typ, (catches, block)) = bti in
     with_loc $sloc (TryTable {label; typ; catches; block}) }
| TRY label = label bti = blocktype(instrs({})) c = catches END label
  { let (typ, block) = bti in
    let (catches, catch_all) = c in
    with_loc $sloc (Try {label; typ; block; catches; catch_all}) }

tbl_catches(cont):
| c = cont { [], c }
| "(" CATCH x = idx l = idx ")" c = tbl_catches(cont)
  { map_fst (fun r -> Catch(x, l) :: r) c }
| "(" CATCH_REF x = idx l = idx ")" c = tbl_catches(cont)
  { map_fst (fun r -> CatchRef(x, l) :: r) c }
| "(" CATCH_ALL l = idx ")" c = tbl_catches(cont)
  { map_fst (fun r -> CatchAll l :: r) c }
| "(" CATCH_ALL_REF l = idx ")" c = tbl_catches(cont)
  { map_fst (fun r -> CatchAllRef l :: r) c }

catches:
| END { [], None }
| CATCH_ALL l = instrs(END) { [], Some l }
| CATCH i = idx l = instrs({}) rem = catches
  { map_fst (fun r -> (i, l) :: r) rem }

label:
| i = ID ? { i }

blocktype(cont):
| r = typeuse_no_bindings(cont)
  { (match fst r with
     | None, Some {params = [||]; results = [|typ|]} -> Some (Valtype typ)
     | None, None -> None
     | _ -> Some (Typeuse (fst r))),
    snd r }

%inline memidx:
| i = ioption(idx) { Option.value ~default:(with_loc $sloc (Num Uint32.zero)) i}

%inline tableidx:
| i = ioption(idx) { Option.value ~default:(with_loc $sloc (Num Uint32.zero)) i}

plaininstr:
| THROW i = idx { with_loc $sloc (Throw i) }
| BR i = idx { with_loc $sloc (Br i) }
| BR_IF i = idx { with_loc $sloc (Br_if i) }
| BR_TABLE l = idx +
  { let l = List.rev l in
    with_loc $sloc (Br_table (List.rev (List.tl l), List.hd l)) }
| BR_ON_NULL i = idx { with_loc $sloc (Br_on_null i) }
| BR_ON_NON_NULL i = idx { with_loc $sloc (Br_on_non_null i) }
| BR_ON_CAST i = idx t1 = reftype t2 = reftype
  { with_loc $sloc (Br_on_cast (i, t1, t2)) }
| BR_ON_CAST_FAIL i = idx t1 = reftype t2 = reftype
  { with_loc $sloc (Br_on_cast_fail (i, t1, t2)) }
| CALL i = idx { with_loc $sloc (Call i) }
| CALL_REF i = idx { with_loc $sloc (CallRef i) }
| RETURN_CALL i = idx { with_loc $sloc (ReturnCall i) }
| RETURN_CALL_REF i = idx { with_loc $sloc (ReturnCallRef i) }
| POP t = valtype { with_loc $sloc (Pop t) }
| LOCAL_GET i = idx { with_loc $sloc (LocalGet i) }
| LOCAL_SET i = idx { with_loc $sloc (LocalSet i) }
| LOCAL_TEE i = idx { with_loc $sloc (LocalTee i) }
| GLOBAL_GET i = idx { with_loc $sloc (GlobalGet i) }
| GLOBAL_SET i = idx { with_loc $sloc (GlobalSet i) }
| sz = LOAD i = memidx m = memarg
  { with_loc $sloc (Load (i, m (Uint64.one), sz)) }
| k = LOADS i = memidx m = memarg
  { let (sz, sz', s) = k in
    with_loc $sloc (LoadS (i, m Uint64.one, sz, sz', s)) }
| sz = STORE i = memidx m = memarg
  { with_loc $sloc (Store (i, m Uint64.one, sz)) }
| sz = STORES i = memidx m = memarg
  { with_loc $sloc (StoreS (i, m Uint64.one, fst sz, snd sz)) }
| MEMORY_SIZE i = memidx { with_loc $sloc (MemorySize i) }
| MEMORY_GROW i = memidx { with_loc $sloc (MemoryGrow i) }
| MEMORY_FILL i = memidx { with_loc $sloc (MemoryFill i) }
| MEMORY_COPY p = option(i1 = idx i2 = idx { (i1, i2) })
  { let zero = with_loc $loc(p) (Num Uint32.zero) in
    let (i, i') = Option.value ~default:(zero, zero) p in
    with_loc $sloc (MemoryCopy (i, i')) }
| MEMORY_INIT i = memidx d = idx { with_loc $sloc (MemoryInit (i, d)) }
| DATA_DROP d = idx { with_loc $sloc (DataDrop d) }
| TABLE_GET i = tableidx { with_loc $sloc (TableGet i) }
| TABLE_SET i = tableidx { with_loc $sloc (TableSet i) }
| TABLE_SIZE i = tableidx { with_loc $sloc (TableSize i) }
| TABLE_GROW i = tableidx { with_loc $sloc (TableGrow i) }
| TABLE_FILL i = tableidx { with_loc $sloc (TableFill i) }
| TABLE_COPY p = option(i1 = idx i2 = idx { (i1, i2) })
  { let zero = with_loc $loc(p) (Num Uint32.zero) in
    let (i, i') = Option.value ~default:(zero, zero) p in
    with_loc $sloc (TableCopy (i, i')) }
| TABLE_INIT i = tableidx d = idx { with_loc $sloc (TableInit (i, d)) }
| ELEM_DROP e = idx { with_loc $sloc (ElemDrop e) }
| REF_NULL t = heaptype { with_loc $sloc (RefNull t) }
| REF_FUNC i = idx { with_loc $sloc (RefFunc i) }
| REF_TEST t = reftype { with_loc $sloc (RefTest t) }
| REF_CAST t = reftype { with_loc $sloc (RefCast t) }
| STRUCT_NEW i = idx { with_loc $sloc (StructNew i) }
| STRUCT_NEW_DEFAULT i = idx { with_loc $sloc (StructNewDefault i) }
| s = STRUCT_GET i1 = idx i2 = idx { with_loc $sloc (StructGet (s, i1, i2)) }
| STRUCT_SET i1 = idx i2 = idx { with_loc $sloc (StructSet (i1, i2)) }
| ARRAY_NEW i = idx { with_loc $sloc (ArrayNew i) }
| ARRAY_NEW_DEFAULT i = idx { with_loc $sloc (ArrayNewDefault i) }
| ARRAY_NEW_FIXED i = idx l = u32
  { with_loc $sloc (ArrayNewFixed (i, l)) }
| ARRAY_NEW_DATA i1 = idx i2 = idx { with_loc $sloc (ArrayNewData (i1, i2)) }
| ARRAY_NEW_ELEM i1 = idx i2 = idx { with_loc $sloc (ArrayNewElem (i1, i2)) }
| s = ARRAY_GET i = idx { with_loc $sloc (ArrayGet (s, i)) }
| ARRAY_SET i = idx { with_loc $sloc (ArraySet i) }
| ARRAY_FILL i = idx { with_loc $sloc (ArrayFill i) }
| ARRAY_COPY i1 = idx i2 = idx { with_loc $sloc (ArrayCopy (i1, i2)) }
| ARRAY_INIT_DATA i1 = idx i2 = idx { with_loc $sloc (ArrayInitData (i1, i2)) }
| ARRAY_INIT_ELEM i1 = idx i2 = idx { with_loc $sloc (ArrayInitElem (i1, i2)) }
| I32_CONST i = i32 { with_loc $sloc (Const (I32 i)) }
| I64_CONST i = i64 { with_loc $sloc (Const (I64 i)) }
| F32_CONST f = f32 { with_loc $sloc (Const (F64 f)) }
| F64_CONST f = f64 { with_loc $sloc (Const (F64 f)) }
| TUPLE_MAKE l = u32 { with_loc $sloc (TupleMake l) }
| TUPLE_EXTRACT l = u32 i = u32
  { with_loc $sloc (TupleExtract (l, i)) }
| i = INSTR { with_loc $sloc i }

memarg:
| o = option(MEM_OFFSET) a = option(MEM_ALIGN)
  { fun width ->
    {offset = Option.value ~default:Uint64.zero (Option.map Uint64.of_string o);
     align = Option.value ~default:width (Option.map Uint64.of_string a)} }

callindirect(cont):
| CALL_INDIRECT i = idx t = typeuse_no_bindings(cont)
  { with_loc ($symbolstartpos, $endpos(i)) (CallIndirect (i, fst t)) :: snd t }
| CALL_INDIRECT t = typeuse_no_bindings(cont)
  { with_loc $loc($1)
      (CallIndirect (Ast.no_loc (Num Uint32.zero), fst t)) :: snd t }
| RETURN_CALL_INDIRECT i = idx t = typeuse_no_bindings(cont)
  { with_loc ($symbolstartpos, $endpos(i))
      (ReturnCallIndirect (i, fst t)) :: snd t }
| RETURN_CALL_INDIRECT t = typeuse_no_bindings(cont)
  { with_loc $loc($1)
      (ReturnCallIndirect (Ast.no_loc (Num Uint32.zero), fst t)) :: snd t }

select_results(cont):
| c = cont { (([], None), c) }
| "(" RESULT l = valtype * ")" rem = select_results(cont)
  { map_fst
      (fun (l', loc) ->
         (l :: l', Some (Option.value ~default:$endpos($4) loc)))
      rem }

select(cont):
| SELECT p = select_results(cont)
  { let ((l, loc), c) = p in
    with_loc ($symbolstartpos, Option.value ~default:$endpos($1) loc)
      (Select (if l = [] then None else Some (List.concat l))) :: c }

ambiguous_instr(cont):
| l = callindirect(cont)
| l = select(cont)
{ l }

instrs (cont):
| cont { [] }
| i = plaininstr r = instrs(cont) { i :: r }
| l = ambiguous_instr(instrs(cont)) { l }
| i = blockinstr r = instrs(cont) { i :: r }
| i = foldedinstr r = instrs(cont) { i :: r }

foldedinstr:
| "(" i = plaininstr l = foldedinstr * ")"
  { with_loc $sloc (Folded (i, l)) }
| "(" l = ambiguous_instr(foldedinstr *) ")"
  { with_loc $sloc (Folded (List.hd l, List.tl l)) }
| "(" BLOCK label = label btx = blocktype (instrs(")"))
  { let (typ, block) = btx in
    with_loc $sloc (Folded (with_loc $sloc (Block {label; typ; block}), [])) }
| "(" LOOP label = label btx = blocktype (instrs(")"))
  { let (typ, block) = btx in
    with_loc $sloc (Folded (with_loc $sloc (Loop {label; typ; block}), [])) }
| "(" IF label = label
  btx = blocktype (foldedinstrs("(" THEN l =  instrs(")") {l}))
  else_block = option("(" ELSE l = instrs(")") { l })
  ")"
  { let (typ, (l, if_block)) = btx in
    with_loc $sloc
      (Folded
        (with_loc $sloc
          (If {label; typ; if_block;
               else_block = Option.value ~default:[] else_block }),
         l)) }
| "(" TRY_TABLE label = label bti = blocktype(tbl_catches(instrs(")")))
   { let (typ, (catches, block)) = bti in
     with_loc $sloc
       (Folded
          (with_loc $sloc (TryTable {label; typ; catches; block}),
          [])) }
| "(" TRY label = label
  btb = blocktype("(" DO l = instrs(")") { l })
  c = foldedcatches ")"
  { let (typ, block) = btb in
    let (catches, catch_all) = c in
    with_loc $sloc
      (Folded
        (with_loc $sloc (Try {label; typ; block; catches; catch_all}), [])) }

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
    { (i, Func (fst t)) }
| "(" MEMORY i = ID ? l = limits ")"
    { (i, (Memory l)) }
| "(" TABLE i = ID ? t = tabletype({}) ")"
    { (i, (Table (fst t))) }
| "(" GLOBAL i = ID ? t = globaltype ")"
    { (i, (Global t)) }
| "(" TAG i = ID ? t = typeuse(")")
    { (i, (Tag (fst t) : importdesc)) }

func:
| "(" FUNC id = ID ? r = exports(typeuse(locals(instrs(")"))))
  { let (exports, (typ, (locals, instrs))) = r in
    Func {id; typ; locals; instrs; exports} }
| "(" FUNC id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = typeuse({}) ")"
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Func (fst t); exports } }

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
    let sz = Uint64.of_int ((String.length s + 65535) lsr 16) in
    Memory {id; limits = {mi = sz; ma = Some sz}; init = Some s; exports} }
| "(" MEMORY id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = memtype ")"
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Memory t; exports } }

table:
| "(" TABLE id = ID? r = exports(tabletype(expr)) ")"
   { let (exports, (typ, e)) = r in
     Table {id; typ; init = Init_expr e; exports} }
| "(" TABLE id = ID?
  r = exports(t = reftype "(" ELEM e = list(elemexpr) ")" {t, e}) ")"
   { let (exports, (reftype, elem)) = r in
     let len = Uint64.of_int (List.length elem) in
     Table {id; typ = {limits ={mi=len; ma =Some len}; reftype};
            init = Init_segment elem; exports} }
| "(" TABLE id = ID?
  r = exports(t = reftype "(" ELEM e = nonempty_list(idx) ")" {t, e}) ")"
   { let (exports, (reftype, elem)) = r in
     let len = Uint64.of_int (List.length elem) in
     let elem = List.map (fun i -> [{i with Ast.desc = RefFunc i}]) elem in
     Table {id; typ = {limits ={mi=len; ma =Some len}; reftype};
            init = Init_segment elem; exports} }
| "(" TABLE id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = tabletype({}) ")"
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Table (fst t); exports } }

tag:
| "(" TAG id = ID ? r = exports(typeuse (")"))
    { let (exports, (typ, _)) = r in
      Tag {id; typ; exports} }
| "(" TAG id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = typeuse (")")
  { let (exports, (module_, name)) = r in
    Import {module_; name; id; desc = Tag (fst t); exports } }

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
| "(" TABLE i = idx ")" { (i, Table) }
| "(" TAG i = idx ")" { (i, (Tag : exportable)) }

start:
| "(" START i = idx ")" { Start i }

elem:
| "(" ELEM id = ID ? l = elemlist ")"
  { let (typ, init) = l in Elem {id; mode = Passive; typ; init} }
| "(" ELEM id = ID ? t = tableuse o = offset l = elemlist ")"
  { let (typ, init) = l in Elem {id; mode = Active (t, o); typ; init} }
| "(" ELEM id = ID ? DECLARE l = elemlist ")"
  { let (typ, init) = l in Elem {id; mode = Declare; typ; init} }
| "(" ELEM id = ID ? o = offset init = list(idx) ")"
  { let init = List.map (fun i -> [{i with Ast.desc = RefFunc i}]) init in
    Elem {id; mode = Active (Ast.no_loc (Num Uint32.zero), o);
          typ = { nullable =true ; typ = Func }; init} }

elemlist:
| t = reftype l = elemexpr * { (t, l) }
| FUNC l = list(idx)
  { let l = List.map (fun i -> [{i with Ast.desc = RefFunc i}]) l in
    ({nullable = false; typ = Func}, l) }

elemexpr:
| "(" ITEM  e = expr ")" { e }
| instr = foldedinstr { [instr] }

%inline tableuse:
| "(" TABLE i = idx ")" { i }
| { with_loc $sloc (Num Uint32.zero) }

data:
| "(" DATA id = ID ? init = datastring ")"
  { Data { id; init; mode = Passive } }
| "(" DATA id = ID ? m = memuse e = offset init = datastring ")"
  { Data { id; init; mode = Active (m, e) } }

offset:
| "(" OFFSET e = expr ")" { e }
| instr = foldedinstr { [instr] }

datastring:
| l = STRING * { String.concat "" l }

%inline memuse:
| "(" MEMORY i = idx ")" { i }
| { with_loc $sloc (Num Uint32.zero) }

modulefield:
| f = rectype
| f = import
| f = func
| f = tag
| f = memory
| f = table
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

parse_script:
| s = script EOF { s }
| inline_module EOF { [] }

script:
| c = cmd* { List.concat c }

inline_module:
| l = modulefield + { [(`Valid, `Parsed (None, l))] }

cmd:
| m = module_ { m `Valid }
| instance { [] }
| register { [] }
| action { [] }
| c = assertion { c }
| c = meta { c }


module_:
| "(" MODULE DEFINITION ? name = ID ? l = modulefield * ")"
  { fun status -> [(status, `Parsed (name, l))] }
| "(" MODULE DEFINITION ? ID ? BINARY STRING *  ")" { fun _ -> [] }
| "(" MODULE DEFINITION ? ID ? QUOTE s = STRING *  ")"
  { fun status -> [(status, `Text (String.concat "" s))] }

script_instance:
| instance { fun _ -> [] }
| m = module_ { m }

instance:
| "(" MODULE INSTANCE option(ID ID ? {}) ")" { }

register:
| "(" REGISTER STRING ID ? ")" {}

action:
| "(" INVOKE ID ? STRING const * ")"
| "(" GET ID? STRING ")"
{}

const:
| "(" I32_CONST i32 ")"
| "(" I64_CONST i64 ")"
| "(" F32_CONST f32 ")"
| "(" F64_CONST f64 ")"
| "(" V128 vec_shape NAT+ ")"
| "(" REF_NULL heaptype ")"
| "(" REF_HOST NAT ")"
| "(" REF_EXTERN NAT ")"
{}

vec_shape:
| I8X16
| I16X8
| I32X4
| I64X2
| F32X4
| F64X2
| V128
{}

assertion:
| "(" ASSERT_RETURN action result_pat* ")" { [] }
| "(" ASSERT_RETURN_NAN action ")" { [] }
| "(" ASSERT_EXCEPTION action ")" { [] }
| "(" ASSERT_TRAP action STRING ")" { [] }
| "(" ASSERT_EXHAUSTION action STRING ")" { [] }
| "(" ASSERT_MALFORMED m = module_ STRING ")" { m `Malformed }
| "(" ASSERT_INVALID m = module_ STRING ")" { m `Invalid }
| "(" ASSERT_UNLINKABLE m = script_instance STRING ")" { m `Valid }
| "(" ASSERT_TRAP m = script_instance STRING ")" { m `Valid }

result_pat:
| "(" I32_CONST i32 ")"
| "(" I64_CONST i64 ")"
| "(" F32_CONST f32 ")"
| "(" F32_CONST NAN ")"
| "(" F64_CONST f64 ")"
| "(" F64_CONST NAN ")"
| "(" V128_CONST vec_shape NAT+ ")"
| "(" REF ")"
| "(" REF_NULL ")"
| "(" REF_FUNC ")"
| "(" REF_EXTERN ")"
| "(" REF_STRUCT ")"
| "(" REF_ARRAY ")"
| "(" REF_NULL heaptype ")"
| "(" REF_HOST NAT ")"
| "(" REF_EXTERN NAT ")"
| "(" INSTR (*RefI31*) ")"
| "(" EITHER result_pat+ ")"
{}

meta:
| "(" SCRIPT ID? s = script ")" { s }
| "(" INPUT ID? STRING ")" { [] }
| "(" OUTPUT ID? STRING? ")" { [] }
