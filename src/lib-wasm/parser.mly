(*
ZZZ
- More memory instructions
- Tables
- Segments
*)

%token <string> NAT
%token <string> INT
%token <string> FLOAT
%token <(string, Ast.location) Ast.annotated> STRING
%token <(string, Ast.location) Ast.annotated> ID
%token I32 I64 F32 F64
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
%token <Ast.Text.num_type> STORE
%token <[`I32|`I64] * [`I8 | `I16 | `I32]> STORES
%token <Ast.Text.num_type> LOAD
%token <Ast.Text.vec_load_op> VEC_LOAD
%token VEC_STORE
%token <Ast.vec_shift_op> VEC_SHIFT_OP
%token <Ast.vec_bitmask_op> VEC_BITMASK_OP
%token <Ast.vec_tern_op> VEC_TERN_OP
%token VEC_BITSELECT
%token <Ast.vec_shape * Ast.signage option> VEC_EXTRACT
%token <Ast.vec_shape> VEC_REPLACE
%token <[ `I8 | `I16 | `I32 | `I64 ]> VEC_LOAD_LANE VEC_STORE_LANE VEC_LOAD_SPLAT
%token VEC_SHUFFLE
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
%token THROW_REF
%token <string> MEM_ALIGN
%token <string> MEM_OFFSET
(* Binaryen extensions *)
%token POP
%token TUPLE
%token TUPLE_MAKE
%token TUPLE_EXTRACT
%token STRING_ANNOT CHAR_ANNOT

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

%parameter <Context : sig type t = Utils.Trivia.context val context : t end>

%{
module Uint32 = Utils.Uint32
module Uint64 = Utils.Uint64
module V128 = Utils.V128
module Wasm = struct
end
open Ast.Text

let lane_width : [ `I8 | `I16 | `I32 | `I64 ] -> Uint64.t = function
  | `I8 -> Uint64.of_int 1
  | `I16 -> Uint64.of_int 2
  | `I32 -> Uint64.of_int 4
  | `I64 -> Uint64.of_int 8

let with_loc loc desc =
  Utils.Trivia.with_pos Context.context {loc_start = fst loc; loc_end = snd loc} desc

let map_fst f (x, y) = (f x, y)

let check_constant f loc s =
  if not (f s) then
    raise
      (Parsing.Syntax_error
         ( loc,
           Printf.sprintf "Constant %s is out of range.\n" s))

let check_labels lab (lab' : Ast.Text.name option) =
  match lab' with
  | None -> ()
  | Some lab' ->
      let mismatch =
        match lab with
        | Some lab -> lab.Ast.desc <> lab'.desc
        | None -> true
      in
      if mismatch then
        raise
          (Parsing.Syntax_error
             ( (lab'.info.loc_start, lab'.info.loc_end),
               Printf.sprintf "Label mismatch.\n"))


%}

%start <Ast.location Ast.Text.module_> parse
%start <([`Valid | `Invalid of string | `Malformed of string ] *
         [`Parsed of Ast.location Ast.Text.module_
         | `Text of string | `Binary of string ]) list> parse_script

 (* To avoid unused token report and refer to Context in the mli *)
%start <Context.t> dummy_ctx

%%

dummy_ctx: EOF { assert false }

u32: n = NAT { Uint32.of_string n }

u64: n = NAT { Uint64.of_string n }

u8: n = NAT { check_constant Misc.is_int8 $sloc n; n }

i8:
  i = NAT
| i = INT
{ check_constant Misc.is_int8 $sloc i; i }

i16:
  i = NAT
| i = INT
{ check_constant Misc.is_int16 $sloc i; i }

i32:
  i = NAT
| i = INT
{ check_constant Misc.is_int32 $sloc i; i }

i64:
  i = NAT
| i = INT
{ check_constant Misc.is_int64 $sloc i; i }

f32:
  f = NAT
| f = INT
| f = FLOAT
{ check_constant Misc.is_float32 $sloc f; f }

f64:
  f = NAT
| f = INT
| f = FLOAT
{ check_constant Misc.is_float64 $sloc f; f }

idx:
| n = u32 { with_loc $sloc (Num n) }
| i = ID { {info = i.info; desc = (Id i.Ast.desc)} }

name: s = STRING
  { if not (String.is_valid_utf_8 s.Ast.desc) then
      raise
        (Parsing.Syntax_error
           ( (s.info.Ast.loc_start, s.info.loc_end),
             Printf.sprintf "Malformed name \"%s\".\n"
               (snd (Misc.escape_string s.desc))));
    s
  }

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
| I32 { I32 }
| I64 { I64 }
| F32 { F32 }
| F64 { F64 }
| V128 { V128 }
| t = reftype { Ref t }
| t = tupletype { Tuple t }

functype:
| "(" FUNC r = params_and_results(")") { fst r }

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
  { let (p, (r, c)) = r in
    ({params = Array.of_list p; results = Array.of_list r}, c) }

params_and_results_no_bindings(cont):
| r = params_no_bindings(results(cont))
  { let (p, (r, c)) = r in
    ({params = Array.of_list (List.map (fun t -> (None, t)) p);
      results = Array.of_list r },
     c) }

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
| "(" REC l = typedef * ")"
  { with_loc ($symbolstartpos, $endpos($2))
      (Types  (Array.map (fun t -> t.Ast.desc) (Array.of_list l))) }
| t = typedef { {t with desc = Types [|t.Ast.desc|]} }

typedef:
| "(" TYPE name = ID ? t = subtype ")"
 { with_loc ($symbolstartpos, $endpos(name)) (name, t) }

subtype:
| "(" SUB final = boption(FINAL) supertype = idx ? typ = comptype
  ")"
  { {final; supertype; typ} }
| typ = comptype { {final = true; supertype = None; typ } }

address_type:
| I32 { `I32 }
| I64 { `I64 }

limits:
| mi = u64 { with_loc $sloc {mi; ma = None; address_type = `I32} }
| mi = u64 ma = u64 { with_loc $sloc {mi; ma = Some ma; address_type = `I32} }
| at = address_type mi = u64
  { with_loc $sloc {mi; ma = None; address_type = at} }
| at = address_type mi = u64 ma = u64
  { with_loc $sloc {mi; ma = Some ma; address_type = at} }

memtype:
| l = limits { l }

tabletype(cont):
| l = limits t = reftype c = cont { ({limits = l; reftype = t}, c) }

(* Instructions *)

blockinstr:
| BLOCK label = label bti = blocktype(instrs(END)) label2 = label
  { check_labels label label2;
    let (typ, block) = bti in with_loc $sloc (Block {label; typ; block}) }
| LOOP label = label bti = blocktype(instrs(END)) label2 = label
  { check_labels label label2;
    let (typ, block) = bti in with_loc $sloc (Loop {label; typ; block}) }
| IF label = label bti = blocktype(instrs(ELSE))
  label2 = label else_block = instrs(END)
  label3 = label
  { check_labels label label2;
    check_labels label label3;
    let (typ, if_block) = bti in
    with_loc $sloc (If {label; typ; if_block; else_block }) }
| IF label = label bti = blocktype(instrs(END))
  label2 = label
  { check_labels label label2;
    let (typ, if_block) = bti in
    with_loc $sloc (If {label; typ; if_block; else_block = [] }) }
| TRY_TABLE label = label bti = blocktype(tbl_catches(instrs({})))
  END label2 = label
   { check_labels label label2;
     let (typ, (catches, block)) = bti in
     with_loc $sloc (TryTable {label; typ; catches; block}) }
| TRY label = label bti = blocktype(instrs({})) c = catches END label2 = label
  { check_labels label label2;
    let (typ, block) = bti in
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
| THROW_REF { with_loc $sloc ThrowRef }
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
| op = VEC_SHIFT_OP { with_loc $sloc (VecShift op) }
| op = VEC_BITMASK_OP { with_loc $sloc (VecBitmask op) }
| op = VEC_TERN_OP { with_loc $sloc (VecTernOp op) }
| VEC_BITSELECT { with_loc $sloc VecBitselect }
| GLOBAL_GET i = idx { with_loc $sloc (GlobalGet i) }
| GLOBAL_SET i = idx { with_loc $sloc (GlobalSet i) }
| sz = LOAD i = memidx m = memarg
  { with_loc $sloc (Load (i, m (Uint64.one), sz)) }
| op = VEC_LOAD i = memidx m = memarg
  { with_loc $sloc (VecLoad (i, op, m (Uint64.one))) }
| k = LOADS i = memidx m = memarg
  { let (sz, sz', s) = k in
    with_loc $sloc (LoadS (i, m Uint64.one, sz, sz', s)) }
| sz = STORE i = memidx m = memarg
  { with_loc $sloc (Store (i, m (Uint64.one), sz)) }
| VEC_STORE i = memidx m = memarg
  { with_loc $sloc (VecStore (i, m (Uint64.one))) }
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
| I32_CONST i = i32
  { with_loc $sloc (Const (I32 i)) }
| I64_CONST i = i64
  { with_loc $sloc (Const (I64 i)) }
| F32_CONST f = f32
  { with_loc $sloc (Const (F32 f)) }
| F64_CONST f = f64
  { with_loc $sloc (Const (F64 f)) }
| V128_CONST I8X16 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8 i8
  { let components =
      [$3; $4; $5; $6; $7; $8; $9; $10; $11; $12; $13; $14; $15; $16; $17; $18]
    in
    with_loc $sloc (VecConst {V128.shape = I8x16; components}) }
| V128_CONST I16X8 i16 i16 i16 i16 i16 i16 i16 i16
  { let components = [$3; $4; $5; $6; $7; $8; $9; $10] in
    with_loc $sloc (VecConst {V128.shape = I16x8; components}) }
| V128_CONST I32X4 i0 = i32 i1 = i32 i2 = i32 i3 = i32
  { let components = [i0; i1; i2; i3] in
    with_loc $sloc (VecConst {V128.shape = I32x4; components}) }
| V128_CONST I64X2 i0 = i64 i1 = i64
  { let components = [i0; i1] in
    with_loc $sloc (VecConst {V128.shape = I64x2; components}) }
| V128_CONST F32X4 f0 = f32 f1 = f32 f2 = f32 f3 = f32
  { let components = [f0; f1; f2; f3] in
    with_loc $sloc (VecConst {V128.shape = F32x4; components}) }
| V128_CONST F64X2 f0 = f64 f1 = f64
  { let components = [f0; f1] in
    with_loc $sloc (VecConst {V128.shape = F64x2; components}) }
| op = VEC_EXTRACT i = u8
  { with_loc $sloc (VecExtract (fst op, snd op, int_of_string i)) }
| op = VEC_REPLACE i = u8
  { with_loc $sloc (VecReplace (op, int_of_string i)) }
| VEC_SHUFFLE i0=u8 i1=u8 i2=u8 i3=u8 i4=u8 i5=u8 i6=u8 i7=u8 i8=u8
              i9=u8 i10=u8 i11=u8 i12=u8 i13=u8 i14=u8 i15=u8
  { let lanes =
      String.concat ""
        (List.map (fun l -> String.make 1 (Char.chr (int_of_string l)))
           [i0; i1; i2; i3; i4; i5; i6; i7; i8;
            i9; i10; i11; i12; i13; i14; i15])
    in
    with_loc $sloc (VecShuffle lanes) }
| op = VEC_LOAD_LANE i = memidx m = memarg l = u8
  { with_loc $sloc (VecLoadLane (i, op, m (lane_width op), int_of_string l)) }
| op = VEC_STORE_LANE i = memidx m = memarg l = u8
  { with_loc $sloc
      (VecStoreLane (i, op, m (lane_width op), int_of_string l)) }
| op = VEC_LOAD_SPLAT i = memidx m = memarg
  { with_loc $sloc (VecLoadSplat (i, op, m (lane_width op))) }
| TUPLE_MAKE l = u32 { with_loc $sloc (TupleMake l) }
| TUPLE_EXTRACT l = u32 i = u32
  { with_loc $sloc (TupleExtract (l, i)) }
| i = INSTR { with_loc $sloc i }

%inline memarg:
| o = ioption(MEM_OFFSET) a = ioption(MEM_ALIGN)
  { fun width ->
    {offset = Option.value ~default:Uint64.zero (Option.map Uint64.of_string o);
     align = Option.value ~default:(width : Uint64.t) (Option.map Uint64.of_string a)} }

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
| STRING_ANNOT id = option(idx) l = list(STRING) ")"
    { with_loc $sloc (String (id, l)) }
| CHAR_ANNOT s = STRING ")"
    { let c = String.get_utf_8_uchar s.Ast.desc 0 in
      if
        not (Uchar.utf_decode_is_valid c) ||
        Uchar.utf_decode_length c <> String.length s.desc
      then
        raise
          (Parsing.Syntax_error
             ( $sloc,
               Printf.sprintf "Malformed char \"%s\".\n"
                 (snd (Misc.escape_string s.desc))));
      with_loc $sloc (Char (Uchar.utf_decode_uchar c)) }

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
  { let (s, r) = rem in
    (match s with
     | {params = [||]; results = [||]} -> Some i, None
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
    { let (id, desc) = desc in
      with_loc ($symbolstartpos, $endpos(name))
        (Import {module_; name; id; desc; exports = [] }) }

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
    with_loc ($symbolstartpos, $endpos(id))
      (Func {id; typ; locals; instrs; exports}) }
| "(" FUNC id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = typeuse({}) ")"
  { let (exports, (module_, name)) = r in
    with_loc ($symbolstartpos, $endpos(id))
      (Import {module_; name; id; desc = Func (fst t); exports }) }

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
  { let (exports, limits) = r in
    with_loc ($symbolstartpos, $endpos(id))
      (Memory {id; limits; init = None; exports}) }
| "(" MEMORY id = ID?
  r = exports(at = ioption(address_type) "(" DATA s = datastring ")"
              { (at, s) }) ")"
  { let (exports, (at, s)) = r in
    let address_type = Option.value ~default:`I32 at in
    let data_len =
      List.fold_left (fun len {Ast.desc; _} -> len + String.length desc) 0 s in
    let sz = Uint64.of_int ((data_len + 65535) lsr 16) in
    let limits = Ast.no_loc {mi = sz; ma = Some sz; address_type} in
    with_loc ($symbolstartpos, $endpos(id))
      (Memory {id; limits; init = Some s; exports}) }
| "(" MEMORY id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = memtype ")"
  { let (exports, (module_, name)) = r in
    with_loc ($symbolstartpos, $endpos(id))
      (Import {module_; name; id; desc = Memory t; exports}) }

table:
| "(" TABLE id = ID? r = exports(tabletype(expr)) ")"
  { let (exports, (typ, e)) = r in
    let init = if e = [] then Init_default else Init_expr e in
    with_loc ($symbolstartpos, $endpos(id))
      (Table {id; typ; init; exports}) }
| "(" TABLE id = ID?
  r = exports(at = ioption(address_type) t = reftype "(" ELEM e = list(elemexpr) ")" {at, t, e}) ")"
  { let (exports, (at, reftype, elem)) = r in
    let address_type = Option.value ~default:`I32 at in
    let len = Uint64.of_int (List.length elem) in
    let limits = Ast.no_loc {mi=len; ma =Some len; address_type} in
    with_loc ($symbolstartpos, $endpos(id))
      (Table {id; typ = {limits; reftype};
              init = Init_segment elem; exports}) }
| "(" TABLE id = ID?
  r = exports(at = ioption(address_type) t = reftype "(" ELEM e = nonempty_list(idx) ")" {at, t, e}) ")"
  { let (exports, (at, reftype, elem)) = r in
    let address_type = Option.value ~default:`I32 at in
    let len = Uint64.of_int (List.length elem) in
    let elem = List.map (fun i -> [{i with Ast.desc = RefFunc i}]) elem in
    let limits = Ast.no_loc {mi=len; ma =Some len; address_type} in
    with_loc ($symbolstartpos, $endpos(id))
      (Table {id; typ = { limits; reftype };
              init = Init_segment elem; exports}) }
| "(" TABLE id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = tabletype({}) ")"
  { let (exports, (module_, name)) = r in
    with_loc ($symbolstartpos, $endpos(id))
      (Import {module_; name; id; desc = Table (fst t); exports }) }

tag:
| "(" TAG id = ID ? r = exports(typeuse (")"))
    { let (exports, (typ, _)) = r in
      with_loc ($symbolstartpos, $endpos(id))
        (Tag {id; typ; exports}) }
| "(" TAG id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  t = typeuse (")")
  { let (exports, (module_, name)) = r in
    with_loc ($symbolstartpos, $endpos(id))
      (Import {module_; name; id; desc = Tag (fst t); exports }) }

global:
| "(" GLOBAL id = ID ? r = exports(t = globaltype init = expr { t, init }) ")"
  { let (exports, (typ, init)) = r in
    with_loc ($symbolstartpos, $endpos(id))
      (Global {id; typ; init; exports}) }
| "(" GLOBAL id = ID ?
  r = exports("(" IMPORT module_ = name name = name ")" { (module_, name) })
  typ = globaltype ")"
  { let (exports, (module_, name)) = r in
    with_loc ($symbolstartpos, $endpos(id))
      (Import {module_; name; id; desc = Global typ; exports }) }

globaltype:
| typ = valtype { {mut = false; typ} }
| "(" MUT typ = valtype ")" { {mut = true; typ} }

export:
| "(" EXPORT name = name d = exportdesc ")"
  { let (index, kind) = d in
    with_loc ($symbolstartpos, $endpos(name))
      (Export {name; kind; index}) }

exportdesc:
| "(" FUNC i = idx ")" { (i, Func) }
| "(" GLOBAL i = idx ")" { (i, Global) }
| "(" MEMORY i = idx ")" { (i, Memory) }
| "(" TABLE i = idx ")" { (i, Table) }
| "(" TAG i = idx ")" { (i, (Tag : exportable)) }

start:
| "(" START i = idx ")" { with_loc $sloc (Start i) }

elem:
| "(" ELEM id = ID ? l = elemlist ")"
  { let (typ, init) = l in
    with_loc ($symbolstartpos, $endpos(id))
      (Elem {id; mode = Passive; typ; init}) }
| "(" ELEM id = ID ? t = tableuse o = offset l = elemlist ")"
  { let (typ, init) = l in
    with_loc ($symbolstartpos, $endpos(id))
      (Elem {id; mode = Active (t, o); typ; init}) }
| "(" ELEM id = ID ? DECLARE l = elemlist ")"
  { let (typ, init) = l in
    with_loc ($symbolstartpos, $endpos(id))
      (Elem {id; mode = Declare; typ; init}) }
| "(" ELEM id = ID ? o = offset init = list(idx) ")"
  { let init = List.map (fun i -> [{i with Ast.desc = RefFunc i}]) init in
    with_loc ($symbolstartpos, $endpos(id))
      (Elem {id; mode = Active (Ast.no_loc (Num Uint32.zero), o);
             typ = { nullable = false ; typ = Func }; init}) }

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
  { with_loc ($symbolstartpos, $endpos(id))
      (Data { id; init; mode = Passive }) }
| "(" DATA id = ID ? m = memuse e = offset init = datastring ")"
  { with_loc ($symbolstartpos, $endpos(id))
      (Data { id; init; mode = Active (m, e) }) }

offset:
| "(" OFFSET e = expr ")" { e }
| instr = foldedinstr { [instr] }

datastring:
| l = STRING * { l }

%inline memuse:
| "(" MEMORY i = idx ")" { i }
| { with_loc $sloc (Num Uint32.zero) }

globalstring:
| STRING_ANNOT id = ID typ = option(idx) init = list(STRING) ")"
  { with_loc $sloc (String_global {id; typ; init}) }

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
| f = globalstring
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
| "(" MODULE DEFINITION ? ID ? BINARY s = STRING *  ")"
  { fun status ->
    [(status, `Binary (String.concat "" (List.map (fun s -> s.Ast.desc) s)))] }
| "(" MODULE DEFINITION ? ID ? QUOTE s = STRING *  ")"
  { fun status ->
    [(status, `Text (String.concat "\n" (List.map (fun s -> s.Ast.desc) s)))] }

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
| "(" V128_CONST vec_shape f64+ ")"
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
| "(" ASSERT_MALFORMED m = module_ r = STRING ")"
  { m (`Malformed ((fun s -> s.Ast.desc) r)) }
| "(" ASSERT_INVALID m = module_ r = STRING ")"
  { m (`Invalid ((fun s -> s.Ast.desc) r)) }
| "(" ASSERT_UNLINKABLE m = script_instance STRING ")" { m `Valid }
| "(" ASSERT_TRAP m = script_instance STRING ")" { m `Valid }

float_or_nan: f64 | NAN {}

result_pat:
| "(" I32_CONST i32 ")"
| "(" I64_CONST i64 ")"
| "(" F32_CONST f32 ")"
| "(" F32_CONST NAN ")"
| "(" F64_CONST f64 ")"
| "(" F64_CONST NAN ")"
| "(" V128_CONST vec_shape float_or_nan+ ")"
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
