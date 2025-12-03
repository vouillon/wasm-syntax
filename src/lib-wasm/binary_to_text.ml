open Ast
module B = Binary
module T = Text

let no_loc = Ast.no_loc
let convert_idx i = no_loc (T.Num (Uint32.of_int i))

let rec convert_heaptype (h : B.heaptype) : T.heaptype =
  match h with
  | B.Func -> T.Func
  | B.NoFunc -> T.NoFunc
  | B.Exn -> T.Exn
  | B.NoExn -> T.NoExn
  | B.Extern -> T.Extern
  | B.NoExtern -> T.NoExtern
  | B.Any -> T.Any
  | B.Eq -> T.Eq
  | B.I31 -> T.I31
  | B.Struct -> T.Struct
  | B.Array -> T.Array
  | B.None_ -> T.None_
  | B.Type i -> T.Type (convert_idx i)

and convert_reftype (r : B.reftype) : T.reftype =
  { nullable = r.nullable; typ = convert_heaptype r.typ }

let convert_valtype (v_orig : B.valtype) : T.valtype =
  let rec convert_valtype_rec (v : B.valtype) : T.valtype =
    match v with
    | B.I32 -> T.I32
    | B.I64 -> T.I64
    | B.F32 -> T.F32
    | B.F64 -> T.F64
    | B.V128 -> T.V128
    | B.Ref r -> T.Ref (convert_reftype r)
    | B.Tuple l -> T.Tuple (List.map convert_valtype_rec l)
  in
  convert_valtype_rec v_orig

let convert_storagetype (s : B.storagetype) : T.storagetype =
  match s with
  | B.Value v -> T.Value (convert_valtype v)
  | B.Packed p -> T.Packed p

let convert_muttype (f : 'a -> 'b) (m : 'a B.muttype) : 'b T.muttype =
  { mut = m.mut; typ = f m.typ }

let convert_fieldtype (f : B.fieldtype) : T.fieldtype =
  convert_muttype convert_storagetype f

let convert_functype (f : B.functype) : T.functype =
  {
    params = Array.map convert_valtype f.params;
    results = Array.map convert_valtype f.results;
  }

let convert_comptype (c : B.comptype) : T.comptype =
  match c with
  | B.Func ft -> T.Func (convert_functype ft)
  | B.Struct fa ->
      T.Struct (Array.map (fun f -> (None, convert_fieldtype f)) fa)
      (* Binary has no field names *)
  | B.Array ft -> T.Array (convert_fieldtype ft)

let convert_subtype (s : B.subtype) : T.subtype =
  {
    typ = convert_comptype s.typ;
    supertype = Option.map convert_idx s.supertype;
    final = s.final;
  }

let convert_rectype (r : B.rectype) : T.rectype =
  Array.map (fun s -> (None, convert_subtype s)) r

let convert_globaltype (g : B.globaltype) : T.globaltype =
  convert_muttype convert_valtype g

let convert_tabletype (t : B.tabletype) : T.tabletype =
  { limits = t.limits; reftype = convert_reftype t.reftype }

let convert_memarg (m : B.memarg) : T.memarg =
  { offset = m.offset; align = m.align }

let convert_blocktype (b : B.blocktype) : T.blocktype =
  match b with
  | B.Typeuse i -> T.Typeuse (Some (convert_idx i), None)
  | B.Valtype v -> T.Valtype (convert_valtype v)

let convert_int_un_op (op_b : B.int_un_op) : T.int_un_op =
  match op_b with
  | B.Clz -> T.Clz
  | B.Ctz -> T.Ctz
  | B.Popcnt -> T.Popcnt
  | B.Eqz -> T.Eqz
  | B.Trunc (f, s) -> T.Trunc (f, s)
  | B.TruncSat (f, s) -> T.TruncSat (f, s)
  | B.Reinterpret -> T.Reinterpret
  | B.ExtendS s -> T.ExtendS s

let convert_float_un_op (op_b : B.float_un_op) : T.float_un_op =
  match op_b with
  | B.Neg -> T.Neg
  | B.Abs -> T.Abs
  | B.Ceil -> T.Ceil
  | B.Floor -> T.Floor
  | B.Trunc -> T.Trunc
  | B.Nearest -> T.Nearest
  | B.Sqrt -> T.Sqrt
  | B.Convert (i, s) -> T.Convert (i, s)
  | B.Reinterpret -> T.Reinterpret

let convert_int_bin_op (op_b : B.int_bin_op) : T.int_bin_op =
  match op_b with
  | B.Add -> T.Add
  | B.Sub -> T.Sub
  | B.Mul -> T.Mul
  | B.Div s -> T.Div s
  | B.Rem s -> T.Rem s
  | B.And -> T.And
  | B.Or -> T.Or
  | B.Xor -> T.Xor
  | B.Shl -> T.Shl
  | B.Shr s -> T.Shr s
  | B.Rotl -> T.Rotl
  | B.Rotr -> T.Rotr
  | B.Eq -> T.Eq
  | B.Ne -> T.Ne
  | B.Lt s -> T.Lt s
  | B.Gt s -> T.Gt s
  | B.Le s -> T.Le s
  | B.Ge s -> T.Ge s

let convert_float_bin_op (op_b : B.float_bin_op) : T.float_bin_op =
  match op_b with
  | B.Add -> T.Add
  | B.Sub -> T.Sub
  | B.Mul -> T.Mul
  | B.Div -> T.Div
  | B.Min -> T.Min
  | B.Max -> T.Max
  | B.CopySign -> T.CopySign
  | B.Eq -> T.Eq
  | B.Ne -> T.Ne
  | B.Lt -> T.Lt
  | B.Gt -> T.Gt
  | B.Le -> T.Le
  | B.Ge -> T.Ge

let convert_un_op
    (op_b : (B.int_un_op, B.int_un_op, B.float_un_op, B.float_un_op) B.op) :
    (T.int_un_op, T.int_un_op, T.float_un_op, T.float_un_op) T.op =
  match op_b with
  | B.I32 x -> T.I32 (convert_int_un_op x)
  | B.I64 x -> T.I64 (convert_int_un_op x)
  | B.F32 x -> T.F32 (convert_float_un_op x)
  | B.F64 x -> T.F64 (convert_float_un_op x)

let convert_bin_op
    (op_b : (B.int_bin_op, B.int_bin_op, B.float_bin_op, B.float_bin_op) B.op) :
    (T.int_bin_op, T.int_bin_op, T.float_bin_op, T.float_bin_op) T.op =
  match op_b with
  | B.I32 x -> T.I32 (convert_int_bin_op x)
  | B.I64 x -> T.I64 (convert_int_bin_op x)
  | B.F32 x -> T.F32 (convert_float_bin_op x)
  | B.F64 x -> T.F64 (convert_float_bin_op x)

let convert_unit_op (op_b : (unit, unit, unit, unit) B.op) :
    (unit, unit, unit, unit) T.op =
  match op_b with
  | B.I32 () -> T.I32 ()
  | B.I64 () -> T.I64 ()
  | B.F32 () -> T.F32 ()
  | B.F64 () -> T.F64 ()

let rec convert_instr (i : 'info B.instr) : 'info T.instr =
  let desc =
    match i.desc with
    | B.Block { label = _; typ; block } ->
        T.Block
          {
            label = None;
            typ = Option.map convert_blocktype typ;
            block = List.map convert_instr block;
          }
    | B.Loop { label = _; typ; block } ->
        T.Loop
          {
            label = None;
            typ = Option.map convert_blocktype typ;
            block = List.map convert_instr block;
          }
    | B.If { label = _; typ; if_block; else_block } ->
        T.If
          {
            label = None;
            typ = Option.map convert_blocktype typ;
            if_block = List.map convert_instr if_block;
            else_block = List.map convert_instr else_block;
          }
    | B.TryTable _ -> T.Nop (* Unsupported in Text format *)
    | B.Try _ -> T.Nop (* Unsupported in Text format *)
    | B.Unreachable -> T.Unreachable
    | B.Nop -> T.Nop
    | B.Throw i -> T.Throw (convert_idx i)
    | B.ThrowRef -> T.ThrowRef
    | B.Br i -> T.Br (convert_idx i)
    | B.Br_if i -> T.Br_if (convert_idx i)
    | B.Br_table (l, d) -> T.Br_table (List.map convert_idx l, convert_idx d)
    | B.Br_on_null i -> T.Br_on_null (convert_idx i)
    | B.Br_on_non_null i -> T.Br_on_non_null (convert_idx i)
    | B.Br_on_cast (l, r1, r2) ->
        T.Br_on_cast (convert_idx l, convert_reftype r1, convert_reftype r2)
    | B.Br_on_cast_fail (l, r1, r2) ->
        T.Br_on_cast_fail (convert_idx l, convert_reftype r1, convert_reftype r2)
    | B.Return -> T.Return
    | B.Call i -> T.Call (convert_idx i)
    | B.CallRef i -> T.CallRef (convert_idx i)
    | B.CallIndirect (t, i) ->
        T.CallIndirect (convert_idx i, (Some (convert_idx t), None))
    | B.ReturnCall i -> T.ReturnCall (convert_idx i)
    | B.ReturnCallRef i -> T.ReturnCallRef (convert_idx i)
    | B.ReturnCallIndirect (t, i) ->
        T.ReturnCallIndirect (convert_idx i, (Some (convert_idx t), None))
    | B.Drop -> T.Drop
    | B.Select None -> T.Select None
    | B.Select (Some l) -> T.Select (Some (List.map convert_valtype l))
    | B.LocalGet i -> T.LocalGet (convert_idx i)
    | B.LocalSet i -> T.LocalSet (convert_idx i)
    | B.LocalTee i -> T.LocalTee (convert_idx i)
    | B.GlobalGet i -> T.GlobalGet (convert_idx i)
    | B.GlobalSet i -> T.GlobalSet (convert_idx i)
    | B.Load (o, m, op) ->
        T.Load (convert_idx o, convert_memarg m, convert_unit_op op)
    | B.LoadS (o, m, sz, bt, s) ->
        T.LoadS (convert_idx o, convert_memarg m, sz, bt, s)
    | B.Store (o, m, op) ->
        T.Store (convert_idx o, convert_memarg m, convert_unit_op op)
    | B.StoreS (o, m, sz, bt) ->
        T.StoreS (convert_idx o, convert_memarg m, sz, bt)
    | B.MemorySize i -> T.MemorySize (convert_idx i)
    | B.MemoryGrow i -> T.MemoryGrow (convert_idx i)
    | B.MemoryFill i -> T.MemoryFill (convert_idx i)
    | B.MemoryCopy (i1, i2) -> T.MemoryCopy (convert_idx i1, convert_idx i2)
    | B.MemoryInit (i1, i2) -> T.MemoryInit (convert_idx i1, convert_idx i2)
    | B.DataDrop i -> T.DataDrop (convert_idx i)
    | B.TableGet i -> T.TableGet (convert_idx i)
    | B.TableSet i -> T.TableSet (convert_idx i)
    | B.TableSize i -> T.TableSize (convert_idx i)
    | B.TableGrow i -> T.TableGrow (convert_idx i)
    | B.TableFill i -> T.TableFill (convert_idx i)
    | B.TableCopy (i1, i2) -> T.TableCopy (convert_idx i1, convert_idx i2)
    | B.TableInit (i1, i2) -> T.TableInit (convert_idx i1, convert_idx i2)
    | B.ElemDrop i -> T.ElemDrop (convert_idx i)
    | B.RefNull h -> T.RefNull (convert_heaptype h)
    | B.RefFunc i -> T.RefFunc (convert_idx i)
    | B.RefIsNull -> T.RefIsNull
    | B.RefAsNonNull -> T.RefAsNonNull
    | B.RefEq -> T.RefEq
    | B.RefTest r -> T.RefTest (convert_reftype r)
    | B.RefCast r -> T.RefCast (convert_reftype r)
    | B.StructNew i -> T.StructNew (convert_idx i)
    | B.StructNewDefault i -> T.StructNewDefault (convert_idx i)
    | B.StructGet (s, s_idx, f_idx) ->
        T.StructGet (s, convert_idx s_idx, convert_idx f_idx)
    | B.StructSet (s_idx, f_idx) ->
        T.StructSet (convert_idx s_idx, convert_idx f_idx)
    | B.ArrayNew i -> T.ArrayNew (convert_idx i)
    | B.ArrayNewDefault i -> T.ArrayNewDefault (convert_idx i)
    | B.ArrayNewFixed (i, len) -> T.ArrayNewFixed (convert_idx i, len)
    | B.ArrayNewData (i1, i2) -> T.ArrayNewData (convert_idx i1, convert_idx i2)
    | B.ArrayNewElem (i1, i2) -> T.ArrayNewElem (convert_idx i1, convert_idx i2)
    | B.ArrayGet (s, i) -> T.ArrayGet (s, convert_idx i)
    | B.ArraySet i -> T.ArraySet (convert_idx i)
    | B.ArrayLen -> T.ArrayLen
    | B.ArrayFill i -> T.ArrayFill (convert_idx i)
    | B.ArrayCopy (i1, i2) -> T.ArrayCopy (convert_idx i1, convert_idx i2)
    | B.ArrayInitData (i1, i2) ->
        T.ArrayInitData (convert_idx i1, convert_idx i2)
    | B.ArrayInitElem (i1, i2) ->
        T.ArrayInitElem (convert_idx i1, convert_idx i2)
    | B.RefI31 -> T.RefI31
    | B.I31Get s -> T.I31Get s
    | B.Const (B.I32 x) -> T.Const (T.I32 (Int32.to_string x))
    | B.Const (B.I64 x) -> T.Const (T.I64 (Int64.to_string x))
    | B.Const (B.F32 x) -> T.Const (T.F32 (string_of_float x))
    | B.Const (B.F64 x) -> T.Const (T.F64 (string_of_float x))
    | B.UnOp op -> T.UnOp (convert_un_op op)
    | B.BinOp op -> T.BinOp (convert_bin_op op)
    | B.I32WrapI64 -> T.I32WrapI64
    | B.I64ExtendI32 s -> T.I64ExtendI32 s
    | B.F32DemoteF64 -> T.F32DemoteF64
    | B.F64PromoteF32 -> T.F64PromoteF32
    | B.ExternConvertAny -> T.ExternConvertAny
    | B.AnyConvertExtern -> T.AnyConvertExtern
    | B.Folded (i1, il) -> T.Folded (convert_instr i1, List.map convert_instr il)
    | B.Pop v -> T.Pop (convert_valtype v)
    | B.TupleMake i -> T.TupleMake i
    | B.TupleExtract (i1, i2) -> T.TupleExtract (i1, i2)
  in
  { desc; info = i.info }

let convert_expr (e : 'info B.expr) : 'info T.expr =
  List.map convert_instr e

let convert_elemmode (e : 'info B.elemmode) : 'info T.elemmode =
  match e with
  | B.Passive -> T.Passive
  | B.Active (i, ex) -> T.Active (convert_idx i, convert_expr ex)
  | B.Declare -> T.Declare

let convert_datamode (d : 'info B.datamode) : 'info T.datamode =
  match d with
  | B.Passive -> T.Passive
  | B.Active (i, ex) -> T.Active (convert_idx i, convert_expr ex)

let convert_exportable (e : B.exportable) : T.exportable =
  match (e : B.exportable) with
  | B.Func -> T.Func
  | B.Table -> T.Table
  | B.Memory -> T.Memory
  | B.Global -> T.Global
  | B.Tag -> T.Tag

let module_ (m : 'info B.module_) : 'info T.module_ =
  let types = List.map convert_rectype m.types in
  let imports =
    List.map
      (fun (imp : B.import) : 'info T.modulefield ->
        T.Import
          {
            module_ = imp.module_;
            name = imp.name;
            id = None;
            desc =
              (match imp.desc with
              | B.Func i -> T.Func (Some (convert_idx i), None)
              | B.Memory l -> T.Memory l
              | B.Table t -> T.Table (convert_tabletype t)
              | B.Global gt -> T.Global (convert_globaltype gt)
              | B.Tag i -> T.Tag (Some (convert_idx i), None));
            exports = [];
            (* Binary imports usually don't have exports here *)
          })
      m.imports
  in

  let funcs =
    List.mapi
      (fun i (func_type_idx : B.idx) ->
        let code = List.nth m.code i in
        (* Assuming parallel lists *)
        T.Func
          {
            id = None;
            (* Names are in m.names *)
            typ = (Some (convert_idx func_type_idx), None);
            (* Binary typeuse is idx *)
            locals = List.map (fun v -> (None, convert_valtype v)) code.locals;
            instrs = List.map convert_instr code.instrs;
            exports = [];
            (* Exports are handled separately *)
          })
      m.functions
  in

  let tables =
    List.map
      (fun (t : 'info B.table) : 'info T.modulefield ->
        let b_tabletype =
          {
            B.limits = t.typ;
            B.reftype = { B.nullable = false; B.typ = B.Func };
          }
        in
        T.Table
          {
            id = None;
            typ = convert_tabletype b_tabletype;
            init = T.Init_default;
            (* Binary init expr is in elem section *)
            exports = [];
          })
      m.tables
  in

  let memories =
    List.map
      (fun (l : B.limits) : 'info T.modulefield ->
        T.Memory
          {
            id = None;
            limits = l;
            init = None;
            (* Binary init is in data section *)
            exports = [];
          })
      m.memories
  in

  let globals =
    List.map
      (fun (g : 'info B.global) : 'info T.modulefield ->
        T.Global
          {
            id = None;
            typ = convert_globaltype g.typ;
            init = convert_expr g.init;
            exports = [];
          })
      m.globals
  in

  let exports =
    List.map
      (fun (e : B.export) : 'info T.modulefield ->
        T.Export
          {
            name = e.name;
            kind = convert_exportable e.kind;
            index = convert_idx e.index;
          })
      m.exports
  in

  let start = Option.map (fun i -> T.Start (convert_idx i)) m.start in

  let elems =
    List.map
      (fun (e : 'info B.elem) : 'info T.modulefield ->
        T.Elem
          {
            id = None;
            typ = convert_reftype e.typ;
            init = List.map convert_expr e.init;
            mode = convert_elemmode e.mode;
          })
      m.elem
  in

  let datas =
    List.map
      (fun (d : 'info B.data) : 'info T.modulefield ->
        T.Data { id = None; init = d.init; mode = convert_datamode d.mode })
      m.data
  in

    let tags =

      List.map

        (fun (type_idx : B.idx) : 'info T.modulefield ->

          T.Tag { id = None; typ = (Some (convert_idx type_idx), None); exports = [] })

        m.tags

    in

  ( None,
    List.flatten
      [
        List.map (fun t -> T.Types t) types;
        imports;
        funcs;
        tables;
        memories;
        globals;
        exports;
        (match start with Some s -> [ s ] | None -> []);
        elems;
        datas;
        tags;
      ] )
