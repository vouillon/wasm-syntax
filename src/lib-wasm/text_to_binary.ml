open Ast
module T = Text
module B = Binary
module StringMap = Map.Make (String)

type index_space = { map : B.idx StringMap.t; count : int }

let empty_space = { map = StringMap.empty; count = 0 }

let add_name space id =
  let idx = space.count in
  let map =
    match id with
    | Some name -> StringMap.add name idx space.map
    | None -> space.map
  in
  ({ map; count = idx + 1 }, idx)

type context = {
  funcs : index_space;
  globals : index_space;
  tables : index_space;
  memories : index_space;
  types : index_space;
  fields : int StringMap.t B.IntMap.t;
  tags : index_space;
  datas : index_space;
  elems : index_space;
  (* Label stack for current function *)
  labels : string option list;
  (* Locals for current function *)
  locals : index_space;
}

let empty_context =
  {
    funcs = empty_space;
    globals = empty_space;
    tables = empty_space;
    memories = empty_space;
    types = empty_space;
    fields = B.IntMap.empty;
    tags = empty_space;
    datas = empty_space;
    elems = empty_space;
    labels = [];
    locals = empty_space;
  }

let resolve_idx space (idx : T.idx) : B.idx =
  match idx.desc with
  | T.Num n -> Utils.Uint32.to_int n
  | T.Id id -> (
      match StringMap.find_opt id space.map with
      | Some i -> i
      | None -> failwith ("Unknown identifier: " ^ id))

let resolve_label labels (idx : T.idx) : B.idx =
  match idx.desc with
  | T.Num n -> Utils.Uint32.to_int n
  | T.Id id ->
      let rec find_depth stack depth =
        match stack with
        | [] -> failwith ("Unknown label: " ^ id)
        | Some name :: rest ->
            if name = id then depth else find_depth rest (depth + 1)
        | None :: rest -> find_depth rest (depth + 1)
      in
      find_depth labels 0

(* Conversion functions *)

let heaptype ctx (h : T.heaptype) : B.heaptype =
  match h with
  | Func -> Func
  | NoFunc -> NoFunc
  | Exn -> Exn
  | NoExn -> NoExn
  | Extern -> Extern
  | NoExtern -> NoExtern
  | Any -> Any
  | Eq -> Eq
  | I31 -> I31
  | Struct -> Struct
  | Array -> Array
  | None_ -> None_
  | Type i -> Type (resolve_idx ctx.types i)

let reftype ctx (r : T.reftype) : B.reftype =
  { nullable = r.nullable; typ = heaptype ctx r.typ }

let rec valtype ctx (v : T.valtype) : B.valtype =
  match v with
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128
  | Ref r -> Ref (reftype ctx r)
  | Tuple l -> Tuple (List.map (valtype ctx) l)

let storage_type ctx (s : T.storagetype) : B.storagetype =
  match s with Value v -> Value (valtype ctx v) | Packed p -> Packed p

let mut_type typ_f ctx m = { mut = m.mut; typ = typ_f ctx m.typ }
let field_type ctx f = mut_type storage_type ctx f

let func_type ctx (f : T.functype) : B.functype =
  {
    params = Array.map (valtype ctx) f.params;
    results = Array.map (valtype ctx) f.results;
  }

let comp_type ctx (c : T.comptype) : B.comptype =
  match c with
  | Func f -> Func (func_type ctx f)
  | Struct f -> Struct (Array.map (fun (_, ft) -> field_type ctx ft) f)
  | Array f -> Array (field_type ctx f)

let sub_type ctx (s : T.subtype) : B.subtype =
  {
    typ = comp_type ctx s.typ;
    supertype = Option.map (resolve_idx ctx.types) s.supertype;
    final = s.final;
  }

let rec_type ctx r = Array.map (fun (_, s) -> sub_type ctx s) r
let global_type ctx g = mut_type valtype ctx g

let table_type_fix ctx (t : T.tabletype) : B.tabletype =
  { limits = t.limits; reftype = reftype ctx t.reftype }

let block_type ~resolve_type ctx (b : T.blocktype) : B.blocktype =
  match b with
  | Typeuse (Some i, _) -> Typeuse (resolve_idx ctx.types i)
  | Typeuse (None, Some ft) -> Typeuse (resolve_type (func_type ctx ft))
  | Typeuse (None, None) -> assert false
  | Valtype v -> Valtype (valtype ctx v)

let catch ctx (c : T.catch) : B.catch =
  match c with
  | Catch (tag, label) ->
      Catch (resolve_idx ctx.tags tag, resolve_label ctx.labels label)
  | CatchRef (tag, label) ->
      CatchRef (resolve_idx ctx.tags tag, resolve_label ctx.labels label)
  | CatchAll label -> CatchAll (resolve_label ctx.labels label)
  | CatchAllRef label -> CatchAllRef (resolve_label ctx.labels label)

let resolve_field_idx ctx type_idx (field_idx_text : T.idx) : B.idx =
  match field_idx_text.desc with
  | T.Num n -> Utils.Uint32.to_int n
  | T.Id id -> (
      match B.IntMap.find_opt type_idx ctx.fields with
      | Some field_map -> (
          match StringMap.find_opt id field_map with
          | Some f_idx -> f_idx
          | None ->
              failwith
                (Printf.sprintf
                   "Unknown field identifier '%s' for type index %d" id type_idx)
          )
      | None ->
          failwith
            (Printf.sprintf "No field map found for type index %d" type_idx))

let rec instr ~resolve_type ctx (i : 'info T.instr) =
  let desc : _ B.instr_desc =
    match i.desc with
    | Block { label; typ; block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        Block
          {
            label = ();
            typ = Option.map (block_type ~resolve_type ctx) typ;
            block = List.map (instr ~resolve_type ctx') block;
          }
    | Loop { label; typ; block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        Loop
          {
            label = ();
            typ = Option.map (block_type ~resolve_type ctx) typ;
            block = List.map (instr ~resolve_type ctx') block;
          }
    | If { label; typ; if_block; else_block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        If
          {
            label = ();
            typ = Option.map (block_type ~resolve_type ctx) typ;
            if_block = List.map (instr ~resolve_type ctx') if_block;
            else_block = List.map (instr ~resolve_type ctx') else_block;
          }
    | Unreachable -> Unreachable
    | Nop -> Nop
    | Br i -> Br (resolve_label ctx.labels i)
    | Br_if i -> Br_if (resolve_label ctx.labels i)
    | Br_table (ls, d) ->
        Br_table
          (List.map (resolve_label ctx.labels) ls, resolve_label ctx.labels d)
    | Return -> Return
    | Call i -> Call (resolve_idx ctx.funcs i)
    | ReturnCall i -> ReturnCall (resolve_idx ctx.funcs i)
    | CallIndirect (table, (idx_opt, type_opt)) ->
        let type_idx =
          match idx_opt with
          | Some i -> resolve_idx ctx.types i
          | None -> (
              match type_opt with
              | Some ft -> resolve_type (func_type ctx ft)
              | None -> assert false)
        in
        CallIndirect (resolve_idx ctx.tables table, type_idx)
    | ReturnCallIndirect (table, (idx_opt, type_opt)) ->
        let type_idx =
          match idx_opt with
          | Some i -> resolve_idx ctx.types i
          | None -> (
              match type_opt with
              | Some ft -> resolve_type (func_type ctx ft)
              | None -> assert false)
        in
        ReturnCallIndirect (resolve_idx ctx.tables table, type_idx)
    | Drop -> Drop
    | Select None -> Select None
    | Select (Some l) -> Select (Some (List.map (valtype ctx) l))
    | LocalGet i -> LocalGet (resolve_idx ctx.locals i)
    | LocalSet i -> LocalSet (resolve_idx ctx.locals i)
    | LocalTee i -> LocalTee (resolve_idx ctx.locals i)
    | GlobalGet i -> GlobalGet (resolve_idx ctx.globals i)
    | GlobalSet i -> GlobalSet (resolve_idx ctx.globals i)
    | Load (o, m, op) -> Load (resolve_idx ctx.memories o, m, op)
    | Store (o, m, op) -> Store (resolve_idx ctx.memories o, m, op)
    | LoadS (o, m, sz, bt, s) -> LoadS (resolve_idx ctx.memories o, m, sz, bt, s)
    | StoreS (o, m, sz, bt) -> StoreS (resolve_idx ctx.memories o, m, sz, bt)
    | MemorySize i -> MemorySize (resolve_idx ctx.memories i)
    | MemoryGrow i -> MemoryGrow (resolve_idx ctx.memories i)
    | MemoryFill i -> MemoryFill (resolve_idx ctx.memories i)
    | MemoryCopy (i1, i2) ->
        MemoryCopy (resolve_idx ctx.memories i1, resolve_idx ctx.memories i2)
    | MemoryInit (i1, i2) ->
        MemoryInit (resolve_idx ctx.memories i1, resolve_idx ctx.datas i2)
    | DataDrop i -> DataDrop (resolve_idx ctx.datas i)
    | TableGet i -> TableGet (resolve_idx ctx.tables i)
    | TableSet i -> TableSet (resolve_idx ctx.tables i)
    | TableSize i -> TableSize (resolve_idx ctx.tables i)
    | TableGrow i -> TableGrow (resolve_idx ctx.tables i)
    | TableFill i -> TableFill (resolve_idx ctx.tables i)
    | TableCopy (i1, i2) ->
        TableCopy (resolve_idx ctx.tables i1, resolve_idx ctx.tables i2)
    | TableInit (i1, i2) ->
        TableInit (resolve_idx ctx.tables i1, resolve_idx ctx.elems i2)
    | ElemDrop i -> ElemDrop (resolve_idx ctx.elems i)
    | Const (I32 x) -> Const (I32 (Int32.of_string x))
    | Const (I64 x) -> Const (I64 (Int64.of_string x))
    | Const (F32 x) -> Const (F32 (float_of_string x))
    | Const (F64 x) -> Const (F64 (float_of_string x))
    | UnOp op -> UnOp op
    | BinOp op -> BinOp op
    | RefNull t -> RefNull (heaptype ctx t)
    | RefFunc i -> RefFunc (resolve_idx ctx.funcs i)
    | RefIsNull -> RefIsNull
    | TryTable { label; typ; catches; block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        TryTable
          {
            label = ();
            typ = Option.map (block_type ~resolve_type ctx) typ;
            catches = List.map (catch ctx) catches;
            block = List.map (instr ~resolve_type ctx') block;
          }
    | Try { label; typ; block; catches; catch_all } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        Try
          {
            label = ();
            typ = Option.map (block_type ~resolve_type ctx) typ;
            block = List.map (instr ~resolve_type ctx') block;
            catches =
              List.map
                (fun (tag, b) ->
                  ( resolve_idx ctx.tags tag,
                    List.map (instr ~resolve_type ctx') b ))
                catches;
            catch_all =
              Option.map (List.map (instr ~resolve_type ctx')) catch_all;
          }
    | Throw i -> Throw (resolve_idx ctx.tags i)
    | ThrowRef -> ThrowRef
    | Br_on_null i -> Br_on_null (resolve_label ctx.labels i)
    | Br_on_non_null i -> Br_on_non_null (resolve_label ctx.labels i)
    | Br_on_cast (i, r1, r2) ->
        Br_on_cast (resolve_label ctx.labels i, reftype ctx r1, reftype ctx r2)
    | Br_on_cast_fail (i, r1, r2) ->
        Br_on_cast_fail
          (resolve_label ctx.labels i, reftype ctx r1, reftype ctx r2)
    | CallRef i -> CallRef (resolve_idx ctx.funcs i)
    | ReturnCallRef i -> ReturnCallRef (resolve_idx ctx.funcs i)
    | RefAsNonNull -> RefAsNonNull
    | RefEq -> RefEq
    | RefTest r -> RefTest (reftype ctx r)
    | RefCast r -> RefCast (reftype ctx r)
    | StructNew i -> StructNew (resolve_idx ctx.types i)
    | StructNewDefault i -> StructNewDefault (resolve_idx ctx.types i)
    | StructGet (s, i1, i2) ->
        let type_idx = resolve_idx ctx.types i1 in
        StructGet (s, type_idx, resolve_field_idx ctx type_idx i2)
    | StructSet (i1, i2) ->
        let type_idx = resolve_idx ctx.types i1 in
        StructSet (type_idx, resolve_field_idx ctx type_idx i2)
    | ArrayNew i -> ArrayNew (resolve_idx ctx.types i)
    | ArrayNewDefault i -> ArrayNewDefault (resolve_idx ctx.types i)
    | ArrayNewFixed (i, u) -> ArrayNewFixed (resolve_idx ctx.types i, u)
    | ArrayNewData (i1, i2) ->
        ArrayNewData (resolve_idx ctx.types i1, resolve_idx ctx.datas i2)
    | ArrayNewElem (i1, i2) ->
        ArrayNewElem (resolve_idx ctx.types i1, resolve_idx ctx.elems i2)
    | ArrayGet (s, i) -> ArrayGet (s, resolve_idx ctx.types i)
    | ArraySet i -> ArraySet (resolve_idx ctx.types i)
    | ArrayLen -> ArrayLen
    | ArrayFill i -> ArrayFill (resolve_idx ctx.types i)
    | ArrayCopy (i1, i2) ->
        ArrayCopy (resolve_idx ctx.types i1, resolve_idx ctx.types i2)
    | ArrayInitData (i1, i2) ->
        ArrayInitData (resolve_idx ctx.types i1, resolve_idx ctx.datas i2)
    | ArrayInitElem (i1, i2) ->
        ArrayInitElem (resolve_idx ctx.types i1, resolve_idx ctx.elems i2)
    | RefI31 -> RefI31
    | I31Get s -> I31Get s
    | I32WrapI64 -> I32WrapI64
    | I64ExtendI32 s -> I64ExtendI32 s
    | F32DemoteF64 -> F32DemoteF64
    | F64PromoteF32 -> F64PromoteF32
    | ExternConvertAny -> ExternConvertAny
    | AnyConvertExtern -> AnyConvertExtern
    | Pop i -> Pop (valtype ctx i)
    | TupleMake u -> TupleMake u
    | TupleExtract (u1, u2) -> TupleExtract (u1, u2)
    | Folded (i, is) ->
        Folded (instr ~resolve_type ctx i, List.map (instr ~resolve_type ctx) is)
  in
  { desc; info = i.info }

let collect_labels instrs ctr map =
  let rec go instrs ctr map =
    List.fold_left
      (fun map (i : _ T.instr) ->
        match i.desc with
        | Block { label; block; _ } | Loop { label; block; _ } ->
            let idx = !ctr in
            incr ctr;
            let map =
              match label with Some l -> B.IntMap.add idx l map | None -> map
            in
            go block ctr map
        | If { label; if_block; else_block; _ } ->
            let idx = !ctr in
            incr ctr;
            let map =
              match label with Some l -> B.IntMap.add idx l map | None -> map
            in
            let map = go if_block ctr map in
            go else_block ctr map
        | Try { label; block; catches; catch_all; _ } -> (
            let idx = !ctr in
            incr ctr;
            let map =
              match label with Some l -> B.IntMap.add idx l map | None -> map
            in
            let map = go block ctr map in
            let map =
              List.fold_left (fun map (_, b) -> go b ctr map) map catches
            in
            match catch_all with Some b -> go b ctr map | None -> map)
        | TryTable { label; block; _ } ->
            let idx = !ctr in
            incr ctr;
            let map =
              match label with Some l -> B.IntMap.add idx l map | None -> map
            in
            go block ctr map
        | _ -> map)
      map instrs
  in
  go instrs ctr map

let invert_map map =
  StringMap.fold (fun k v acc -> B.IntMap.add v k acc) map B.IntMap.empty

let module_ (m : 'info T.module_) : 'info B.module_ =
  let module_name, fields = m in

  (* Pass 1: Build Context *)
  let ctx = empty_context in

  let func_types_by_idx = B.IntMap.empty in
  let ctx, func_types_by_idx =
    List.fold_left
      (fun (ctx, acc_func_types) f ->
        match f with
        | T.Types r ->
            let types_space, _ =
              Array.fold_left
                (fun (space, _) (id, _) -> add_name space id)
                (ctx.types, 0) r
            in
            let current_type_idx = ctx.types.count in
            let acc_func_types =
              Array.fold_left
                (fun (acc_map, idx_in_arr) (_, subtype) ->
                  match subtype.T.typ with
                  | T.Func func_t ->
                      let b_func_t = func_type ctx func_t in
                      ( B.IntMap.add
                          (current_type_idx + idx_in_arr)
                          (Array.length b_func_t.B.Types.params)
                          acc_map,
                        idx_in_arr + 1 )
                  | _ -> (acc_map, idx_in_arr + 1))
                (acc_func_types, 0) r
              |> fst
            in
            ({ ctx with types = types_space }, acc_func_types)
        | T.Import { id; desc; _ } -> (
            match desc with
            | T.Func _ ->
                ( { ctx with funcs = fst (add_name ctx.funcs id) },
                  acc_func_types )
            | T.Table _ ->
                ( { ctx with tables = fst (add_name ctx.tables id) },
                  acc_func_types )
            | T.Memory _ ->
                ( { ctx with memories = fst (add_name ctx.memories id) },
                  acc_func_types )
            | T.Global _ ->
                ( { ctx with globals = fst (add_name ctx.globals id) },
                  acc_func_types )
            | T.Tag _ ->
                ({ ctx with tags = fst (add_name ctx.tags id) }, acc_func_types)
            )
        | T.Func { id; _ } ->
            ({ ctx with funcs = fst (add_name ctx.funcs id) }, acc_func_types)
        | T.Table { id; _ } ->
            ({ ctx with tables = fst (add_name ctx.tables id) }, acc_func_types)
        | T.Memory { id; _ } ->
            ( { ctx with memories = fst (add_name ctx.memories id) },
              acc_func_types )
        | T.Global { id; _ } ->
            ( { ctx with globals = fst (add_name ctx.globals id) },
              acc_func_types )
        | T.Tag { id; _ } ->
            ({ ctx with tags = fst (add_name ctx.tags id) }, acc_func_types)
        | T.Elem { id; _ } ->
            ({ ctx with elems = fst (add_name ctx.elems id) }, acc_func_types)
        | T.Data { id; _ } ->
            ({ ctx with datas = fst (add_name ctx.datas id) }, acc_func_types)
        | _ -> (ctx, acc_func_types))
      (ctx, func_types_by_idx) fields
  in

  (* Collect Struct Field Names *)
  let field_names =
    let rec scan_fields type_idx fields acc =
      match fields with
      | [] -> acc
      | T.Types r :: rest ->
          let acc, _ =
            Array.fold_left
              (fun (acc, i) (_, subtype) ->
                match subtype.T.typ with
                | T.Struct field_defs ->
                    let field_map =
                      Array.fold_left
                        (fun (fmap, fidx) (fname, _) ->
                          match fname with
                          | Some n -> (StringMap.add n fidx fmap, fidx + 1)
                          | None -> (fmap, fidx + 1))
                        (StringMap.empty, 0) field_defs
                      |> fst
                    in
                    if StringMap.is_empty field_map then (acc, i + 1)
                    else (B.IntMap.add (type_idx + i) field_map acc, i + 1)
                | _ -> (acc, i + 1))
              (acc, 0) r
          in
          scan_fields (type_idx + Array.length r) rest acc
      | _ :: rest -> scan_fields type_idx rest acc
    in
    scan_fields 0 fields B.IntMap.empty
  in
  let ctx = { ctx with fields = field_names } in

  (* Type Memoization *)
  let type_map = Hashtbl.create 1024 in
  let extra_types = ref [] in
  let type_count = ref ctx.types.count in

  (* Populate type_map with existing explicit types *)
  let () =
    let rec scan_existing_types idx fields =
      match fields with
      | [] -> ()
      | T.Types r :: rest ->
          Array.iteri
            (fun i (_, subtype) ->
              match subtype.T.typ with
              | T.Func f ->
                  let b_f = func_type ctx f in
                  if not (Hashtbl.mem type_map b_f) then
                    Hashtbl.add type_map b_f (idx + i)
              | _ -> ())
            r;
          scan_existing_types (idx + Array.length r) rest
      | _ :: rest -> scan_existing_types idx rest
    in
    scan_existing_types 0 fields
  in

  let resolve_type (ft : B.functype) : int =
    match Hashtbl.find_opt type_map ft with
    | Some i -> i
    | None ->
        let i = !type_count in
        type_count := i + 1;
        Hashtbl.add type_map ft i;
        extra_types := ft :: !extra_types;
        i
  in

  (* Pass 2: Convert *)
  let imports =
    List.filter_map
      (fun f ->
        match f with
        | T.Import { module_; name; desc; _ } ->
            let desc : B.importdesc =
              match desc with
              | Func (Some i, _) -> Func (resolve_idx ctx.types i)
              | Func (None, Some (params, results)) ->
                  (* Inline type in Import *)
                  let params =
                    Array.of_list
                      (List.map (fun (_, t) -> valtype ctx t) params)
                  in
                  let results =
                    Array.of_list (List.map (valtype ctx) results)
                  in
                  Func (resolve_type { params; results })
              | Func (None, None) -> assert false
              | Table t -> Table (table_type_fix ctx t)
              | Memory l -> Memory l
              | Global g -> Global (global_type ctx g)
              | Tag (Some i, _) -> Tag (resolve_idx ctx.types i)
              | Tag (None, Some (params, results)) ->
                  let params =
                    Array.of_list
                      (List.map (fun (_, t) -> valtype ctx t) params)
                  in
                  let results =
                    Array.of_list (List.map (valtype ctx) results)
                  in
                  Tag (resolve_type { params; results })
              | Tag (None, None) -> failwith "Tag import missing type"
            in
            Some { B.module_; name; desc }
        | _ -> None)
      fields
  in

  let explicit_types =
    List.filter_map
      (fun f -> match f with T.Types r -> Some (rec_type ctx r) | _ -> None)
      fields
  in

  let functions =
    List.filter_map
      (fun f ->
        match f with
        | T.Func { typ = Some i, _; _ } -> Some (resolve_idx ctx.types i)
        | T.Func { typ = None, Some (params, results); _ } ->
            let params =
              Array.of_list (List.map (fun (_, t) -> valtype ctx t) params)
            in
            let results = Array.of_list (List.map (valtype ctx) results) in
            Some (resolve_type { B.params; results })
        | T.Func { typ = None, None; _ } -> failwith "Func missing type"
        | _ -> None)
      fields
  in

  (* Prepare for Code Generation: Calculate Import Count for Indexing *)
  let func_import_count =
    List.fold_left
      (fun acc f ->
        match f with T.Import { desc = T.Func _; _ } -> acc + 1 | _ -> acc)
      0 fields
  in

  let locals_names = ref B.IntMap.empty in
  let labels_names = ref B.IntMap.empty in

  let code =
    let rec process_funcs func_types_by_idx fields func_idx acc =
      match fields with
      | [] -> List.rev acc
      | T.Func { typ; locals; instrs; _ } :: rest ->
          (* Build local context *)
          let locals_space =
            let num_unnamed_params =
              match typ with
              | Some type_idx, None -> (
                  let resolved_idx = resolve_idx ctx.types type_idx in
                  match B.IntMap.find_opt resolved_idx func_types_by_idx with
                  | Some num_params -> num_params
                  | None ->
                      failwith
                        (Printf.sprintf "Function type with index %d not found"
                           resolved_idx))
              | _ -> 0
            in
            let all_variables_for_mapping =
              match typ with
              | _, Some (named_params, _) -> named_params @ locals
              | _, None -> locals
            in
            List.fold_left
              (fun space (id, _) ->
                let space, _ = add_name space id in
                space)
              { empty_space with count = num_unnamed_params }
              all_variables_for_mapping
          in
          let func_ctx = { ctx with locals = locals_space } in

          (* Collect Local Names *)
          let local_map = invert_map locals_space.map in
          if not (B.IntMap.is_empty local_map) then
            locals_names := B.IntMap.add func_idx local_map !locals_names;

          (* Collect Label Names *)
          let label_map = collect_labels instrs (ref 0) B.IntMap.empty in
          if not (B.IntMap.is_empty label_map) then
            labels_names := B.IntMap.add func_idx label_map !labels_names;

          let b_locals = List.map (fun (_, v) -> valtype ctx v) locals in
          let converted_func =
            {
              B.locals = b_locals;
              instrs = List.map (instr ~resolve_type func_ctx) instrs;
            }
          in

          process_funcs func_types_by_idx rest (func_idx + 1)
            (converted_func :: acc)
      | _ :: rest -> process_funcs func_types_by_idx rest func_idx acc
    in
    process_funcs func_types_by_idx fields func_import_count []
  in

  let tables =
    List.filter_map
      (fun f ->
        match f with
        | T.Table { typ; init; _ } ->
            let expr =
              match init with
              | T.Init_expr e -> Some (List.map (instr ~resolve_type ctx) e)
              | _ -> None
            in
            Some { B.typ = table_type_fix ctx typ; B.expr }
        | _ -> None)
      fields
  in

  let memories =
    List.filter_map
      (fun f ->
        match f with T.Memory { limits; _ } -> Some limits | _ -> None)
      fields
  in

  let globals =
    List.filter_map
      (fun f ->
        match f with
        | T.Global { typ; init; _ } ->
            Some
              {
                B.typ = global_type ctx typ;
                B.init = List.map (instr ~resolve_type ctx) init;
              }
        | _ -> None)
      fields
  in

  let exports =
    List.filter_map
      (fun f ->
        match f with
        | T.Export { name; kind; index } ->
            let (kind : B.exportable), index =
              match kind with
              | Func -> (Func, resolve_idx ctx.funcs index)
              | Table -> (Table, resolve_idx ctx.tables index)
              | Memory -> (Memory, resolve_idx ctx.memories index)
              | Global -> (Global, resolve_idx ctx.globals index)
              | Tag -> (Tag, resolve_idx ctx.tags index)
            in
            Some { B.name; kind; index }
        | _ -> None)
      fields
  in

  let start =
    List.find_map
      (fun f ->
        match f with T.Start i -> Some (resolve_idx ctx.funcs i) | _ -> None)
      fields
  in

  let table_import_count =
    List.fold_left
      (fun acc f ->
        match f with T.Import { desc = T.Table _; _ } -> acc + 1 | _ -> acc)
      0 fields
  in

  let elem =
    let rec scan fields table_idx acc =
      match fields with
      | [] -> List.rev acc
      | T.Elem { typ; init; mode; _ } :: rest ->
          let mode : 'info B.elemmode =
            match mode with
            | Passive -> Passive
            | Active (i, ex) ->
                Active
                  ( resolve_idx ctx.tables i,
                    List.map (instr ~resolve_type ctx) ex )
            | Declare -> Declare
          in
          let e =
            {
              B.typ = reftype ctx typ;
              init = List.map (List.map (instr ~resolve_type ctx)) init;
              mode;
            }
          in
          scan rest table_idx (e :: acc)
      | T.Table { typ; init = T.Init_segment exprs; _ } :: rest ->
          let mode = B.Active (table_idx, [ Ast.no_loc (B.Const (B.I32 0l)) ]) in
          let e =
            {
              B.typ = reftype ctx typ.reftype;
              init = List.map (List.map (instr ~resolve_type ctx)) exprs;
              mode;
            }
          in
          scan rest (table_idx + 1) (e :: acc)
      | T.Table _ :: rest -> scan rest (table_idx + 1) acc
      | _ :: rest -> scan rest table_idx acc
    in
    scan fields table_import_count []
  in

  let memory_import_count =
    List.fold_left
      (fun acc f ->
        match f with T.Import { desc = T.Memory _; _ } -> acc + 1 | _ -> acc)
      0 fields
  in

  let data =
    let rec scan fields mem_idx acc =
      match fields with
      | [] -> List.rev acc
      | T.Data { init; mode; _ } :: rest ->
          let mode : 'info B.datamode =
            match mode with
            | Passive -> Passive
            | Active (i, ex) ->
                Active
                  ( resolve_idx ctx.memories i,
                    List.map (instr ~resolve_type ctx) ex )
          in
          let d = { B.init; mode } in
          scan rest mem_idx (d :: acc)
      | T.Memory { init = Some init; _ } :: rest ->
          let (mode : 'info B.datamode) =
            B.Active (mem_idx, [ Ast.no_loc (B.Const (B.I32 0l)) ])
          in
          let d = { B.init; mode } in
          scan rest (mem_idx + 1) (d :: acc)
      | T.Memory _ :: rest -> scan rest (mem_idx + 1) acc
      | _ :: rest -> scan rest mem_idx acc
    in
    scan fields memory_import_count []
  in

  let tags =
    List.filter_map
      (fun f ->
        match f with
        | T.Tag { typ = Some i, _; _ } -> Some (resolve_idx ctx.types i)
        | Tag { typ = None, Some (params, results); _ } ->
            let params =
              Array.of_list (List.map (fun (_, t) -> valtype ctx t) params)
            in
            let results = Array.of_list (List.map (valtype ctx) results) in
            Some (resolve_type { B.params; results })
        | Tag { typ = None, None; _ } ->
            failwith "Tag type must have an explicit type index or inline type"
        | _ -> None)
      fields
  in

  let types =
    explicit_types
    @ (List.rev !extra_types
      |> List.map (fun ft ->
          [| { B.typ = B.Func ft; supertype = None; final = true } |]))
  in

  {
    B.types;
    imports;
    functions;
    tables;
    memories;
    tags;
    globals;
    exports;
    start;
    elem;
    code;
    data;
    names =
      {
        B.module_ = module_name;
        functions = invert_map ctx.funcs.map;
        locals = !locals_names;
        types = invert_map ctx.types.map;
        fields = B.IntMap.map invert_map field_names;
        tags = invert_map ctx.tags.map;
        globals = invert_map ctx.globals.map;
        tables = invert_map ctx.tables.map;
        memories = invert_map ctx.memories.map;
        data = invert_map ctx.datas.map;
        elem = invert_map ctx.elems.map;
        labels = !labels_names;
      };
  }
