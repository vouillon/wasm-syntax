open Ast
module T = Text
module B = Binary
module StringMap = Map.Make (String)

type index_space = { map : B.idx StringMap.t; count : int }

let empty_space = { map = StringMap.empty; count = 0 }

let add_name (space : index_space) (id : string option) : index_space * B.idx =
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
    tags = empty_space;
    datas = empty_space;
    elems = empty_space;
    labels = [];
    locals = empty_space;
  }

let resolve_idx (space : index_space) (idx : T.idx) : B.idx =
  match idx.desc with
  | T.Num n -> Utils.Uint32.to_int n
  | T.Id id -> (
      match StringMap.find_opt id space.map with
      | Some i -> i
      | None -> failwith ("Unknown identifier: " ^ id))

let resolve_label (labels : string option list) (idx : T.idx) : B.idx =
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

let convert_heaptype ctx (h : T.heaptype) : B.heaptype =
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

let convert_reftype ctx (r : T.reftype) : B.reftype =
  { nullable = r.nullable; typ = convert_heaptype ctx r.typ }

let rec convert_valtype ctx (v : T.valtype) : B.valtype =
  match v with
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128
  | Ref r -> Ref (convert_reftype ctx r)
  | Tuple l -> Tuple (List.map (convert_valtype ctx) l)

let convert_storage_type ctx (s : T.storagetype) : B.storagetype =
  match s with Value v -> Value (convert_valtype ctx v) | Packed p -> Packed p

let convert_mut_type convert_typ_f ctx m =
  { mut = m.mut; typ = convert_typ_f ctx m.typ }

let convert_field_type ctx f = convert_mut_type convert_storage_type ctx f

let convert_func_type ctx (f : T.functype) : B.functype =
  {
    params = Array.map (convert_valtype ctx) f.params;
    results = Array.map (convert_valtype ctx) f.results;
  }

let convert_comp_type ctx (c : T.comptype) : B.comptype =
  match c with
  | Func f -> Func (convert_func_type ctx f)
  | Struct f -> Struct (Array.map (fun (_, ft) -> convert_field_type ctx ft) f)
  | Array f -> Array (convert_field_type ctx f)

let convert_sub_type ctx (s : T.subtype) : B.subtype =
  {
    typ = convert_comp_type ctx s.typ;
    supertype = Option.map (resolve_idx ctx.types) s.supertype;
    final = s.final;
  }

let convert_rec_type ctx r = Array.map (fun (_, s) -> convert_sub_type ctx s) r
let convert_global_type ctx g = convert_mut_type convert_valtype ctx g

let convert_table_type_fix ctx (t : T.tabletype) : B.tabletype =
  { limits = t.limits; reftype = convert_reftype ctx t.reftype }

let convert_block_type ~resolve_type ctx (b : T.blocktype) : B.blocktype =
  match b with
  | Typeuse (Some i, _) -> Typeuse (resolve_idx ctx.types i)
  | Typeuse (None, Some ft) -> Typeuse (resolve_type (convert_func_type ctx ft))
  | Typeuse (None, None) ->
      failwith "Inline block types not supported yet (need index)"
  | Valtype v -> Valtype (convert_valtype ctx v)

let convert_catch ctx (c : T.catch) : B.catch =
  match c with
  | Catch (tag, label) ->
      Catch (resolve_idx ctx.tags tag, resolve_label ctx.labels label)
  | CatchRef (tag, label) ->
      CatchRef (resolve_idx ctx.tags tag, resolve_label ctx.labels label)
  | CatchAll label -> CatchAll (resolve_label ctx.labels label)
  | CatchAllRef label -> CatchAllRef (resolve_label ctx.labels label)

let rec convert_instr ~resolve_type ctx (i : 'info T.instr) =
  let recurse = convert_instr ~resolve_type in
  let desc : _ B.instr_desc =
    match i.desc with
    | Block { label; typ; block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        Block
          {
            label = ();
            typ = Option.map (convert_block_type ~resolve_type ctx) typ;
            block = List.map (recurse ctx') block;
          }
    | Loop { label; typ; block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        Loop
          {
            label = ();
            typ = Option.map (convert_block_type ~resolve_type ctx) typ;
            block = List.map (recurse ctx') block;
          }
    | If { label; typ; if_block; else_block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        If
          {
            label = ();
            typ = Option.map (convert_block_type ~resolve_type ctx) typ;
            if_block = List.map (recurse ctx') if_block;
            else_block = List.map (recurse ctx') else_block;
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
    | CallIndirect (table, (idx_opt, type_opt)) ->
        let type_idx =
          match idx_opt with
          | Some i -> resolve_idx ctx.types i
          | None -> (
              match type_opt with
              | Some ft -> resolve_type (convert_func_type ctx ft)
              | None -> failwith "CallIndirect requires type")
        in
        CallIndirect (resolve_idx ctx.tables table, type_idx)
    | Drop -> Drop
    | Select None -> Select None
    | Select (Some l) -> Select (Some (List.map (convert_valtype ctx) l))
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
    | Const (I32 x) -> Const (I32 (Int32.of_string x))
    | Const (I64 x) -> Const (I64 (Int64.of_string x))
    | Const (F32 x) -> Const (F32 (float_of_string x))
    | Const (F64 x) -> Const (F64 (float_of_string x))
    | UnOp op -> UnOp op
    | BinOp op -> BinOp op
    | RefNull t -> RefNull (convert_heaptype ctx t)
    | RefFunc i -> RefFunc (resolve_idx ctx.funcs i)
    | RefIsNull -> RefIsNull
    | TryTable { label; typ; catches; block } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        TryTable
          {
            label = ();
            typ = Option.map (convert_block_type ~resolve_type ctx) typ;
            catches = List.map (convert_catch ctx) catches;
            block = List.map (recurse ctx') block;
          }
    | Try { label; typ; block; catches; catch_all } ->
        let ctx' = { ctx with labels = label :: ctx.labels } in
        Try
          {
            label = ();
            typ = Option.map (convert_block_type ~resolve_type ctx) typ;
            block = List.map (recurse ctx') block;
            catches =
              List.map
                (fun (tag, b) ->
                  (resolve_idx ctx.tags tag, List.map (recurse ctx') b))
                catches;
            catch_all = Option.map (List.map (recurse ctx')) catch_all;
          }
    | _ -> Nop (* Placeholder for unimplemented *)
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

let module_ (m : 'info T.module_) : 'info B.module_ =
  let module_name, fields = m in

  (* Pass 1: Build Context *)
  let ctx = empty_context in

  let scan_types ctx fields =
    List.fold_left
      (fun ctx f ->
        match f with
        | T.Types r ->
            let types_space, _ =
              Array.fold_left
                (fun (space, _) (id, _) -> add_name space id)
                (ctx.types, 0) r
            in
            { ctx with types = types_space }
        | _ -> ctx)
      ctx fields
  in
  let ctx = scan_types ctx fields in

  let scan_imports ctx fields =
    List.fold_left
      (fun ctx f ->
        match f with
        | T.Import { id; desc; _ } -> (
            match desc with
            | T.Func _ -> { ctx with funcs = fst (add_name ctx.funcs id) }
            | T.Table _ -> { ctx with tables = fst (add_name ctx.tables id) }
            | T.Memory _ ->
                { ctx with memories = fst (add_name ctx.memories id) }
            | T.Global _ -> { ctx with globals = fst (add_name ctx.globals id) }
            | T.Tag _ -> { ctx with tags = fst (add_name ctx.tags id) })
        | _ -> ctx)
      ctx fields
  in
  let ctx = scan_imports ctx fields in

  let scan_defs ctx fields =
    List.fold_left
      (fun ctx f ->
        match f with
        | T.Func { id; _ } -> { ctx with funcs = fst (add_name ctx.funcs id) }
        | T.Table { id; _ } ->
            { ctx with tables = fst (add_name ctx.tables id) }
        | T.Memory { id; _ } ->
            { ctx with memories = fst (add_name ctx.memories id) }
        | T.Global { id; _ } ->
            { ctx with globals = fst (add_name ctx.globals id) }
        | T.Tag { id; _ } -> { ctx with tags = fst (add_name ctx.tags id) }
        | T.Elem { id; _ } -> { ctx with elems = fst (add_name ctx.elems id) }
        | T.Data { id; _ } -> { ctx with datas = fst (add_name ctx.datas id) }
        | _ -> ctx)
      ctx fields
  in
  let ctx = scan_defs ctx fields in

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
                          | Some n -> (B.IntMap.add fidx n fmap, fidx + 1)
                          | None -> (fmap, fidx + 1))
                        (B.IntMap.empty, 0) field_defs
                      |> fst
                    in
                    if B.IntMap.is_empty field_map then (acc, i + 1)
                    else (B.IntMap.add (type_idx + i) field_map acc, i + 1)
                | _ -> (acc, i + 1))
              (acc, 0) r
          in
          scan_fields (type_idx + Array.length r) rest acc
      | _ :: rest -> scan_fields type_idx rest acc
    in
    scan_fields 0 fields B.IntMap.empty
  in

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
                  let b_f = convert_func_type ctx f in
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
            let desc =
              match desc with
              | T.Func (Some i, _) -> B.Func (resolve_idx ctx.types i)
              | T.Func (None, Some (params, results)) ->
                  (* Inline type in Import *)
                  let params =
                    Array.of_list
                      (List.map (fun (_, t) -> convert_valtype ctx t) params)
                  in
                  let results =
                    Array.of_list (List.map (convert_valtype ctx) results)
                  in
                  B.Func (resolve_type { B.params; results })
              | T.Func (None, None) ->
                  failwith "Func import missing type"
              | T.Table t -> B.Table (convert_table_type_fix ctx t)
              | T.Memory l -> B.Memory l
              | T.Global g -> B.Global (convert_global_type ctx g)
              | T.Tag (Some i, _) -> B.Tag (resolve_idx ctx.types i)
              | T.Tag (None, Some (params, results)) ->
                  let params =
                    Array.of_list
                      (List.map (fun (_, t) -> convert_valtype ctx t) params)
                  in
                  let results =
                    Array.of_list (List.map (convert_valtype ctx) results)
                  in
                  B.Tag (resolve_type { B.params; results })
              | T.Tag (None, None) -> failwith "Tag import missing type"
            in
            Some { B.module_; name; desc }
        | _ -> None)
      fields
  in

  let explicit_types =
    List.filter_map
      (fun f ->
        match f with T.Types r -> Some (convert_rec_type ctx r) | _ -> None)
      fields
  in

  let functions =
    List.filter_map
      (fun f ->
        match f with
        | T.Func { typ = Some i, _; _ } -> Some (resolve_idx ctx.types i)
        | T.Func { typ = None, Some (params, results); _ } ->
            let params =
              Array.of_list
                (List.map (fun (_, t) -> convert_valtype ctx t) params)
            in
            let results =
              Array.of_list (List.map (convert_valtype ctx) results)
            in
            Some (resolve_type { B.params; results })
        | T.Func { typ = None, None; _ } ->
             failwith "Func missing type"
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
    let rec process_funcs fields func_idx acc =
      match fields with
      | [] -> List.rev acc
      | T.Func { locals; instrs; _ } :: rest ->
          (* Collect Local Names *)
          let local_map =
            List.fold_left
              (fun (map, idx) (id, _) ->
                match id with
                | Some n -> (B.IntMap.add idx n map, idx + 1)
                | None -> (map, idx + 1))
              (B.IntMap.empty, 0) locals
            |> fst
          in
          if not (B.IntMap.is_empty local_map) then
            locals_names := B.IntMap.add func_idx local_map !locals_names;

          (* Collect Label Names *)
          let label_map = collect_labels instrs (ref 0) B.IntMap.empty in
          if not (B.IntMap.is_empty label_map) then
            labels_names := B.IntMap.add func_idx label_map !labels_names;

          (* Build local context *)
          let locals_space, _ =
            List.fold_left
              (fun (space, idx) (id, _) ->
                let space, _ = add_name space id in
                (space, idx + 1))
              (empty_space, 0) locals
          in
          let func_ctx = { ctx with locals = locals_space } in
          let b_locals =
            List.map (fun (_, v) -> convert_valtype ctx v) locals
          in
          let converted_func =
            {
              B.locals = b_locals;
              instrs = List.map (convert_instr ~resolve_type func_ctx) instrs;
            }
          in

          process_funcs rest (func_idx + 1) (converted_func :: acc)
      | _ :: rest -> process_funcs rest func_idx acc
    in
    process_funcs fields func_import_count []
  in

  let tables =
    List.filter_map
      (fun f ->
        match f with
        | T.Table { typ; _ } -> Some { B.typ = typ.limits; B.expr = None }
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
                B.typ = convert_global_type ctx typ;
                B.init = List.map (convert_instr ~resolve_type ctx) init;
              }
        | _ -> None)
      fields
  in

  let exports =
    List.filter_map
      (fun f ->
        match f with
        | T.Export { name; kind; index } ->
            let kind, index =
              match kind with
              | T.Func -> ((B.Func : B.exportable), resolve_idx ctx.funcs index)
              | T.Table ->
                  ((B.Table : B.exportable), resolve_idx ctx.tables index)
              | T.Memory ->
                  ((B.Memory : B.exportable), resolve_idx ctx.memories index)
              | T.Global ->
                  ((B.Global : B.exportable), resolve_idx ctx.globals index)
              | T.Tag -> ((B.Tag : B.exportable), resolve_idx ctx.tags index)
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

  let elem =
    List.filter_map
      (fun f ->
        match f with
        | T.Elem { typ; init; mode; _ } ->
            let mode : 'info B.elemmode =
              match mode with
              | Passive -> Passive
              | Active (i, ex) ->
                  Active
                    (resolve_idx ctx.tables i, List.map (convert_instr ~resolve_type ctx) ex)
              | Declare -> Declare
            in
            Some
              {
                B.typ = convert_reftype ctx typ;
                init = List.map (List.map (convert_instr ~resolve_type ctx)) init;
                mode;
              }
        | _ -> None)
      fields
  in

  let data =
    List.filter_map
      (fun f ->
        match f with
        | T.Data { init; mode; _ } ->
            let mode : 'info B.datamode =
              match mode with
              | Passive -> Passive
              | Active (i, ex) ->
                  Active
                    (resolve_idx ctx.memories i, List.map (convert_instr ~resolve_type ctx) ex)
            in
            Some { B.init; mode }
        | _ -> None)
      fields
  in

  let tags =
    List.filter_map
      (fun f ->
        match f with
        | T.Tag { typ = Some i, _; _ } -> Some (resolve_idx ctx.types i)
        | Tag { typ = None, Some (params, results); _ } ->
             let params =
              Array.of_list
                (List.map (fun (_, t) -> convert_valtype ctx t) params)
            in
            let results =
              Array.of_list (List.map (convert_valtype ctx) results)
            in
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

  let invert_map map =
    StringMap.fold (fun k v acc -> B.IntMap.add v k acc) map B.IntMap.empty
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
        fields = field_names;
        tags = invert_map ctx.tags.map;
        globals = invert_map ctx.globals.map;
        tables = invert_map ctx.tables.map;
        memories = invert_map ctx.memories.map;
        data = invert_map ctx.datas.map;
        elem = invert_map ctx.elems.map;
        labels = !labels_names;
      };
  }