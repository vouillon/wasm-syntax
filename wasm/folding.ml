open Ast.Text

let map_instrs func (name, fields) =
  ( name,
    List.map
      (fun f ->
        match f with
        | Func ({ typ; locals; instrs; _ } as f) ->
            Func { f with instrs = func (Some (typ, locals)) instrs }
        | Global ({ init; _ } as g) -> Global { g with init = func None init }
        | Table ({ init; _ } as t) ->
            Table
              {
                t with
                init =
                  (match init with
                  | Init_default -> Init_default
                  | Init_expr init -> Init_expr (func None init)
                  | Init_segment seg ->
                      Init_segment (List.map (fun e -> func None e) seg));
              }
        | Elem ({ init; _ } as e) ->
            Elem { e with init = List.map (fun l -> func None l) init }
        | Types _ | Import _ | Memory _ | Tag _ | Export _ | Start _ | Data _ ->
            f)
      fields )

(****)

module Uint32Map = Map.Make (Uint32)
module StringMap = Map.Make (String)

module Tbl = struct
  type 'a t = {
    by_index : 'a Uint32Map.t;
    by_name : 'a StringMap.t;
    last : int;
  }

  let empty =
    { by_index = Uint32Map.empty; by_name = StringMap.empty; last = 0 }

  let add id v tbl =
    {
      by_index = Uint32Map.add (Uint32.of_int tbl.last) v tbl.by_index;
      by_name =
        (match id with
        | None -> tbl.by_name
        | Some id -> StringMap.add id v tbl.by_name);
      last = tbl.last + 1;
    }
end

let lookup (tbl : _ Tbl.t) idx =
  try
    match idx.Ast.desc with
    | Num i -> Uint32Map.find i tbl.by_index
    | Id i -> StringMap.find i tbl.by_name
  with Not_found -> assert false (*ZZZ *)

type outer_env = {
  types : subtype Tbl.t;
  functions : typeuse Tbl.t;
  globals : globaltype Tbl.t;
  locals : valtype Tbl.t;
}

let types m =
  List.fold_left
    (fun tbl f ->
      match f with
      | Types l ->
          Array.fold_left (fun tbl (id, typ) -> Tbl.add id typ tbl) tbl l
      | Import _ | Func _ | Memory _ | Table _ | Tag _ | Global _ | Export _
      | Start _ | Elem _ | Data _ ->
          tbl)
    Tbl.empty m

let functions f =
  List.fold_left
    (fun tbl f ->
      match f with
      | Func { id; typ; _ } | Import { id; desc = Func typ; _ } ->
          Tbl.add id typ tbl
      | Import { desc = Memory _ | Table _ | Global _ | Tag _; _ }
      | Types _ | Memory _ | Table _ | Tag _ | Global _ | Export _ | Start _
      | Elem _ | Data _ ->
          tbl)
    Tbl.empty f

let globals f =
  List.fold_left
    (fun tbl f ->
      match f with
      | Global { id; typ; _ } | Import { id; desc = Global typ; _ } ->
          Tbl.add id typ tbl
      | Import { desc = Func _ | Memory _ | Table _ | Tag _; _ }
      | Types _ | Func _ | Memory _ | Table _ | Tag _ | Export _ | Start _
      | Elem _ | Data _ ->
          tbl)
    Tbl.empty f

let locals env typ l =
  let tbl =
    match typ with
    | _, Some (params, _) ->
        List.fold_left
          (fun tbl (id, typ) -> Tbl.add id typ tbl)
          Tbl.empty params
    | Some ty, None -> (
        match (lookup env.types ty).typ with
        | Func ty ->
            Array.fold_left
              (fun tbl typ -> Tbl.add None typ tbl)
              Tbl.empty ty.params
        | Struct _ | Array _ -> assert false (*ZZZ*))
    | None, None -> assert false
  in
  List.fold_left (fun tbl (id, typ) -> Tbl.add id typ tbl) tbl l

let module_env (_, m) =
  {
    types = types m;
    functions = functions m;
    globals = globals m;
    locals = Tbl.empty;
  }

(****)

type env = {
  outer_env : outer_env;
  labels : (id option * int) list;
  return_arity : int;
}

let lookup_type env idx = lookup env.outer_env.types idx

let functype_no_bindings_arity { params; results } =
  (Array.length params, Array.length results)

let functype_arity (params, results) = (List.length params, List.length results)

let type_arity env idx =
  match (lookup_type env idx).typ with
  | Func ty -> functype_no_bindings_arity ty
  | Struct _ | Array _ -> assert false (*ZZZ*)

let typeuse_no_bindings_arity env (i, ty) =
  match (i, ty) with
  | _, Some t -> functype_no_bindings_arity t
  | Some i, None -> type_arity env i
  | None, None -> assert false

let typeuse_arity env (i, ty) =
  match (i, ty) with
  | _, Some t -> functype_arity t
  | Some i, None -> type_arity env i
  | None, None -> assert false

let blocktype_arity env t =
  match t with
  | None -> (0, 0)
  | Some (Valtype _) -> (0, 1)
  | Some (Typeuse t) -> typeuse_no_bindings_arity env t

let function_arity env f = typeuse_arity env (lookup env.outer_env.functions f)
let valtype_arity t = match t with Tuple l -> List.length l | _ -> 1
let globaltype_arity (t : globaltype) = valtype_arity t.typ
let global_arity env g = globaltype_arity (lookup env.outer_env.globals g)
let local_arity env l = valtype_arity (lookup env.outer_env.locals l)
let unreachable = 100_000

let label_arity env idx =
  match idx.Ast.desc with
  | Id id ->
      snd
        (List.find
           (fun e -> match e with Some id', _ -> id = id' | _ -> false)
           env.labels)
  | Num i -> snd (List.nth env.labels (Uint32.to_int i))

let arity env i =
  match i.Ast.desc with
  | Block { typ; _ } | Loop { typ; _ } | Try { typ; _ } | TryTable { typ; _ } ->
      blocktype_arity env typ
  | If { typ; _ } ->
      let i, o = blocktype_arity env typ in
      (i + 1, o)
  | Call f -> function_arity env f
  | ReturnCall f ->
      let i, _ = function_arity env f in
      (i, unreachable)
  | CallRef t ->
      let i, o = type_arity env t in
      (i + 1, o)
  | ReturnCallRef t ->
      let i, _ = type_arity env t in
      (i + 1, unreachable)
  | Br l ->
      let i = label_arity env l in
      (i, unreachable)
  | Br_if l ->
      let i = label_arity env l in
      (i + 1, i)
  | Br_table (_, l) ->
      let i = label_arity env l in
      (i + 1, unreachable)
  | Br_on_null l ->
      let i = label_arity env l in
      (i + 1, i + 1)
  | Br_on_non_null l ->
      let i = label_arity env l in
      (i + 1, i)
  | Br_on_cast (l, _, _) | Br_on_cast_fail (l, _, _) ->
      let i = label_arity env l in
      (i + 1, i + 1)
  | Return -> (env.return_arity, unreachable)
  | ReturnCallIndirect (_, ty) ->
      let i, _ = typeuse_no_bindings_arity env ty in
      (i + 1, unreachable)
  | CallIndirect (_, ty) ->
      let i, o = typeuse_no_bindings_arity env ty in
      (i + 1, o)
  | Unreachable -> (0, unreachable)
  | Nop -> (0, 0)
  | Throw _ -> (1, unreachable)
  | Drop -> (1, 0)
  | Select _ -> (3, 1)
  | LocalGet l -> (0, local_arity env l)
  | LocalSet l -> (local_arity env l, 0)
  | LocalTee l ->
      let i = local_arity env l in
      (i, i)
  | GlobalGet g -> (0, global_arity env g)
  | GlobalSet g -> (global_arity env g, 0)
  | Load _ | LoadS _ | Store _ | StoreS _ -> (1, 1)
  | MemorySize _ -> (0, 1)
  | MemoryGrow _ -> (1, 1)
  | MemoryFill _ | MemoryCopy _ | MemoryInit _ -> (3, 0)
  | DataDrop _ -> (0, 0)
  | TableGet _ -> (1, 1)
  | TableSet _ -> (2, 0)
  | TableSize _ -> (0, 1)
  | TableGrow _ -> (2, 1)
  | TableFill _ | TableCopy _ | TableInit _ -> (3, 0)
  | ElemDrop _ -> (0, 0)
  | RefNull _ -> (0, 1)
  | RefFunc _ -> (0, 1)
  | RefIsNull -> (1, 1)
  | RefAsNonNull -> (1, 1)
  | RefEq -> (2, 1)
  | RefTest _ -> (1, 1)
  | RefCast _ -> (1, 1)
  | StructNew t -> (
      match (lookup_type env t).typ with
      | Struct f -> (Array.length f, 1)
      | Func _ | Array _ -> assert false (*ZZZ*))
  | StructNewDefault _ -> (0, 1)
  | StructGet _ -> (1, 1)
  | StructSet _ -> (2, 0)
  | ArrayNew _ -> (2, 1)
  | ArrayNewDefault _ -> (1, 1)
  | ArrayNewFixed (_, n) -> (Uint32.to_int n, 1)
  | ArrayNewData _ -> (2, 1)
  | ArrayNewElem _ -> (2, 1)
  | ArrayGet _ -> (2, 1)
  | ArraySet _ -> (3, 0)
  | ArrayLen -> (1, 1)
  | ArrayFill _ -> (4, 0)
  | ArrayCopy _ -> (5, 0)
  | ArrayInitData _ -> (4, 0)
  | ArrayInitElem _ -> (4, 0)
  | RefI31 -> (1, 1)
  | I31Get _ -> (1, 1)
  | Const _ -> (0, 1)
  | UnOp _ -> (1, 1)
  | BinOp _ -> (2, 1)
  | I32WrapI64 -> (1, 1)
  | I64ExtendI32 _ -> (1, 1)
  | F32DemoteF64 -> (1, 1)
  | F64PromoteF32 -> (1, 1)
  | ExternConvertAny -> (1, 1)
  | AnyConvertExtern -> (1, 1)
  | Folded _ -> assert false
  (* Binaryen extensions *)
  | Pop _ -> (0, 1)
  | TupleMake n -> (Uint32.to_int n, Uint32.to_int n)
  | TupleExtract (n, _) -> (Uint32.to_int n, 1)

(****)

let push_back tentative_args stream =
  List.rev_append (List.map (fun i -> (0, i)) tentative_args) stream

let rec consume n folded =
  if n = 0 then folded
  else
    match folded with
    | [] -> []
    | (n', i) :: rem ->
        if n >= n' then (0, i) :: consume (n - n') folded
        else (n' - n, i) :: rem

let rec fold_stream env folded stream : _ Ast.Text.instr list =
  match stream with
  | [] -> List.rev (List.map snd folded)
  | ({ Ast.desc = Block ({ label; typ; block; _ } as b); _ } as i) :: rem ->
      let block =
        let _, i = blocktype_arity env typ in
        let env = { env with labels = (label, i) :: env.labels } in
        fold_stream env [] block
      in
      let inputs, outputs = arity env i in
      let folded = consume inputs folded in
      fold_stream env
        (( outputs,
           {
             i with
             desc = Folded ({ i with desc = Block { b with block } }, []);
           } )
        :: folded)
        rem
  | ({ Ast.desc = Loop ({ label; typ; block; _ } as b); _ } as i) :: rem ->
      let block =
        let i, _ = blocktype_arity env typ in
        let env = { env with labels = (label, i) :: env.labels } in
        fold_stream env [] block
      in
      let inputs, outputs = arity env i in
      let folded = consume inputs folded in
      fold_stream env
        (( outputs,
           {
             i with
             desc = Folded ({ i with desc = Loop { b with block } }, []);
           } )
        :: folded)
        rem
  | ({ Ast.desc = If ({ label; typ; if_block; else_block; _ } as b); _ } as i)
    :: rem ->
      let env' =
        let i, _ = blocktype_arity env typ in
        { env with labels = (label, i) :: env.labels }
      in
      let if_block = fold_stream env' [] if_block in
      let else_block = fold_stream env' [] else_block in
      let inputs, outputs = arity env i in
      fold_instr env folded [] [] rem
        { i with desc = If { b with if_block; else_block } }
        inputs outputs
  | ({ Ast.desc = TryTable ({ label; typ; block; _ } as b); _ } as i) :: rem ->
      let block =
        let i, _ = blocktype_arity env typ in
        let env = { env with labels = (label, i) :: env.labels } in
        fold_stream env [] block
      in
      let inputs, outputs = arity env i in
      let folded = consume inputs folded in
      fold_stream env
        (( outputs,
           {
             i with
             desc = Folded ({ i with desc = TryTable { b with block } }, []);
           } )
        :: folded)
        rem
  | ({ Ast.desc = Try ({ label; typ; block; catches; catch_all; _ } as b); _ }
     as i)
    :: rem ->
      let env' =
        let i, _ = blocktype_arity env typ in
        { env with labels = (label, i) :: env.labels }
      in
      let block = fold_stream env' [] block in
      let catches =
        List.map (fun (i, l) -> (i, fold_stream env' [] l)) catches
      in
      let catch_all = Option.map (fold_stream env' []) catch_all in
      let inputs, outputs = arity env i in
      let folded = consume inputs folded in
      fold_stream env
        (( outputs,
           {
             i with
             desc =
               Folded
                 ({ i with desc = Try { b with block; catches; catch_all } }, []);
           } )
        :: folded)
        rem
  | { Ast.desc = Folded (i, l); _ } :: rem ->
      fold_stream env folded (l @ (i :: rem))
  | i :: rem ->
      let inputs, outputs = arity env i in
      fold_instr env folded [] [] rem i inputs outputs

and fold_instr env folded args tentative_args stream i inputs outputs =
  if inputs = 0 then
    fold_stream env
      ((outputs, { i with desc = Folded (i, args) })
      :: push_back tentative_args folded)
      stream
  else
    match folded with
    | [] ->
        fold_stream env
          ((outputs, { i with desc = Folded (i, args) })
          :: push_back tentative_args folded)
          stream
    | (n, i') :: folded' ->
        if n <= inputs then
          if n > 0 then
            let args = (i' :: tentative_args) @ args in
            fold_instr env folded' args [] stream i (inputs - n) outputs
          else
            let tentative_args = i' :: tentative_args in
            fold_instr env folded' args tentative_args stream i inputs outputs
        else
          fold_stream env
            ((outputs, { i with desc = Folded (i, args) })
            :: push_back tentative_args ((n - inputs, i') :: folded'))
            stream

let fold m =
  let env = { outer_env = module_env m; labels = []; return_arity = 0 } in
  map_instrs
    (fun typ str ->
      let env =
        match typ with
        | None -> env
        | Some (ty, l) ->
            let _, i = typeuse_arity env ty in
            {
              outer_env =
                { env.outer_env with locals = locals env.outer_env ty l };
              labels = [ (None, i) ];
              return_arity = i;
            }
      in
      fold_stream env [] str)
    m

(****)

let rec unfold_stream stream start =
  List.fold_left
    (fun start i ->
      let unfold_block i =
        match i.Ast.desc with
        | Block ({ block; _ } as b) ->
            Block { b with block = unfold_instrs block }
        | Loop ({ block; _ } as b) ->
            Loop { b with block = unfold_instrs block }
        | If ({ if_block; else_block; _ } as b) ->
            If
              {
                b with
                if_block = unfold_instrs if_block;
                else_block = unfold_instrs else_block;
              }
        | TryTable ({ block; _ } as b) ->
            TryTable { b with block = unfold_instrs block }
        | Try ({ block; catches; catch_all; _ } as b) ->
            Try
              {
                b with
                block = unfold_instrs block;
                catches = List.map (fun (i, l) -> (i, unfold_instrs l)) catches;
                catch_all = Option.map unfold_instrs catch_all;
              }
        | Folded _ -> assert false
        | _ -> i.desc
      in
      match i.Ast.desc with
      | Folded (i, l) ->
          { i with desc = unfold_block i } :: unfold_stream l start
      | _ -> { i with desc = unfold_block i } :: start)
    start stream

and unfold_instrs l = List.rev (unfold_stream l [])

let unfold m = map_instrs (fun _ str -> unfold_instrs str) m
