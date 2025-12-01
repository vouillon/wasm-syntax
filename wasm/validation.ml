(*
ZZZ
- tag validation
- resolve all type uses
- uninitialized locals
- name parameters in function types
*)

(* Check that all occurences of instructions ref.func uses a function
   index that occurs in the module outside functions *)
let validate_refs = ref true

module Uint32 = Utils.Uint32
module Uint64 = Utils.Uint64

let print_instr f i = Utils.Printer.run f (fun p -> Output.instr p i)

open Ast.Binary.Types

module Sequence = struct
  type 'a t = {
    name : string;
    index_mapping : (Uint32.t, 'a) Hashtbl.t;
    label_mapping : (string, 'a) Hashtbl.t;
    mutable last_index : int;
  }

  let make name =
    {
      name;
      index_mapping = Hashtbl.create 16;
      label_mapping = Hashtbl.create 16;
      last_index = 0;
    }

  let register seq id v =
    let idx = Uint32.of_int seq.last_index in
    seq.last_index <- seq.last_index + 1;
    Hashtbl.add seq.index_mapping idx v;
    Option.iter (fun id -> (*ZZZ*) Hashtbl.add seq.label_mapping id v) id

  let get seq (idx : Ast.Text.idx) =
    match idx.desc with
    | Num n -> (
        try Hashtbl.find seq.index_mapping n
        with Not_found ->
          Format.eprintf "Unbound %s %s@." seq.name (Uint32.to_string n);
          exit 1)
    | Id id -> (
        try Hashtbl.find seq.label_mapping id
        with Not_found ->
          Format.eprintf "Unbound %s $%s@." seq.name id;
          exit 1)
end

type type_context = {
  types : Types.t;
  mutable last_index : int;
  index_mapping : (Uint32.t, int * (string * int) list) Hashtbl.t;
  label_mapping : (string, int * (string * int) list) Hashtbl.t;
}

(*ZZZ unbound?*)
let get_type_info ctx (idx : Ast.Text.idx) =
  match idx.desc with
  | Num x -> Hashtbl.find ctx.index_mapping x
  | Id id -> Hashtbl.find ctx.label_mapping id

let resolve_type_index ctx idx = fst (get_type_info ctx idx)

let heaptype ctx (h : Ast.Text.heaptype) : heaptype =
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
  | Type idx -> Type (resolve_type_index ctx idx)

let reftype ctx { Ast.Text.nullable; typ } =
  { nullable; typ = heaptype ctx typ }

let rec valtype ctx (ty : Ast.Text.valtype) =
  match ty with
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128
  | Ref r -> Ref (reftype ctx r)
  | Tuple l -> Tuple (List.map (valtype ctx) l)

let functype ctx { Ast.Text.params; results } =
  {
    params = Array.map (fun ty -> valtype ctx ty) params;
    results = Array.map (fun ty -> valtype ctx ty) results;
  }

let storagetype ctx (ty : Ast.Text.storagetype) =
  match ty with Value ty -> Value (valtype ctx ty) | Packed ty -> Packed ty

let muttype f ctx { mut; typ } = { mut; typ = f ctx typ }
let fieldtype ctx ty = muttype storagetype ctx ty

let tabletype ctx ({ limits; reftype = typ } : Ast.Text.tabletype) =
  { limits; reftype = reftype ctx typ }

let globaltype ctx ty = muttype valtype ctx ty

let comptype ctx (ty : Ast.Text.comptype) =
  match ty with
  | Func ty -> Func (functype ctx ty)
  | Struct fields ->
      (*ZZZ Check unique names*)
      Struct (Array.map (fun (_, ty) -> fieldtype ctx ty) fields)
  | Array field -> Array (fieldtype ctx field)

let subtype ctx { Ast.Text.typ; supertype; final } =
  {
    typ = comptype ctx typ;
    supertype = Option.map (fun ty -> resolve_type_index ctx ty) supertype;
    final;
  }

let rectype ctx ty = Array.map (fun (_, ty) -> subtype ctx ty) ty

let signature ctx (params, results) =
  {
    params = Array.of_list (List.map (fun (_, ty) -> valtype ctx ty) params);
    results = Array.of_list (List.map (valtype ctx) results);
  }

let typeuse ctx (idx, sign) =
  match (idx, sign) with
  | Some idx, _ ->
      (*ZZZ Validate signature *)
      resolve_type_index ctx idx
  | _, Some sign ->
      Types.add_rectype ctx.types
        [|
          { typ = Func (signature ctx sign); supertype = None; final = true };
        |]
  | None, None -> assert false

let typeuse' ctx (idx, sign) =
  match (idx, sign) with
  | Some idx, _ ->
      (*ZZZ Validate signature *)
      resolve_type_index ctx idx
  | _, Some sign ->
      Types.add_rectype ctx.types
        [| { typ = Func (functype ctx sign); supertype = None; final = true } |]
  | None, None -> assert false

type module_context = {
  types : type_context;
  subtyping_info : Types.subtyping_info;
  functions : int Sequence.t;
  memories : limits Sequence.t;
  tables : tabletype Sequence.t;
  globals : globaltype Sequence.t;
  tags : int Sequence.t;
  data : unit Sequence.t;
  elem : reftype Sequence.t;
  exports : (string, unit) Hashtbl.t;
  refs : (int, unit) Hashtbl.t;
}

type ctx = {
  locals : valtype Sequence.t;
  control_types : (string option * valtype list) list;
  return_types : valtype list;
  modul : module_context;
}

type stack = Unreachable | Empty | Cons of valtype option * stack

let print_heaptype f (ty : heaptype) =
  match ty with
  | Func -> Format.fprintf f "func"
  | NoFunc -> Format.fprintf f "nofunc"
  | Exn -> Format.fprintf f "exn"
  | NoExn -> Format.fprintf f "noexn"
  | Extern -> Format.fprintf f "extern"
  | NoExtern -> Format.fprintf f "noextern"
  | Any -> Format.fprintf f "any"
  | Eq -> Format.fprintf f "eq"
  | I31 -> Format.fprintf f "i31"
  | Struct -> Format.fprintf f "struct"
  | Array -> Format.fprintf f "array"
  | None_ -> Format.fprintf f "none"
  | Type id -> Format.fprintf f "%d" id

let print_valtype f ty =
  match ty with
  | I32 -> Format.fprintf f "i32"
  | I64 -> Format.fprintf f "i64"
  | F32 -> Format.fprintf f "f32"
  | F64 -> Format.fprintf f "f64"
  | V128 -> Format.fprintf f "v128"
  | Ref { nullable; typ } ->
      if nullable then
        Format.fprintf f "@[<1>(ref@ null@ %a)@]" print_heaptype typ
      else Format.fprintf f "@[<1>(ref@ %a)@]" print_heaptype typ
  | Tuple _ -> assert false

let pop_any st =
  match st with
  | Unreachable -> (Unreachable, None)
  | Cons (ty, r) -> (r, ty)
  | Empty -> assert false

let pop ctx ty st =
  match st with
  | Unreachable -> (Unreachable, ())
  | Cons (None, r) -> (r, ())
  | Cons (Some ty', r) ->
      let ok = Types.val_subtype ctx.modul.subtyping_info ty' ty in
      if not ok then
        Format.eprintf "%a <: %a@." print_valtype ty' print_valtype ty;
      assert ok;
      (r, ())
  | Empty -> assert false

let push_poly ty st = (Cons (ty, st), ())
let push ty st = (Cons (Some ty, st), ())
let unreachable _ = (Unreachable, ())
let return v st = (st, v)

let ( let* ) e f st =
  let st, v = e st in
  f v st

let get_local ctx ?(initialize = false) i =
  ignore initialize;
  (*ZZZ Check in bound + initialized*)
  Sequence.get ctx.locals i

let is_nullable ty =
  match ty with
  | None -> true
  | Some (Ref { nullable; _ }) -> nullable
  | _ -> assert false

let number_or_vec ty =
  match ty with
  | I32 | I64 | F32 | F64 | V128 -> true
  | Ref _ | Tuple _ -> false

let int_un_op_type ty (op : Ast.Text.int_un_op) =
  match op with
  | Clz | Ctz | Popcnt | ExtendS _ -> (ty, ty)
  | Trunc (sz, _) | TruncSat (sz, _) ->
      ((match sz with `F32 -> F32 | `F64 -> F64), ty)
  | Reinterpret ->
      ((match ty with I32 -> F32 | I64 -> F64 | _ -> assert false), ty)
  | Eqz -> (ty, I32)

let int_bin_op_type ty (op : Ast.Text.int_bin_op) =
  match op with
  | Add | Sub | Mul | Div _ | Rem _ | And | Or | Xor | Shl | Shr _ | Rotl | Rotr
    ->
      ty
  | Eq | Ne | Lt _ | Gt _ | Le _ | Ge _ -> I32

let float_un_op_type ty (op : Ast.Text.float_un_op) =
  match op with
  | Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt -> ty
  | Convert (sz, _) -> ( match sz with `I32 -> I32 | `I64 -> I64)
  | Reinterpret -> (
      match ty with F32 -> I32 | F64 -> I64 | _ -> assert false)

let float_bin_op_type ty (op : Ast.Text.float_bin_op) =
  match op with
  | Add | Sub | Mul | Div | Min | Max | CopySign -> ty
  | Eq | Ne | Lt | Gt | Le | Ge -> I32

let blocktype ctx (ty : Ast.Text.blocktype option) =
  match ty with
  | None -> ([], [])
  | Some (Typeuse (_, Some { params; results })) ->
      ( Array.to_list (Array.map (valtype ctx) params),
        Array.to_list (Array.map (valtype ctx) results) )
  | Some (Typeuse (_, None)) -> assert false (*ZZZ*)
  | Some (Valtype ty) -> ([], [ valtype ctx ty ])

let rec pop_args ctx args =
  match args with
  | [] -> return ()
  | ty :: rem ->
      let* () = pop_args ctx rem in
      pop ctx ty

let rec push_results results =
  match results with
  | [] -> return ()
  | ty :: rem ->
      let* () = push ty in
      push_results rem

let rec output_stack f st =
  match st with
  | Empty -> ()
  | Unreachable -> Format.fprintf f "unreachable"
  | Cons (ty, st) ->
      Format.fprintf f "@ %a%a"
        (Format.pp_print_option
           ~none:(fun f _ -> Format.fprintf f "bot")
           print_valtype)
        ty output_stack st

let print_stack st =
  Format.eprintf "Stack:%a@." output_stack st;
  (st, ())

let _ = print_stack

let with_empty_stack f =
  let st, () = f Empty in
  match st with
  | Cons _ ->
      Format.eprintf "Stack:%a@." output_stack st;
      assert false
  | Empty | Unreachable -> ()

let branch_target ctx (idx : Ast.Text.idx) =
  match idx.desc with
  | Num i -> snd (List.nth ctx.control_types (Uint32.to_int i))
  | Id id ->
      let rec find l id =
        match l with
        | [] -> assert false
        | (Some id', res) :: _ when id = id' -> res
        | _ :: rem -> find rem id
        (* ZZZ *)
      in
      find ctx.control_types id

let top_heap_type ctx (t : heaptype) : heaptype =
  match t with
  | Any | Eq | I31 | Struct | Array | None_ -> Any
  | Func | NoFunc -> Func
  | Exn | NoExn -> Exn
  | Extern | NoExtern -> Extern
  | Type ty -> (
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> Any
      | Func _ -> Func)

let storage_subtype info ty ty' =
  match (ty, ty') with
  | Packed I8, Packed I8 | Packed I16, Packed I16 -> true
  | Value ty, Value ty' -> Types.val_subtype info ty ty'
  | Packed I8, Packed I16
  | Packed I16, Packed I8
  | Packed _, Value _
  | Value _, Packed _ ->
      false

let field_subtype info (ty : fieldtype) (ty' : fieldtype) =
  ty.mut = ty'.mut
  && storage_subtype info ty.typ ty'.typ
  && ((not ty.mut) || storage_subtype info ty'.typ ty.typ)

let diff_ref_type t1 t2 =
  { nullable = t1.nullable && not t2.nullable; typ = t1.typ }

let with_current_stack f st = (st, f st)

let unpack_type (f : fieldtype) =
  match f.typ with Value v -> v | Packed _ -> I32

let rec repeat n f =
  if n = 0 then return ()
  else
    let* () = f in
    repeat (n - 1) f

let max_offset = Uint64.of_string "0x1_0000_0000"
let max_align = Uint64.of_int 8

let check_memarg _ sz { Ast.Text.offset; align } =
  (*ZZZ*)
  assert (Uint64.compare offset max_offset < 0);
  assert (Uint64.compare align max_align <= 8);
  assert (
    match (Uint64.to_int align, sz) with
    | 1, (`I8 | `I16 | `I32 | `I64) -> true
    | 2, (`I16 | `I32 | `I64) -> true
    | 4, (`I32 | `I64) -> true
    | 8, `I64 -> true
    | _ -> false);
  assert (Uint64.compare align max_align <= 0)

let memory_instruction_type_and_size ty =
  match (ty : _ Ast.Text.op) with
  | I32 _ -> (I32, `I32)
  | F32 _ -> (F32, `I32)
  | I64 _ -> (I64, `I64)
  | F64 _ -> (F64, `I64)

let field_has_default (ty : fieldtype) =
  match ty.typ with
  | Packed _ -> true
  | Value ty -> (
      match ty with
      | I32 | I64 | F32 | F64 | V128 -> true
      | Ref { nullable; _ } -> nullable
      | Tuple _ -> assert false)

let rec instruction ctx (i : _ Ast.Text.instr) =
  if false then Format.eprintf "%a@." print_instr i;
  match i.desc with
  | Block { label; typ; block = b } ->
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop_args ctx params in
      block ctx label params results results b;
      push_results results
  | Loop { label; typ; block = b } ->
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop_args ctx params in
      block ctx label params results params b;
      push_results results
  | If { label; typ; if_block; else_block } ->
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop ctx I32 in
      let* () = pop_args ctx params in
      block ctx label params results results if_block;
      block ctx label params results results else_block;
      push_results results
  | TryTable { label; typ; block = b; catches } ->
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop_args ctx params in
      block ctx label params results results b;
      List.iter
        (fun (catch : Ast.Text.catch) ->
          match catch with
          | Catch (tag, label) ->
              let ty = Sequence.get ctx.modul.tags tag in
              let args =
                match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
                | Struct _ | Array _ -> assert false (*ZZZ*)
                | Func { params; results } ->
                    assert (results = [||]);
                    params
              in
              let params = branch_target ctx label in
              with_empty_stack
                (let* () = push_results (Array.to_list args) in
                 pop_args ctx params)
          | CatchRef (tag, label) ->
              let ty = Sequence.get ctx.modul.tags tag in
              let args =
                match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
                | Struct _ | Array _ -> assert false (*ZZZ*)
                | Func { params; results } ->
                    assert (results = [||]);
                    params
              in
              let params = branch_target ctx label in
              with_empty_stack
                (let* () = push_results (Array.to_list args) in
                 let* () = push (Ref { nullable = false; typ = Exn }) in
                 pop_args ctx params)
          | CatchAll label ->
              let params = branch_target ctx label in
              with_empty_stack (pop_args ctx params)
          | CatchAllRef label ->
              let params = branch_target ctx label in
              with_empty_stack
                (let* () = push (Ref { nullable = false; typ = Exn }) in
                 pop_args ctx params))
        catches;
      push_results results
  | Try { label; typ; block = b; catches; catch_all } ->
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop_args ctx params in
      block ctx label params results results b;
      List.iter
        (fun (tag, b) ->
          let ty = Sequence.get ctx.modul.tags tag in
          let params' =
            match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
            | Struct _ | Array _ -> assert false (*ZZZ*)
            | Func { params; results } ->
                assert (results = [||]);
                params
          in
          block ctx label (Array.to_list params') results results b)
        catches;
      Option.iter (fun b -> block ctx label params results results b) catch_all;
      push_results results
  | Unreachable -> unreachable
  | Nop -> return ()
  | Throw idx -> (
      let ty = Sequence.get ctx.modul.tags idx in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          assert (results = [||]);
          let* () = pop_args ctx (Array.to_list params) in
          unreachable)
  | ThrowRef ->
      let* () = pop ctx (Ref { nullable = true; typ = Exn }) in
      unreachable
  | Br idx ->
      let params = branch_target ctx idx in
      let* () = pop_args ctx params in
      unreachable
  | Br_if idx ->
      let* () = pop ctx I32 in
      let params = branch_target ctx idx in
      let* () = pop_args ctx params in
      push_results params
  | Br_table (lst, idx) ->
      let* () = pop ctx I32 in
      let len = List.length (branch_target ctx idx) in
      let* () =
        with_current_stack (fun st ->
            List.iter
              (fun idx ->
                let params = branch_target ctx idx in
                assert (List.length params = len);
                ignore (pop_args ctx params st))
              (idx :: lst))
      in
      unreachable
  | Br_on_null idx -> (
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some (Ref { nullable = _; typ }) ->
          let params = branch_target ctx idx in
          let* () = pop_args ctx params in
          let* () = push_results params in
          push (Ref { nullable = false; typ })
      | Some _ -> assert false (*ZZZ*))
  | Br_on_non_null idx -> (
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some (Ref { nullable = _; typ }) ->
          let* () = push (Ref { nullable = false; typ }) in
          let params = branch_target ctx idx in
          let* () = pop_args ctx params in
          let* () = push_results params in
          let* _ = pop_any in
          return ()
      | Some _ -> assert false (*ZZZ*))
  | Br_on_cast (idx, ty1, ty2) ->
      let ty1 = reftype ctx.modul.types ty1 in
      let ty2 = reftype ctx.modul.types ty2 in
      assert (Types.val_subtype ctx.modul.subtyping_info (Ref ty2) (Ref ty1));
      let* () = pop ctx (Ref ty1) in
      let* () = push (Ref ty2) in
      let params = branch_target ctx idx in
      let* () = pop_args ctx params in
      let* () = push_results params in
      let* _ = pop_any in
      push (Ref (diff_ref_type ty1 ty2))
  | Br_on_cast_fail (idx, ty1, ty2) ->
      let ty1 = reftype ctx.modul.types ty1 in
      let ty2 = reftype ctx.modul.types ty2 in
      assert (Types.val_subtype ctx.modul.subtyping_info (Ref ty2) (Ref ty1));
      let* () = pop ctx (Ref ty1) in
      let* () = push (Ref (diff_ref_type ty1 ty2)) in
      let params = branch_target ctx idx in
      let* () = pop_args ctx params in
      let* () = push_results params in
      let* _ = pop_any in
      push (Ref ty2)
  | Return ->
      let* () = pop_args ctx ctx.return_types in
      unreachable
  | Call idx -> (
      let ty = Sequence.get ctx.modul.functions idx in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop_args ctx (Array.to_list params) in
          push_results (Array.to_list results))
  | CallRef idx -> (
      let ty = resolve_type_index ctx.modul.types idx in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop ctx (Ref { nullable = true; typ = Type ty }) in
          let* () = pop_args ctx (Array.to_list params) in
          push_results (Array.to_list results))
  | CallIndirect (idx, tu) -> (
      let typ = Sequence.get ctx.modul.tables idx in
      let ty = typeuse' ctx.modul.types tu in
      assert (
        Types.val_subtype ctx.modul.subtyping_info (Ref typ.reftype)
          (Ref { nullable = true; typ = Func }));
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop ctx I32 in
          let* () = pop_args ctx (Array.to_list params) in
          push_results (Array.to_list results))
  | ReturnCall idx -> (
      let ty = Sequence.get ctx.modul.functions idx in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop_args ctx (Array.to_list params) in
          with_empty_stack
            (let* () = push_results (Array.to_list results) in
             pop_args ctx ctx.return_types);
          unreachable)
  | ReturnCallRef idx -> (
      let ty = resolve_type_index ctx.modul.types idx in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop ctx (Ref { nullable = true; typ = Type ty }) in
          let* () = pop_args ctx (Array.to_list params) in
          with_empty_stack
            (let* () = push_results (Array.to_list results) in
             pop_args ctx ctx.return_types);
          unreachable)
  | ReturnCallIndirect (idx, tu) -> (
      let typ = Sequence.get ctx.modul.tables idx in
      let ty = typeuse' ctx.modul.types tu in
      assert (
        Types.val_subtype ctx.modul.subtyping_info (Ref typ.reftype)
          (Ref { nullable = true; typ = Func }));
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop ctx I32 in
          let* () = pop_args ctx (Array.to_list params) in
          with_empty_stack
            (let* () = push_results (Array.to_list results) in
             pop_args ctx ctx.return_types);
          unreachable)
  | Drop ->
      let* _ = pop_any in
      return ()
  | Select None -> (
      let* () = pop ctx I32 in
      let* ty1 = pop_any in
      let* ty2 = pop_any in
      match (ty1, ty2) with
      | None, None -> push_poly None
      | Some ty1, Some ty2 ->
          (*ZZZ*)
          assert (number_or_vec ty1);
          assert (number_or_vec ty2);
          assert (ty1 = ty2);
          push ty1
      | Some ty, None | None, Some ty ->
          assert (number_or_vec ty);
          (*ZZZ*)
          push ty)
  | Select (Some lst) -> (
      match lst with
      | [ typ ] ->
          let typ = valtype ctx.modul.types typ in
          let* () = pop ctx I32 in
          let* () = pop ctx typ in
          let* () = pop ctx typ in
          push typ
      | _ -> assert false)
  | LocalGet i -> push (get_local ctx i)
  | LocalSet i -> pop ctx (get_local ~initialize:true ctx i)
  | LocalTee i ->
      let ty = get_local ~initialize:true ctx i in
      let* () = pop ctx ty in
      push ty
  | GlobalGet idx ->
      let ty = Sequence.get ctx.modul.globals idx in
      push ty.typ
  | GlobalSet idx ->
      let ty = Sequence.get ctx.modul.globals idx in
      assert ty.mut;
      (*ZZZ*)
      pop ctx ty.typ
  | Load (idx, memarg, ty) ->
      let limits = Sequence.get ctx.modul.memories idx in
      let ty, sz = memory_instruction_type_and_size ty in
      check_memarg limits sz memarg;
      let* () = pop ctx I32 in
      push ty
  | LoadS (idx, memarg, ty, sz, _) ->
      let limits = Sequence.get ctx.modul.memories idx in
      let ty = match ty with `I32 -> I32 | `I64 -> I64 in
      check_memarg limits (sz :> [ `I8 | `I16 | `I32 | `I64 ]) memarg;
      let* () = pop ctx I32 in
      push ty
  | Store (idx, memarg, ty) ->
      let limits = Sequence.get ctx.modul.memories idx in
      let ty, sz = memory_instruction_type_and_size ty in
      check_memarg limits sz memarg;
      let* () = pop ctx ty in
      pop ctx I32
  | StoreS (idx, memarg, ty, sz) ->
      let limits = Sequence.get ctx.modul.memories idx in
      let ty = match ty with `I32 -> I32 | `I64 -> I64 in
      check_memarg limits (sz :> [ `I8 | `I16 | `I32 | `I64 ]) memarg;
      let* () = pop ctx ty in
      pop ctx I32
  | MemorySize idx ->
      ignore (Sequence.get ctx.modul.memories idx);
      push I32
  | MemoryGrow idx ->
      ignore (Sequence.get ctx.modul.memories idx);
      let* () = pop ctx I32 in
      push I32
  | MemoryFill idx ->
      ignore (Sequence.get ctx.modul.memories idx);
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      pop ctx I32
  | MemoryCopy (idx, idx') ->
      ignore (Sequence.get ctx.modul.memories idx);
      ignore (Sequence.get ctx.modul.memories idx');
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      pop ctx I32
  | MemoryInit (idx, idx') ->
      ignore (Sequence.get ctx.modul.memories idx);
      ignore (Sequence.get ctx.modul.data idx');
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      pop ctx I32
  | DataDrop idx ->
      ignore (Sequence.get ctx.modul.data idx);
      return ()
  | TableGet idx ->
      let typ = Sequence.get ctx.modul.tables idx in
      let* () = pop ctx I32 in
      push (Ref typ.reftype)
  | TableSet idx ->
      let typ = Sequence.get ctx.modul.tables idx in
      let* () = pop ctx (Ref typ.reftype) in
      pop ctx I32
  | TableSize idx ->
      ignore (Sequence.get ctx.modul.tables idx);
      push I32
  | TableGrow idx ->
      let typ = Sequence.get ctx.modul.tables idx in
      let* () = pop ctx I32 in
      let* () = pop ctx (Ref typ.reftype) in
      push I32
  | TableFill idx ->
      let typ = Sequence.get ctx.modul.tables idx in
      let* () = pop ctx I32 in
      let* () = pop ctx (Ref typ.reftype) in
      pop ctx I32
  | TableCopy (idx, idx') ->
      let ty = Sequence.get ctx.modul.tables idx in
      let ty' = Sequence.get ctx.modul.tables idx' in
      assert (
        Types.val_subtype ctx.modul.subtyping_info (Ref ty'.reftype)
          (Ref ty.reftype));
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      pop ctx I32
  | TableInit (idx, idx') ->
      let tabletype = Sequence.get ctx.modul.tables idx in
      let typ = Sequence.get ctx.modul.elem idx' in
      assert (
        Types.val_subtype ctx.modul.subtyping_info (Ref typ)
          (Ref tabletype.reftype));
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      pop ctx I32
  | ElemDrop idx ->
      ignore (Sequence.get ctx.modul.elem idx);
      return ()
  | RefNull typ ->
      let typ = heaptype ctx.modul.types typ in
      push (Ref { nullable = true; typ })
  | RefFunc idx ->
      let i = Sequence.get ctx.modul.functions idx in
      assert ((not !validate_refs) || Hashtbl.mem ctx.modul.refs i);
      push (Ref { nullable = false; typ = Type i })
  | RefIsNull -> (
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some (Ref _) -> push I32
      | Some _ -> assert false (*ZZZ*))
  | RefAsNonNull -> (
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some (Ref ty) -> push (Ref { ty with nullable = false })
      | Some _ -> assert false (*ZZZ*))
  | RefEq ->
      let* () = pop ctx (Ref { nullable = true; typ = Eq }) in
      let* () = pop ctx (Ref { nullable = true; typ = Eq }) in
      push I32
  | RefTest ty ->
      let ty = reftype ctx.modul.types ty in
      let* () =
        pop ctx (Ref { nullable = true; typ = top_heap_type ctx ty.typ })
      in
      push I32
  | RefCast ty ->
      let ty = reftype ctx.modul.types ty in
      let* () =
        pop ctx (Ref { nullable = true; typ = top_heap_type ctx ty.typ })
      in
      push (Ref ty)
  | StructNew idx ->
      let ty = resolve_type_index ctx.modul.types idx in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Struct fields ->
            pop_args ctx
              (Array.to_list
                 (Array.map
                    (fun (f : fieldtype) ->
                      match f.typ with Value v -> v | Packed _ -> I32)
                    fields))
        | _ -> (*ZZZ *) assert false
      in
      push (Ref { nullable = false; typ = Type ty })
  | StructNewDefault idx ->
      let ty = resolve_type_index ctx.modul.types idx in
      (match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct fields -> assert (Array.for_all field_has_default fields)
      | _ -> (*ZZZ *) assert false);
      push (Ref { nullable = false; typ = Type ty })
  | StructGet (signage, idx, idx') -> (
      let ty, fields = get_type_info ctx.modul.types idx in
      let* () = pop ctx (Ref { nullable = true; typ = Type ty }) in
      let n =
        match idx'.desc with
        | Id id -> List.assoc id fields
        | Num n -> Uint32.to_int n
      in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct fields ->
          (*ZZZ*)
          ignore signage;
          push (unpack_type fields.(n))
      | Array _ | Func _ -> assert false (*ZZZ*))
  | StructSet (idx, idx') ->
      let ty, fields = get_type_info ctx.modul.types idx in
      let n =
        match idx'.desc with
        | Id id -> List.assoc id fields
        | Num n -> Uint32.to_int n
      in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Struct fields ->
            (*ZZZ*)
            assert fields.(n).mut;
            pop ctx (unpack_type fields.(n))
        | Array _ | Func _ -> assert false (*ZZZ*)
      in
      pop ctx (Ref { nullable = true; typ = Type ty })
  | ArrayNew idx ->
      let ty = resolve_type_index ctx.modul.types idx in
      let* () = pop ctx I32 in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field -> pop ctx (unpack_type field)
        | _ -> (*ZZZ *) assert false
      in
      push (Ref { nullable = false; typ = Type ty })
  | ArrayNewDefault idx ->
      let ty = resolve_type_index ctx.modul.types idx in
      (match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Array field -> assert (field_has_default field)
      | _ -> (*ZZZ *) assert false);
      let* () = pop ctx I32 in
      push (Ref { nullable = false; typ = Type ty })
  | ArrayNewFixed (idx, n) ->
      let ty = resolve_type_index ctx.modul.types idx in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field -> repeat (Uint32.to_int n) (pop ctx (unpack_type field))
        | _ -> (*ZZZ *) assert false
      in
      push (Ref { nullable = false; typ = Type ty })
  | ArrayNewData (idx, idx') ->
      let ty = resolve_type_index ctx.modul.types idx in
      ignore (Sequence.get ctx.modul.data idx');
      let () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field ->
            assert (
              match field.typ with
              | Packed _ | Value (I32 | I64 | F32 | F64 | V128) -> true
              | Value (Ref _ | Tuple _) -> false)
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      push (Ref { nullable = false; typ = Type ty })
  | ArrayNewElem (idx, idx') ->
      let ty = resolve_type_index ctx.modul.types idx in
      let ty' = Sequence.get ctx.modul.elem idx' in
      let () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field ->
            assert (
              match field.typ with
              | Packed _ -> false
              | Value ty ->
                  Types.val_subtype ctx.modul.subtyping_info (Ref ty') ty)
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      push (Ref { nullable = false; typ = Type ty })
  | ArrayGet (_signage, idx) -> (
      (*ZZZ signage *)
      let ty = resolve_type_index ctx.modul.types idx in
      let* () = pop ctx I32 in
      let* () = pop ctx (Ref { nullable = true; typ = Type ty }) in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Array field -> push (unpack_type field)
      | _ -> (*ZZZ *) assert false)
  | ArraySet idx ->
      let ty = resolve_type_index ctx.modul.types idx in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field ->
            assert field.mut;
            pop ctx (unpack_type field)
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      pop ctx (Ref { nullable = true; typ = Type ty })
  | ArrayLen ->
      let* () = pop ctx (Ref { nullable = true; typ = Array }) in
      push I32
  | ArrayFill idx ->
      let ty = resolve_type_index ctx.modul.types idx in
      let* () = pop ctx I32 in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field ->
            assert field.mut;
            pop ctx (unpack_type field)
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      pop ctx (Ref { nullable = true; typ = Type ty })
  | ArrayCopy (idx1, idx2) ->
      let ty1 = resolve_type_index ctx.modul.types idx1 in
      let ty2 = resolve_type_index ctx.modul.types idx2 in
      (match
         ( (Types.get_subtype ctx.modul.subtyping_info ty1).typ,
           (Types.get_subtype ctx.modul.subtyping_info ty2).typ )
       with
      | Array field1, Array field2 ->
          assert field1.mut;
          assert (storage_subtype ctx.modul.subtyping_info field1.typ field2.typ)
      | _ -> (*ZZZ *) assert false);
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      let* () = pop ctx (Ref { nullable = true; typ = Type ty2 }) in
      let* () = pop ctx I32 in
      pop ctx (Ref { nullable = true; typ = Type ty1 })
  | ArrayInitData (idx, idx') ->
      let ty = resolve_type_index ctx.modul.types idx in
      ignore (Sequence.get ctx.modul.data idx');
      let () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field ->
            assert field.mut;
            assert (
              match field.typ with
              | Packed _ | Value (I32 | I64 | F32 | F64 | V128) -> true
              | Value (Ref _ | Tuple _) -> false)
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      pop ctx (Ref { nullable = true; typ = Type ty })
  | ArrayInitElem (idx, idx') ->
      let ty = resolve_type_index ctx.modul.types idx in
      let ty' = Sequence.get ctx.modul.elem idx' in
      let () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field ->
            assert field.mut;
            assert (
              match field.typ with
              | Packed _ -> false
              | Value ty ->
                  Types.val_subtype ctx.modul.subtyping_info (Ref ty') ty)
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      pop ctx (Ref { nullable = true; typ = Type ty })
  | RefI31 ->
      let* () = pop ctx I32 in
      push (Ref { nullable = false; typ = I31 })
  | I31Get _ ->
      let* () = pop ctx (Ref { nullable = true; typ = I31 }) in
      push I32
  | Const (I32 _) -> push I32
  | Const (I64 _) -> push I64
  | Const (F32 _) -> push F32
  | Const (F64 _) -> push F64
  | UnOp (I32 op) ->
      let expected, returned = int_un_op_type I32 op in
      let* () = pop ctx expected in
      push returned
  | UnOp (I64 op) ->
      let expected, returned = int_un_op_type I64 op in
      let* () = pop ctx expected in
      push returned
  | UnOp (F32 op) ->
      let expected = float_un_op_type F32 op in
      let* () = pop ctx expected in
      push F32
  | UnOp (F64 op) ->
      let expected = float_un_op_type F64 op in
      let* () = pop ctx expected in
      push F64
  | BinOp (I32 op) ->
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      push (int_bin_op_type I32 op)
  | BinOp (I64 op) ->
      let* () = pop ctx I64 in
      let* () = pop ctx I64 in
      push (int_bin_op_type I64 op)
  | BinOp (F32 op) ->
      let* () = pop ctx F32 in
      let* () = pop ctx F32 in
      push (float_bin_op_type F32 op)
  | BinOp (F64 op) ->
      let* () = pop ctx F64 in
      let* () = pop ctx F64 in
      push (float_bin_op_type F64 op)
  | I32WrapI64 ->
      let* () = pop ctx I64 in
      push I32
  | I64ExtendI32 _ ->
      let* () = pop ctx I32 in
      push I64
  | F32DemoteF64 ->
      let* () = pop ctx F64 in
      push F32
  | F64PromoteF32 ->
      let* () = pop ctx F32 in
      push F64
  | ExternConvertAny ->
      let* ty = pop_any in
      Option.iter
        (fun ty ->
          (*ZZZ*)
          assert (
            Types.val_subtype ctx.modul.subtyping_info ty
              (Ref { nullable = true; typ = Any })))
        ty;
      push (Ref { nullable = is_nullable ty; typ = Extern })
  | AnyConvertExtern ->
      let* ty = pop_any in
      Option.iter
        (fun ty ->
          (*ZZZ*)
          assert (
            Types.val_subtype ctx.modul.subtyping_info ty
              (Ref { nullable = true; typ = Extern })))
        ty;
      push (Ref { nullable = is_nullable ty; typ = Any })
  | Folded (i, l) ->
      let* () = instructions ctx l in
      instruction ctx i
  | TupleMake _ -> return ()
  (*
    (* Binaryen extensions *)
    | Pop of X.valtype
    | TupleMake of Int32.t
    | TupleExtract of Int32.t * Int32.t
*)
  | _ ->
      Format.eprintf "%a@." print_instr i;
      raise Exit

and instructions ctx l =
  match l with
  | [] -> return ()
  | i :: r ->
      let* () = instruction ctx i in
      instructions ctx r

and block ctx label params results br_params block =
  with_empty_stack
    (let* () = push_results params in
     let* () =
       instructions
         { ctx with control_types = (label, br_params) :: ctx.control_types }
         block
     in
     pop_args ctx results)

let rec check_constant_instruction ctx (i : _ Ast.Text.instr) =
  match i.desc with
  | GlobalGet idx -> assert (not (Sequence.get ctx.globals idx).mut)
  | RefFunc i -> Hashtbl.replace ctx.refs (Sequence.get ctx.functions i) ()
  | RefNull _ | StructNew _ | StructNewDefault _ | ArrayNew _
  | ArrayNewDefault _ | ArrayNewFixed _ | RefI31 | Const _
  | BinOp (I32 (Add | Sub | Mul) | I64 (Add | Sub | Mul))
  | ExternConvertAny | AnyConvertExtern ->
      ()
  | Folded (i, l) ->
      check_constant_instruction ctx i;
      check_constant_instructions ctx l
  | Block _ | Loop _ | If _ | TryTable _ | Try _ | Unreachable | Nop | Throw _
  | ThrowRef | Br _ | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _
  | Br_on_cast _ | Br_on_cast_fail _ | Return | Call _ | CallRef _
  | CallIndirect _ | ReturnCall _ | ReturnCallRef _ | ReturnCallIndirect _
  | Drop | Select _ | LocalGet _ | LocalSet _ | LocalTee _ | GlobalSet _
  | Load _ | LoadS _ | Store _ | StoreS _ | MemorySize _ | MemoryGrow _
  | MemoryFill _ | MemoryCopy _ | MemoryInit _ | DataDrop _ | TableGet _
  | TableSet _ | TableSize _ | TableGrow _ | TableFill _ | TableCopy _
  | TableInit _ | ElemDrop _ | RefIsNull | RefAsNonNull | RefEq | RefTest _
  | RefCast _ | StructGet _ | StructSet _ | ArrayNewData _ | ArrayNewElem _
  | ArrayGet _ | ArraySet _ | ArrayLen | ArrayFill _ | ArrayCopy _
  | ArrayInitData _ | ArrayInitElem _ | I31Get _ | UnOp _
  | BinOp
      ( F32 _ | F64 _
      | I32
          ( Div _ | Rem _ | And | Or | Xor | Shl | Shr _ | Rotl | Rotr | Eq | Ne
          | Lt _ | Gt _ | Le _ | Ge _ )
      | I64
          ( Div _ | Rem _ | And | Or | Xor | Shl | Shr _ | Rotl | Rotr | Eq | Ne
          | Lt _ | Gt _ | Le _ | Ge _ ) )
  | I32WrapI64 | I64ExtendI32 _ | F32DemoteF64 | F64PromoteF32 | Pop _
  | TupleMake _ | TupleExtract _ ->
      assert false

and check_constant_instructions ctx l =
  List.iter (fun i -> check_constant_instruction ctx i) l

let constant_expression ctx ty expr =
  check_constant_instructions ctx expr;
  with_empty_stack
    (let ctx =
       {
         locals = Sequence.make "local";
         control_types = [];
         return_types = [];
         modul = ctx;
       }
     in
     let* () = instructions ctx expr in
     pop ctx ty)

let add_type ctx ty =
  Array.iteri
    (fun i (label, _) ->
      (*ZZZ Check unique names*)
      Hashtbl.replace ctx.index_mapping
        (Uint32.of_int (ctx.last_index + i))
        (lnot i, []);
      Option.iter
        (fun label -> Hashtbl.replace ctx.label_mapping label (lnot i, []))
        label)
    ty;
  let i' = Types.add_rectype ctx.types (rectype ctx ty) in
  Array.iteri
    (fun i (label, typ) ->
      let fields =
        match (typ : Ast.Text.subtype).typ with
        | Struct fields ->
            Array.mapi
              (fun i (id, _) ->
                match id with Some id -> Some (id, i) | None -> None)
              fields
            |> Array.to_list |> List.filter_map Fun.id
        | _ -> []
      in
      Hashtbl.replace ctx.index_mapping
        (Uint32.of_int (ctx.last_index + i))
        (i' + i, fields);
      Option.iter
        (fun label -> Hashtbl.replace ctx.label_mapping label (i' + i, fields))
        label)
    ty;
  ctx.last_index <- ctx.last_index + Array.length ty

let register_exports ctx lst =
  List.iter
    (fun name ->
      assert (not (Hashtbl.mem ctx.exports name));
      (*ZZZ*)
      Hashtbl.add ctx.exports name ())
    lst

let limits { mi; ma } max =
  assert (
    match ma with
    | None -> Uint64.compare mi max <= 0
    | Some ma -> Uint64.compare mi ma <= 0 && Uint64.compare ma max <= 0)

let max_memory_size = Uint64.of_int 65536
let max_table_size = Uint64.of_string "0xffff_ffff"

let rec register_typeuses ctx l =
  List.iter (fun i -> register_typeuses' ctx i) l

and register_typeuses' ctx (i : _ Ast.Text.instr) =
  match i.desc with
  | Block { typ; _ }
  | Loop { typ; _ }
  | If { typ; _ }
  | TryTable { typ; _ }
  | Try { typ; _ } -> (
      match typ with
      | Some (Typeuse use) -> ignore (typeuse' ctx use)
      | Some (Valtype _) | None -> ())
  | CallIndirect (_, use) | ReturnCallIndirect (_, use) ->
      ignore (typeuse' ctx use)
  | Folded (i, l) ->
      register_typeuses' ctx i;
      register_typeuses ctx l
  | Unreachable | Nop | Throw _ | ThrowRef | Br _ | Br_if _ | Br_table _
  | Br_on_null _ | Br_on_non_null _ | Br_on_cast _ | Br_on_cast_fail _ | Return
  | Call _ | CallRef _ | ReturnCall _ | ReturnCallRef _ | Drop | Select _
  | LocalGet _ | LocalSet _ | LocalTee _ | GlobalGet _ | GlobalSet _ | Load _
  | LoadS _ | Store _ | StoreS _ | MemorySize _ | MemoryGrow _ | MemoryFill _
  | MemoryCopy _ | MemoryInit _ | DataDrop _ | TableGet _ | TableSet _
  | TableSize _ | TableGrow _ | TableFill _ | TableCopy _ | TableInit _
  | ElemDrop _ | RefNull _ | RefFunc _ | RefIsNull | RefAsNonNull | RefEq
  | RefTest _ | RefCast _ | StructNew _ | StructNewDefault _ | StructGet _
  | StructSet _ | ArrayNew _ | ArrayNewDefault _ | ArrayNewFixed _
  | ArrayNewData _ | ArrayNewElem _ | ArrayGet _ | ArraySet _ | ArrayLen
  | ArrayFill _ | ArrayCopy _ | ArrayInitData _ | ArrayInitElem _ | RefI31
  | I31Get _ | Const _ | UnOp _ | BinOp _ | I32WrapI64 | I64ExtendI32 _
  | F32DemoteF64 | F64PromoteF32 | ExternConvertAny | AnyConvertExtern | Pop _
  | TupleMake _ | TupleExtract _ ->
      ()

let build_initial_env ctx fields =
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Import { id; desc; exports; module_ = _; name = _ } -> (
          (* ZZZ Check for non-import fields *)
          register_exports ctx exports;
          match desc with
          | Func tu -> Sequence.register ctx.functions id (typeuse ctx.types tu)
          | Memory lim ->
              limits lim max_memory_size;
              Sequence.register ctx.memories id lim
          | Table typ ->
              limits typ.limits max_table_size;
              Sequence.register ctx.tables id (tabletype ctx.types typ)
          | Global ty ->
              Sequence.register ctx.globals id (globaltype ctx.types ty)
          | Tag tu ->
              let ty = typeuse ctx.types tu in
              (*
              (match (Types.get_subtype ctx.subtyping_info ty).typ with
              | Func { results; _ } -> assert (results = [||])
              | Struct _ | Array _ -> assert false (*ZZZ*));
*)
              Sequence.register ctx.tags id ty)
      | Func { id; typ; instrs; _ } ->
          Sequence.register ctx.functions id (typeuse ctx.types typ);
          register_typeuses ctx.types instrs
      | Tag { id; typ; exports } ->
          let ty = typeuse ctx.types typ in
          (*
          (match (Types.get_subtype ctx.subtyping_info ty).typ with
          | Func { results; _ } -> assert (results = [||])
          | Struct _ | Array _ -> assert false (*ZZZ*));
*)
          register_exports ctx exports;
          Sequence.register ctx.tags id ty
      | _ -> ())
    fields

let check_type_definitions ctx =
  for i = 0 to ctx.types.last_index - 1 do
    let i, _ =
      get_type_info ctx.types (Ast.no_loc (Ast.Text.Num (Uint32.of_int i)))
    in
    let ty = Types.get_subtype ctx.subtyping_info i in
    match ty.supertype with
    | None -> ()
    | Some j -> (
        let ty' = Types.get_subtype ctx.subtyping_info j in
        assert (not ty'.final);
        match (ty.typ, ty'.typ) with
        | ( Func { params; results },
            Func { params = params'; results = results' } ) ->
            assert (Array.length params = Array.length params');
            assert (Array.length results = Array.length results');
            Array.iter2
              (fun p p' -> assert (Types.val_subtype ctx.subtyping_info p' p))
              params params';
            Array.iter2
              (fun r r' -> assert (Types.val_subtype ctx.subtyping_info r r'))
              results results'
        | Struct fields, Struct fields' ->
            assert (Array.length fields' <= Array.length fields);
            for i = 0 to Array.length fields' - 1 do
              assert (field_subtype ctx.subtyping_info fields.(i) fields'.(i))
            done
        | Array field, Array field' ->
            assert (field_subtype ctx.subtyping_info field field')
        | Func _, (Struct _ | Array _)
        | Struct _, (Func _ | Array _)
        | Array _, (Func _ | Struct _) ->
            assert false)
  done

let tables_and_memories ctx fields =
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Memory { id; limits = lim; init = _; exports } ->
          limits lim max_memory_size;
          Sequence.register ctx.memories id lim;
          register_exports ctx exports
      | Table { id; typ; init; exports } ->
          limits typ.limits max_table_size;
          let typ = tabletype ctx.types typ in
          (match init with
          | Init_default -> assert typ.reftype.nullable
          | Init_expr e -> constant_expression ctx (Ref typ.reftype) e
          | Init_segment _ -> ());
          Sequence.register ctx.tables id typ;
          register_exports ctx exports
      | _ -> ())
    fields

let globals ctx fields =
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Global { id; typ; init; exports } ->
          let typ = globaltype ctx.types typ in
          constant_expression ctx typ.typ init;
          Sequence.register ctx.globals id typ;
          register_exports ctx exports
      | _ -> ())
    fields

let segments ctx fields =
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Memory { init; _ } -> (
          match init with
          | None -> ()
          | Some _ -> Sequence.register ctx.data None ())
      | Data { id; init = _; mode } ->
          (match mode with
          | Passive -> ()
          | Active (i, e) ->
              ignore (Sequence.get ctx.memories i);
              constant_expression ctx I32 (*or I64*) e);
          Sequence.register ctx.data id ()
      | Table { typ; init; _ } -> (
          match init with
          | Init_default | Init_expr _ -> ()
          | Init_segment lst ->
              let typ = reftype ctx.types typ.reftype in
              List.iter (fun e -> constant_expression ctx (Ref typ) e) lst;
              Sequence.register ctx.elem None typ)
      | Elem { id; typ; init; mode } ->
          let typ = reftype ctx.types typ in
          (match mode with
          | Passive | Declare -> ()
          | Active (i, e) ->
              let tabletype = Sequence.get ctx.tables i in
              assert (
                Types.val_subtype ctx.subtyping_info (Ref typ)
                  (Ref tabletype.reftype));
              constant_expression ctx I32 e);
          List.iter (fun e -> constant_expression ctx (Ref typ) e) init;
          Sequence.register ctx.elem id typ
      | _ -> ())
    fields

let functions ctx fields =
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Func { id = _; typ; locals = locs; instrs; exports } ->
          let func_typ =
            match
              (Types.get_subtype ctx.subtyping_info (typeuse ctx.types typ)).typ
            with
            | Func typ -> typ
            | _ -> assert false
          in
          let return_types = Array.to_list func_typ.results in
          let locals = Sequence.make "local" in
          (match typ with
          | _, Some (params, _) ->
              List.iter
                (fun (id, typ) ->
                  Sequence.register locals id (valtype ctx.types typ))
                params
          | _ ->
              Array.iter
                (fun typ -> Sequence.register locals None typ)
                func_typ.params);
          List.iter
            (fun (id, typ) ->
              Sequence.register locals id (valtype ctx.types typ))
            locs;
          with_empty_stack
            (let ctx =
               {
                 locals;
                 control_types = [ (None, return_types) ];
                 return_types;
                 modul = ctx;
               }
             in
             let* () = instructions ctx instrs in
             pop_args ctx return_types);
          register_exports ctx exports
      | _ -> ())
    fields

let exports ctx fields =
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Export { name; kind; index } -> (
          register_exports ctx [ name ];
          match kind with
          | Func -> ignore (Sequence.get ctx.functions index)
          | Memory -> ignore (Sequence.get ctx.memories index)
          | Table -> ignore (Sequence.get ctx.tables index)
          | Tag -> ignore (Sequence.get ctx.tags index)
          | Global -> ignore (Sequence.get ctx.globals index))
      | _ -> ())
    fields

let start ctx fields =
  let start_count = ref 0 in
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Start idx -> (
          assert (!start_count = 0);
          incr start_count;
          let ty = Sequence.get ctx.functions idx in
          match (Types.get_subtype ctx.subtyping_info ty).typ with
          | Struct _ | Array _ -> assert false
          | Func { params; results } -> assert (params = [||] && results = [||])
          )
      | _ -> ())
    fields

let f (_, fields) =
  let type_context =
    {
      types = Types.create ();
      last_index = 0;
      index_mapping = Hashtbl.create 16;
      label_mapping = Hashtbl.create 16;
    }
  in
  List.iter
    (fun (field : _ Ast.Text.modulefield) ->
      match field with
      | Types rectype -> add_type type_context rectype
      | _ -> ())
    fields;
  let ctx =
    {
      types = type_context;
      subtyping_info = Types.subtyping_info type_context.types;
      functions = Sequence.make "function";
      memories = Sequence.make "memory";
      tables = Sequence.make "tables";
      globals = Sequence.make "global";
      tags = Sequence.make "tag";
      data = Sequence.make "data";
      elem = Sequence.make "data";
      exports = Hashtbl.create 16;
      refs = Hashtbl.create 16;
    }
  in
  check_type_definitions ctx;
  build_initial_env ctx fields;
  let ctx =
    { ctx with subtyping_info = Types.subtyping_info type_context.types }
  in
  tables_and_memories ctx fields;
  globals ctx fields;
  segments ctx fields;
  functions ctx fields;
  exports ctx fields;
  start ctx fields
