open Ast.Binary.Types

module Sequence = struct
  type 'a t = {
    name : string;
    index_mapping : (int, 'a) Hashtbl.t;
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
    let idx = seq.last_index in
    seq.last_index <- seq.last_index + 1;
    Hashtbl.add seq.index_mapping idx v;
    Option.iter (fun id -> (*ZZZ*) Hashtbl.add seq.label_mapping id v) id

  let get seq (idx : Ast.Text.idx) =
    match idx with
    | Num n -> (
        try Hashtbl.find seq.index_mapping (Int32.to_int n)
        with Not_found ->
          Format.eprintf "Unbound %s %ld@." seq.name n;
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
  index_mapping : (int, int * (string * int) list) Hashtbl.t;
  label_mapping : (string, int * (string * int) list) Hashtbl.t;
}

let get_type_info ctx (idx : Ast.Text.idx) =
  match idx with
  | Num x ->
      let x = Int32.to_int x in
      Hashtbl.find ctx.index_mapping x
  | Id id -> Hashtbl.find ctx.label_mapping id

let resolve_type_index ctx idx = fst (get_type_info ctx idx)

let heaptype ctx (h : Ast.Text.heaptype) : heaptype =
  match h with
  | Func -> Func
  | NoFunc -> NoFunc
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

type module_context = {
  types : type_context;
  subtyping_info : Types.subtyping_info;
  functions : int Sequence.t;
  memories : limits Sequence.t;
  globals : globaltype Sequence.t;
  tags : int Sequence.t;
}

type ctx = {
  locals : valtype Sequence.t;
  control_types : (string option * valtype list) list;
  return_types : valtype list;
  modul : module_context;
}

type stack = Unreachable | Empty | Cons of valtype * stack

let print_heaptype f (ty : heaptype) =
  match ty with
  | Func -> Format.fprintf f "func"
  | NoFunc -> Format.fprintf f "nofunc"
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
        Format.fprintf f "@[<1>(ref@ null@ %a)]" print_heaptype typ
      else Format.fprintf f "@[<1>(ref@ %a)]" print_heaptype typ
  | Tuple _ -> assert false

let pop_any st =
  match st with
  | Unreachable -> (Unreachable, None)
  | Cons (ty, r) -> (r, Some ty)
  | Empty -> assert false

let pop ctx ty st =
  match st with
  | Unreachable -> (Unreachable, ())
  | Cons (ty', r) ->
      let ok = Types.val_subtype ctx.modul.subtyping_info ty' ty in
      if not ok then
        Format.eprintf "%a <: %a@." print_valtype ty' print_valtype ty;
      assert ok;
      (r, ())
  | Empty -> assert false

let push ty st = (Cons (ty, st), ())
let unreachable _ = (Unreachable, ())
let return v st = (st, v)

let ( let* ) e f st =
  let st, v = e st in
  f v st

let resolve_index tbl (idx : Ast.Text.idx) =
  match idx with
  | Num i -> (*ZZZ overflow*) Int32.to_int i
  | Id id -> (*ZZZ*) Hashtbl.find tbl id

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

let rec print_stack st =
  match st with
  | Empty | Unreachable -> ()
  | Cons (ty, st) ->
      Format.eprintf "%a@." print_valtype ty;
      print_stack st

let with_empty_stack f =
  let st, () = f Empty in
  match st with
  | Cons _ ->
      prerr_endline "Stack:";
      print_stack st;
      assert false
  | Empty | Unreachable -> ()

let print_stack st =
  prerr_endline "Stack:";
  print_stack st;
  (st, ())

let branch_target ctx (idx : Ast.Text.idx) =
  match idx with
  | Num i -> snd (List.nth ctx.control_types (Int32.to_int i))
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
  | Extern | NoExtern -> Extern
  | Type ty -> (
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> Any
      | Func _ -> Func)

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

let rec instruction ctx (i : Ast.Text.instr) =
  match i with
  | Block { label; typ; block = b } | Loop { label; typ; block = b } ->
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop_args ctx params in
      block ctx label params results b;
      push_results results
  | If { label; typ; if_block; else_block } ->
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop ctx I32 in
      let* () = pop_args ctx params in
      block ctx label params results if_block;
      block ctx label params results else_block;
      push_results results
  (*
    | Try of {
        label : X.label;
        typ : blocktype option;
        block : instr list;
        catches : (X.idx * instr list) list;
        catch_all : instr list option;
      }
*)
  | Try { label; typ; block = b; catches = _; catch_all = _ } ->
      (*ZZZ handlers*)
      let params, results = blocktype ctx.modul.types typ in
      let* () = pop_args ctx params in
      block ctx label params results b;
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
      let* () =
        with_current_stack (fun st ->
            List.iter
              (fun idx ->
                let params = branch_target ctx idx in
                ignore (pop_args ctx params st))
              lst)
      in
      let params = branch_target ctx idx in
      let* () = pop_args ctx params in
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
          push_results params
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
      push (Ref (diff_ref_type ty2 ty1))
  | Br_on_cast_fail (idx, ty1, ty2) ->
      let ty1 = reftype ctx.modul.types ty1 in
      let ty2 = reftype ctx.modul.types ty2 in
      assert (Types.val_subtype ctx.modul.subtyping_info (Ref ty2) (Ref ty1));
      let* () = pop ctx (Ref ty1) in
      let* () = push (Ref (diff_ref_type ty2 ty1)) in
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
  (*
    | CallIndirect of X.idx * X.typeuse
*)
  | ReturnCall idx -> (
      let ty = Sequence.get ctx.modul.functions idx in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop_args ctx (Array.to_list params) in
          let* () = push_results (Array.to_list results) in
          let* () = pop_args ctx ctx.return_types in
          unreachable)
  | ReturnCallRef idx -> (
      let ty = resolve_type_index ctx.modul.types idx in
      match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
      | Struct _ | Array _ -> assert false (*ZZZ*)
      | Func { params; results } ->
          let* () = pop ctx (Ref { nullable = true; typ = Type ty }) in
          let* () = pop_args ctx (Array.to_list params) in
          let* () = push_results (Array.to_list results) in
          let* () = pop_args ctx ctx.return_types in
          unreachable)
  (*
    | ReturnCallIndirect of X.idx * X.typeuse
*)
  | Drop ->
      let* _ = pop_any in
      return ()
  | Select None -> (
      let* () = pop ctx I32 in
      let* ty1 = pop_any in
      let* ty2 = pop_any in
      match (ty1, ty2) with
      | None, None -> return ()
      | Some ty1, Some ty2 ->
          (*ZZZ*)
          assert (number_or_vec ty1);
          assert (number_or_vec ty2);
          assert (ty1 = ty2);
          push ty1
      | Some ty, None | None, Some ty ->
          (*ZZZ*)
          assert (number_or_vec ty);
          push ty)
  (*
    | Select of X.valtype option
*)
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
  (*
    | I32Load8 of signage * memarg
    | I32Store8 of memarg
*)
  | RefNull typ ->
      let typ = heaptype ctx.modul.types typ in
      push (Ref { nullable = true; typ })
  | RefFunc idx ->
      push
        (Ref
           {
             nullable = false;
             typ = Type (Sequence.get ctx.modul.functions idx);
           })
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
  (*
    | StructNewDefault of X.idx
*)
  | StructGet (signage, idx, idx') -> (
      let ty, fields = get_type_info ctx.modul.types idx in
      let* () = pop ctx (Ref { nullable = true; typ = Type ty }) in
      let n =
        match idx' with
        | Id id -> List.assoc id fields
        | Num n -> Int32.to_int n
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
        match idx' with
        | Id id -> List.assoc id fields
        | Num n -> Int32.to_int n
      in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Struct fields ->
            (*ZZZ*)
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
  (*
    | ArrayNewDefault of X.idx
*)
  | ArrayNewFixed (idx, n) ->
      let ty = resolve_type_index ctx.modul.types idx in
      let* () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array field -> repeat (Int32.to_int n) (pop ctx (unpack_type field))
        | _ -> (*ZZZ *) assert false
      in
      push (Ref { nullable = false; typ = Type ty })
  | ArrayNewData (idx, _idx') ->
      (*ZZZ data *)
      let ty = resolve_type_index ctx.modul.types idx in
      let () =
        match (Types.get_subtype ctx.modul.subtyping_info ty).typ with
        | Array _ -> ()
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      let* () = pop ctx I32 in
      push (Ref { nullable = false; typ = Type ty })
  (*
    | ArrayNewData of X.idx * X.idx
    | ArrayNewElem of X.idx * X.idx
*)
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
        | Array field -> pop ctx (unpack_type field)
        | _ -> (*ZZZ *) assert false
      in
      let* () = pop ctx I32 in
      pop ctx (Ref { nullable = true; typ = Type ty })
  | ArrayLen ->
      let* () = pop ctx (Ref { nullable = true; typ = Array }) in
      push I32
  (*
    | ArrayFill of X.idx
    | ArrayCopy of X.idx * X.idx
    | ArrayInitData of X.idx * X.idx
    | ArrayInitElem of X.idx * X.idx
*)
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
  (*
    (* Binaryen extensions *)
    | Pop of X.valtype
    | TupleMake of Int32.t
    | TupleExtract of Int32.t * Int32.t
*)
  | _ ->
      Format.eprintf "%a@." Output.instr i;
      assert false

and instructions ctx l =
  match l with
  | [] -> return ()
  | i :: r ->
      let* () = instruction ctx i in
      instructions ctx r

and block ctx label params results block =
  with_empty_stack
    (let* () = push_results params in
     let* () =
       instructions
         { ctx with control_types = (label, results) :: ctx.control_types }
         block
     in
     pop_args ctx results)

let add_type ctx ty =
  Array.iteri
    (fun i (label, _) ->
      (*ZZZ Check unique names*)
      Hashtbl.replace ctx.index_mapping (ctx.last_index + i) (lnot i, []);
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
      Hashtbl.replace ctx.index_mapping (ctx.last_index + i) (i' + i, fields);
      Option.iter
        (fun label -> Hashtbl.replace ctx.label_mapping label (i' + i, fields))
        label)
    ty;
  ctx.last_index <- ctx.last_index + Array.length ty

(*
  idx option * ((id option * valtype) list * valtype list) option
*)
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

let build_initial_env ctx fields =
  List.iter
    (fun (field : Ast.Text.modulefield) ->
      match field with
      | Import { id; desc; _ } -> (
          (* ZZZ Check for non-import fields *)
          match desc with
          | Func tu -> Sequence.register ctx.functions id (typeuse ctx.types tu)
          | Memory limits -> Sequence.register ctx.memories id limits
          | Global ty ->
              Sequence.register ctx.globals id (globaltype ctx.types ty)
          | Tag tu -> Sequence.register ctx.tags id (typeuse ctx.types tu))
      | Func { id; typ; _ } ->
          Sequence.register ctx.functions id (typeuse ctx.types typ)
      | Memory { id; limits; _ } ->
          (*ZZZ init *)
          Sequence.register ctx.memories id limits
      | Tag { id; typ; _ } ->
          Sequence.register ctx.tags id (typeuse ctx.types typ)
      | _ -> ())
    fields

let globals ctx fields =
  List.iter
    (fun (field : Ast.Text.modulefield) ->
      match field with
      | Global { id; typ; init; _ } ->
          let typ = globaltype ctx.types typ in
          with_empty_stack
            (let ctx =
               {
                 locals = Sequence.make "local";
                 control_types = [];
                 return_types = [];
                 modul = ctx;
               }
             in
             let* () = instructions ctx init in
             pop ctx typ.typ);
          Sequence.register ctx.globals id typ
      | _ -> ())
    fields

let functions ctx fields =
  List.iter
    (fun (field : Ast.Text.modulefield) ->
      match field with
      | Func { id; typ; locals = locs; instrs; _ } ->
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
          | _ -> ());
          List.iter
            (fun (id, typ) ->
              Sequence.register locals id (valtype ctx.types typ))
            locs;
          Format.eprintf "=== %s@." (Option.value ~default:"" id);
          with_empty_stack
            (let ctx =
               { locals; control_types = []; return_types; modul = ctx }
             in
             let* () = instructions ctx instrs in
             pop_args ctx return_types)
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
    (fun (field : Ast.Text.modulefield) ->
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
      globals = Sequence.make "global";
      tags = Sequence.make "tag";
    }
  in
  build_initial_env ctx fields;
  let ctx =
    { ctx with subtyping_info = Types.subtyping_info type_context.types }
  in
  globals ctx fields;
  functions ctx fields
