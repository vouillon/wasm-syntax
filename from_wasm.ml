module Src = Wasm.Ast.Text

let get_annot (a, _) = a
let get_type (_, t) = t
let name_field _st _a = "foo"
let name_type _st _a = "foo"
let annotated a t = (a, t)

(*ZZZ Get name from table*)
let idx _st _ i =
  match i with Src.Num i -> Format.sprintf "t%ld" i | Id i -> i

let heaptype st (t : Src.heaptype) : Ast.heaptype =
  match t with
  | Src.Func -> Ast.Func
  | NoFunc -> NoFunc
  | Extern -> Extern
  | NoExtern -> NoExtern
  | Any -> Any
  | Eq -> Eq
  | I31 -> I31
  | Struct -> Struct
  | Array -> Array
  | None_ -> None_
  | Type i -> Type (idx st `Type i)

let reftype st (t : Src.reftype) : Ast.reftype =
  { nullable = t.nullable; typ = heaptype st t.typ }

let rec valtype st (t : Src.valtype) : Ast.valtype =
  match t with
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128
  | Ref t -> Ref (reftype st t)
  | Tuple l -> Tuple (List.map (fun t -> valtype st t) l)

let rec functype st (t : Src.functype) : Ast.functype =
  {
    params = Array.map (fun t -> valtype st t) t.params;
    results = Array.map (fun t -> valtype st t) t.results;
  }

let rec packedtype _ (t : Src.packedtype) : Ast.packedtype = t

let rec storagetype st (t : Src.storagetype) : Ast.storagetype =
  match t with
  | Value t -> Value (valtype st t)
  | Packed t -> Packed (packedtype st t)

let muttype typ st (t : _ Src.muttype) : _ Ast.muttype =
  { t with typ = typ st t.typ }

let fieldtype = muttype storagetype

let comptype st (t : Src.comptype) : Ast.comptype =
  match t with
  | Func t -> Func (functype st t)
  | Struct l ->
      Struct
        (Array.map
           (fun t ->
             annotated (name_field st (get_annot t)) (fieldtype st (get_type t)))
           l)
  | Array t -> Array (fieldtype st t)

let subtype st (t : Src.subtype) : Ast.subtype =
  {
    typ = comptype st t.typ;
    supertype = Option.map (fun i -> idx st `Type i) t.supertype;
    final = t.final;
  }

let rectype st (t : Src.rectype) : Ast.rectype =
  Array.map
    (fun t -> annotated (name_type st (get_annot t)) (subtype st (get_type t)))
    t

let globaltype = muttype valtype

(*
Step 1: traverse types and find existing names
Step 2: use this info to generate using names without reusing existing names
*)

(*ZZZ
  - first pass to see missing labels
  - explode tuples
*)
let sequence l = match l with [ i ] -> i | _ -> Ast.no_loc (Ast.Sequence [])

let sequence_opt l =
  match l with
  | [] -> None
  | [ i ] -> Some i
  | _ -> Some (Ast.no_loc (Ast.Sequence []))

let rec instr st (i : Src.instr) (args : Ast.instr list) : Ast.instr =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  match i with
  | Block { label; typ = _; block } ->
      assert (args = []);
      no_loc (Block (label, List.map (fun i -> instr st i []) block))
  | Loop { label; typ = _; block } ->
      assert (args = []);
      no_loc (Block (label, List.map (fun i -> instr st i []) block))
  | If { label; typ = _; if_block; else_block } ->
      no_loc
        (If
           ( label,
             sequence args,
             List.map (fun i -> instr st i []) if_block,
             if else_block = [] then None
             else Some (List.map (fun i -> instr st i []) else_block) ))
  | Unreachable -> sequence (args @ [ no_loc Unreachable ])
  | Nop -> sequence (args @ [ no_loc Nop ])
  | Br i -> no_loc (Br (idx st `Label i, sequence_opt args))
  | Br_if i -> no_loc (Br_if (idx st `Label i, sequence args))
  | Folded (i, args') ->
      assert (args = []);
      instr st i (List.map (fun i -> instr st i []) args')
  | LocalGet x -> sequence (args @ [ no_loc (Get (idx st `Local x)) ])
  | GlobalGet x -> sequence (args @ [ no_loc (Get (idx st `Global x)) ])
  | LocalSet x -> no_loc (Set (idx st `Local x, sequence args))
  | GlobalSet x -> no_loc (Set (idx st `Global x, sequence args))
  | _ -> assert false
(*
    | Try of {
        label : X.label;
        typ : blocktype option;
        block : instr list;
        catches : (X.idx * instr list) list;
        catch_all : instr list option;
      }
    | Throw of X.idx
    | Br_table of X.idx list * X.idx
    | Br_on_null of X.idx
    | Br_on_non_null of X.idx
    | Br_on_cast of X.idx * X.reftype * X.reftype
    | Br_on_cast_fail of X.idx * X.reftype * X.reftype
    | Return
    | Call of X.idx
    | CallRef of X.idx
    | CallIndirect of X.idx * X.typeuse
    | ReturnCall of X.idx
    | ReturnCallRef of X.idx
    | ReturnCallIndirect of X.idx * X.typeuse
    | Drop
    | Select of X.valtype option
    | LocalTee of X.idx
    | I32Load8 of signage * memarg
    | I32Store8 of memarg
    | RefNull of X.heaptype
    | RefFunc of X.idx
    | RefIsNull
    | RefAsNonNull
    | RefEq
    | RefTest of X.reftype
    | RefCast of X.reftype
    | StructNew of X.idx
    | StructNewDefault of X.idx
    | StructGet of signage option * X.idx * X.idx
    | StructSet of X.idx * X.idx
    | ArrayNew of X.idx
    | ArrayNewDefault of X.idx
    | ArrayNewFixed of X.idx * Int32.t
    | ArrayNewData of X.idx * X.idx
    | ArrayNewElem of X.idx * X.idx
    | ArrayGet of signage option * X.idx
    | ArraySet of X.idx
    | ArrayLen
    | ArrayFill of X.idx
    | ArrayCopy of X.idx * X.idx
    | ArrayInitData of X.idx * X.idx
    | ArrayInitElem of X.idx * X.idx
    | RefI31
    | I31Get of signage
    | Const of (Int32.t, Int64.t, string, string) op
    | UnOp of (int_un_op, int_un_op, float_un_op, float_un_op) op
    | BinOp of (int_bin_op, int_bin_op, float_bin_op, float_bin_op) op
    | I32WrapI64
    | I64ExtendI32 of signage
    | F32DemoteF64
    | F64PromoteF32
    | ExternConvertAny
    | AnyConvertExtern
    | Folded of instr * instr list
    (* Binaryen extensions *)
    | Pop of X.valtype
    | TupleMake of Int32.t
    | TupleExtract of Int32.t * Int32.t
*)
