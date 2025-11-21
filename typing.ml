open Ast

type stack = Unreachable | Empty | Cons of valtype * stack

type type_context = {
  internal_types : Wasm.Types.t;
  types : (string, int * comptype) Hashtbl.t;
}

(*ZZZ unbound type*)
let resolve_type_name ctx name = fst (Hashtbl.find ctx.types name)

module Internal = Wasm.Ast.Binary.Types

let heaptype ctx (h : heaptype) : Internal.heaptype =
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
  | Type idx -> Type (resolve_type_name ctx idx)

let reftype ctx { nullable; typ } : Internal.reftype =
  { nullable; typ = heaptype ctx typ }

let rec valtype ctx ty : Internal.valtype =
  match ty with
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128
  | Ref r -> Ref (reftype ctx r)
  | Tuple l -> Tuple (List.map (valtype ctx) l)

let functype ctx { params; results } : Internal.functype =
  {
    params = Array.map (fun ty -> valtype ctx ty) params;
    results = Array.map (fun ty -> valtype ctx ty) results;
  }

let storagetype ctx ty : Internal.storagetype =
  match ty with Value ty -> Value (valtype ctx ty) | Packed ty -> Packed ty

let muttype f ctx { mut; typ } = { mut; typ = f ctx typ }
let fieldtype ctx ty = muttype storagetype ctx ty
let globaltype ctx ty = muttype valtype ctx ty

let comptype ctx (ty : comptype) : Internal.comptype =
  match ty with
  | Func ty -> Func (functype ctx ty)
  | Struct fields -> Struct (Array.map (fun (_, ty) -> fieldtype ctx ty) fields)
  | Array field -> Array (fieldtype ctx field)

let subtype ctx { typ; supertype; final } : Internal.subtype =
  {
    typ = comptype ctx typ;
    supertype = Option.map (fun ty -> resolve_type_name ctx ty) supertype;
    final;
  }

let rectype ctx ty = Array.map (fun (_, ty) -> subtype ctx ty) ty

let add_type ctx ty =
  (*ZZZ Check unique names / field names*)
  let i' = Wasm.Types.add_rectype ctx.internal_types (rectype ctx ty) in
  Array.iteri
    (fun i (name, typ) -> Hashtbl.replace ctx.types name (i' + i, typ.typ))
    ty

type module_context = {
  subtyping_info : Wasm.Types.subtyping_info;
  types : (string, int * comptype) Hashtbl.t;
  functions : (string, int * string) Hashtbl.t;
  globals : (string, Internal.globaltype * globaltype) Hashtbl.t;
  tags : (string, funsig) Hashtbl.t;
  memories : (string, limits) Hashtbl.t;
}

let typeuse ctx typ sign =
  match (typ, sign) with
  | Some idx, _ ->
      (*ZZZ Validate signature *)
      resolve_type_name ctx idx
  | _, Some sign ->
      (* The type of function [name] *)
      Wasm.Types.add_rectype ctx.internal_types
        [|
          { typ = Func (signature ctx sign); supertype = None; final = true };
        |]
  | None, None -> assert false (*ZZZ*)

let f (_, fields) =
  let type_context =
    { internal_types = Wasm.Types.create (); types = Hashtbl.create 16 }
  in
  List.iter
    (fun (field : modulefield) ->
      match field with Type rectype -> add_type type_context rectype | _ -> ())
    fields;
  let ctx =
    {
      subtyping_info = Wasm.Types.subtyping_info type_context.internal_types;
      types = type_context.types;
      functions = Hashtbl.create 16;
      globals = Hashtbl.create 16;
      tags = Hashtbl.create 16;
      memories = Hashtbl.create 16;
    }
  in
  List.iter
    (fun field ->
      match field with
      | Fundecl { name; typ; sign; _ } ->
          (*ZZZ Check existing*)
          Hashtbl.add ctx.functions name (typeuse ctx.types typ sign)
      | GlobalDecl { name; typ; _ } ->
          Hashtbl.add ctx.globals name (globaltype type_context typ, typ)
      | Func { name; typ; sign; _ } ->
          Hashtbl.add ctx.functions name (typeuse ctx.types typ sign)
      | Tag { name; typ; _ } ->
          Hashtbl.add ctx.tags name (typeuse ctx.types typ)
      | _ -> ())
    fields;
  let ctx =
    {
      ctx with
      subtyping_info = Wasm.Types.subtyping_info type_context.internal_types;
    }
  in
  globals ctx fields;
  functions ctx fields

(*
let rec infer env (stack : _ list) i : _ option =
  match i with
  | Unreachable -> None
  (*
  | Block (_label, _instrs) ->
  | Loop (label, instrs) ->
  | If of string option * instr list * instr list * instr list option
  | Get of idx
  | Set of idx * instr list
  | Call of idx * instr list list
  | RefFunc of idx
  | StructGet of instr list * string
  | StructSet of instr list * string * instr list
  | BinOp of binop * instr list * instr list
  | Local of string * valtype option * instr list option
*)
  | Struct (name, _fields) ->
      Some [ `Type (Ref { nullable = false; typ = Type name }) ]
  | String _ -> Some [ `String ]
  | Int _ -> Some [ `Int ]
  | Cast (l, t) ->
      check_list env [] l [ `Typ t ];
      Some [ `Typ t ]
  | Nop -> Some stack
  | BinOp (_op, l1, l2) ->
      let _stack' = infer_list env [] l1 in
      let _stack'' = infer_list env [] l2 in
      assert false
  | _ -> assert false

and infer_list env stack l =
  match l with
  | [] -> Some []
  | i :: r -> (
      let stack' = infer env stack i in
      match stack' with None -> None | Some stack' -> infer_list env stack' r)

and check_list env stack l =
  match l with
  | i :: _r ->
      let _st = infer env stack i in
      assert false
  | [] -> assert false
*)

(*
- Arity inference?
  ==> we know the type of a block; how do we split the block to check this type?
- Use stack to type sequence of instructions

Expressions may consume some of the stack but return a single value
_
=> expects one value on the stack; leave the stack unchanged

f(_,_,x)
=> expects two values on the stack; return some values (depending on f)

_+1

Annotated syntax tree: type (+ is it checked or inferred?)
*)
