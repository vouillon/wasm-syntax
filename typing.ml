(*
Type, function/global and tag names are unique
A local let can override a previous let
*)

open Ast

exception Type_error of location * string

module Namespace = struct
  type t = (string, string * location) Hashtbl.t

  let make () = Hashtbl.create 16

  let register ns kind x =
    match Hashtbl.find_opt ns x.descr with
    | None -> Hashtbl.replace ns x.descr (kind, x.loc)
    | Some (kind', _loc') ->
        raise
          (Type_error
             ( x.loc,
               Printf.sprintf "A %s named %s is already bound" kind' x.descr ))
end

module Tbl = struct
  type 'a t = {
    kind : string;
    namespace : Namespace.t;
    tbl : (string, 'a) Hashtbl.t;
  }

  let make namespace kind = { kind; namespace; tbl = Hashtbl.create 16 }

  let add env x v =
    Namespace.register env.namespace env.kind x;
    Hashtbl.replace env.tbl x.descr v

  let override env x v = Hashtbl.replace env.tbl x.descr v

  let find env x =
    try Hashtbl.find env.tbl x.descr
    with Not_found ->
      raise
        (Type_error (x.loc, Printf.sprintf "Unbound %s %s\n" env.kind x.descr))

  let find_opt env x = Hashtbl.find_opt env.tbl x.descr
end

type type_context = {
  internal_types : Wasm.Types.t;
  types : (int * comptype) Tbl.t;
}

let resolve_type_name ctx name = fst (Tbl.find ctx.types name)

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
  (*ZZZ Check unique field names*)
  Array.iteri (fun i (name, typ) -> Tbl.add ctx.types name (lnot i, typ.typ)) ty;
  let i' = Wasm.Types.add_rectype ctx.internal_types (rectype ctx ty) in
  Array.iteri
    (fun i (name, typ) -> Tbl.override ctx.types name (i' + i, typ.typ))
    ty

type module_context = {
  type_context : type_context;
  subtyping_info : Wasm.Types.subtyping_info;
  types : (int * comptype) Tbl.t;
  functions : (int * string) Tbl.t;
  globals : (*mutable:*) (bool * Internal.valtype * valtype) Tbl.t;
  tags : funsig Tbl.t;
  memories : limits Tbl.t;
}

module UnionFind = struct
  type 'a state = Link of 'a t | Root of 'a
  and 'a t = { mutable state : 'a state }

  let make v = { state = Root v }

  let rec representative node =
    match node.state with
    | Root _ -> node
    | Link next ->
        let root = representative next in
        if next != root then node.state <- Link root;
        root

  let find node =
    let root = representative node in
    match root.state with Root v -> v | Link _ -> assert false

  let merge t1 t2 new_val =
    let root1 = representative t1 in
    let root2 = representative t2 in
    if root1 == root2 then root1.state <- Root new_val
    else begin
      root1.state <- Link root2;
      root2.state <- Root new_val
    end

  let set t new_val =
    let root = representative t in
    root.state <- Root new_val
end

let top_heap_type ctx (t : heaptype) : heaptype =
  match t with
  | Any | Eq | I31 | Struct | Array | None_ -> Any
  | Func | NoFunc -> Func
  | Extern | NoExtern -> Extern
  | Type ty -> (
      match snd (Tbl.find ctx.types ty) with
      | Struct _ | Array _ -> Any
      | Func _ -> Func)

type inferred_type =
  | Null
  | Number
  | Int
  | Float
  | Valtype of { typ : valtype; internal : Internal.valtype }

let subtype ctx ty ty' =
  let ity = UnionFind.find ty in
  let ity' = UnionFind.find ty' in
  match (ity, ity') with
  | Valtype ty, Valtype ty' ->
      Wasm.Types.val_subtype ctx.subtyping_info ty'.internal ty.internal
  | Null, Null
  | Int, Int
  | Float, Float
  | Number, Number
  | (Int | Float | Valtype { internal = I32 | I64 | F32 | F64; _ }), Number
  | Valtype { internal = I32 | I64; _ }, Int
  | Valtype { internal = F32 | F64; _ }, Float ->
      UnionFind.merge ty ty' ity;
      true
  | Number, Valtype { internal = I32 | I64 | F32 | F64; _ }
  | Int, Valtype { internal = I32 | I64; _ }
  | Float, Valtype { internal = F32 | F64; _ }
  | Null, Valtype { internal = Ref { nullable = true; _ }; _ } ->
      UnionFind.merge ty ty' ity';
      true
  | ( Null,
      ( Number | Int | Float
      | Valtype
          {
            internal =
              ( I32 | I64 | F32 | F64 | V128
              | Ref { nullable = false; _ }
              | Tuple _ );
            _;
          } ) )
  | Valtype _, Null
  | Valtype { internal = V128 | Ref _ | Tuple _; _ }, Number
  | Valtype { internal = F32 | F64 | V128 | Ref _ | Tuple _; _ }, Int
  | Valtype { internal = I32 | I64 | V128 | Ref _ | Tuple _; _ }, Float
  | ( Number,
      (Null | Int | Float | Valtype { internal = V128 | Ref _ | Tuple _; _ }) )
  | ( Int,
      ( Null | Float
      | Valtype { internal = F32 | F64 | V128 | Ref _ | Tuple _; _ } ) )
  | ( Float,
      (Null | Int | Valtype { internal = I32 | I64 | V128 | Ref _ | Tuple _; _ })
    ) ->
      false

let cast ctx ty ty' (ity' : Internal.valtype) =
  let ity = UnionFind.find ty in
  match (ity, ity') with
  | (Number | Int), Ref { typ = I31; _ } ->
      UnionFind.set ty (Valtype { typ = I32; internal = I32 });
      true
  | (Number | Int), I32 | Int, F32 ->
      UnionFind.set ty (Valtype { typ = I32; internal = I32 });
      true
  | (Number | Int), I64 | Int, F64 ->
      UnionFind.set ty (Valtype { typ = I64; internal = I64 });
      true
  | (Number | Float), F32 | Float, I32 ->
      UnionFind.set ty (Valtype { typ = F32; internal = F32 });
      true
  | (Number | Float), F64 | Float, I64 ->
      UnionFind.set ty (Valtype { typ = F64; internal = F64 });
      true
  | Null, Ref { nullable = true; _ } ->
      UnionFind.set ty (Valtype { typ = ty'; internal = ity' });
      true
  | Valtype { internal = I32; _ }, Ref { typ = I31; _ } -> true
  | Valtype ty, _ -> Wasm.Types.val_subtype ctx.subtyping_info ity' ty.internal
  | ( (Number | Int | Float),
      ( Ref
          {
            typ =
              ( Func | NoFunc | Extern | NoExtern | Any | Eq | Array | Struct
              | Type _ | None_ );
            _;
          }
      | V128 | Tuple _ ) )
  | Float, Ref { typ = I31; _ }
  | Null, (I32 | I64 | F32 | F64 | V128 | Ref { nullable = false; _ } | Tuple _)
    ->
      false

type stack =
  | Unreachable
  | Empty
  | Cons of location * inferred_type UnionFind.t * stack

let unreachable _ = (Unreachable, ())
let return v st = (st, v)

let ( let* ) e f st =
  let st, v = e st in
  f v st

let pop_any st =
  match st with
  | Unreachable -> (Unreachable, None)
  | Cons (_, ty, r) -> (r, Some ty)
  | Empty -> assert false

let pop ctx ty st =
  match st with
  | Unreachable -> (Unreachable, ())
  | Cons (_, ty', r) ->
      let ok = subtype ctx ty' ty in
      (*ZZZ
      if not ok then
        Format.eprintf "%a <: %a@." print_valtype ty' print_valtype ty;
*)
      assert ok;
      (r, ())
  | Empty -> assert false

let push loc ty st = (Cons (loc, ty, st), ())

let rec pop_args ctx args =
  match args with
  | [] -> return ()
  | ty :: rem ->
      let* () = pop_args ctx rem in
      pop ctx ty

let rec push_results results =
  match results with
  | [] -> return ()
  | (loc, ty) :: rem ->
      let* () = push loc ty in
      push_results rem

let rec instruction ctx i =
  match i.descr with
  | Block _ -> assert false
  (*
  | Loop of label option * instr list
  | If of label option * instr * instr list * instr list option
*)
  | Unreachable -> unreachable
  | Nop -> return ()
  | Null -> push i.loc (UnionFind.make Null)
  | Get idx -> (
      (*ZZZ local *)
      match Tbl.find_opt ctx.globals idx with
      | Some (_, ty, ty') ->
          push i.loc (UnionFind.make (Valtype { typ = ty'; internal = ty }))
      | None -> (
          match Tbl.find_opt ctx.functions idx with
          | Some (ty, ty') ->
              push i.loc
                (UnionFind.make
                   (Valtype
                      {
                        typ =
                          Ref { nullable = false; typ = Type (Ast.no_loc ty') };
                        internal = Ref { nullable = false; typ = Type ty };
                      }))
          | None ->
              Format.eprintf "%a@." Output.instr i;
              assert false))
  | Set (idx, i') -> (
      let* () = instruction ctx i' in
      (*ZZZ local *)
      match Tbl.find_opt ctx.globals idx with
      | Some (mut, ty, ty') ->
          assert mut;
          (*ZZZ*)
          pop ctx (UnionFind.make (Valtype { typ = ty'; internal = ty }))
      | None -> (
          match Tbl.find_opt ctx.functions idx with
          | Some _ -> assert false (*ZZZ*)
          | None -> assert false))
  (*
  | Tee of idx * instr
  | Call of instr * instr list
  | String of idx option * string
*)
  | Int _ -> push i.loc (UnionFind.make Number)
  | Float _ -> push i.loc (UnionFind.make Float)
  | Cast (i, typ) -> (
      let* () = instruction ctx i in
      let* ty' = pop_any in
      match ty' with
      | None -> assert false
      | Some ty' ->
          let ty = valtype ctx.type_context typ in
          assert (cast ctx ty' typ ty);
          push i.loc (UnionFind.make (Valtype { typ; internal = ty })))
  | Test (i, ty) ->
      let* () = instruction ctx i in
      let typ = Ref { nullable = true; typ = top_heap_type ctx ty.typ } in
      pop ctx
        (UnionFind.make
           (Valtype { typ; internal = valtype ctx.type_context typ }))
  | Struct (ty, fields) -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          let* () =
            match snd (Tbl.find ctx.types ty) with
            | Struct fields' ->
                assert (List.length fields = Array.length fields');
                (*ZZZ*)
                let* () =
                  Array.fold_left
                    (fun cont (name, _) ->
                      match
                        List.find_opt
                          (fun (idx, _) -> name.descr = idx.descr)
                          fields
                      with
                      | None -> assert false (*ZZZ*)
                      | Some (_, i') ->
                          let* () = instruction ctx i' in
                          cont)
                    (return ()) fields'
                in
                pop_args ctx
                  (Array.to_list
                     (Array.map
                        (fun (_, (f : fieldtype)) ->
                          let typ =
                            match f.typ with Value v -> v | Packed _ -> I32
                          in
                          UnionFind.make
                            (Valtype
                               { typ; internal = valtype ctx.type_context typ }))
                        fields'))
            | _ -> (*ZZZ *) assert false
          in
          let typ = Ref { nullable = false; typ = Type ty } in
          push i.loc
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  (*
  | Struct of idx option * (idx * instr) list
  | StructGet of instr * idx
  | StructSet of instr * idx * instr
  | Array of idx option * instr * instr
  | ArrayFixed of idx option * instr list
  | ArrayGet of instr * instr
  | ArraySet of instr * instr * instr
  | BinOp of binop * instr * instr
  | UnOp of unop * instr
  | Let of (idx option * valtype option) list * instr option
  | Br of label * instr option
  | Br_if of label * instr
  | Br_table of label list * instr
  | Br_on_null of label * instr
  | Br_on_non_null of label * instr
  | Br_on_cast of label * reftype * instr
  | Br_on_cast_fail of label * reftype * instr
  | Throw of idx * instr list
  | Return of instr option
*)
  | Sequence l -> instructions ctx l
  (*
  | Select of instr * instr * instr
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

(*ZZZ
let fundecl ctx typ sign =
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
*)

let with_empty_stack f =
  let st, () = f Empty in
  match st with
  | Cons _ ->
      (*ZZZ
      prerr_endline "Stack:";
      print_stack st;
*)
      assert false
  | Empty | Unreachable -> ()

let globals type_context ctx fields =
  List.iter
    (fun field ->
      match field with
      | Global { name; mut; typ = Some typ; def; _ } ->
          with_empty_stack
            ((*let ctx =
               {
                 locals = Sequence.make "local";
                 control_types = [];
                 return_types = [];
                 modul = ctx;
               }
             in
*)
             let* () = instruction ctx def in
             let typ' = valtype type_context typ in
             Tbl.add ctx.globals name (mut, typ', typ);
             pop ctx (UnionFind.make (Valtype { typ; internal = typ' })))
      | _ -> ())
    fields

let f (_, fields) =
  let type_context =
    {
      internal_types = Wasm.Types.create ();
      types = Tbl.make (Namespace.make ()) "type";
    }
  in
  List.iter
    (fun (field : modulefield) ->
      match field with Type rectype -> add_type type_context rectype | _ -> ())
    fields;
  let ctx =
    let namespace = Namespace.make () in
    {
      type_context;
      subtyping_info = Wasm.Types.subtyping_info type_context.internal_types;
      types = type_context.types;
      functions = Tbl.make namespace "function";
      globals = Tbl.make namespace "global";
      tags = Tbl.make (Namespace.make ()) "tag";
      memories = Tbl.make (Namespace.make ()) "memories";
    }
  in
  (*
  List.iter
    (fun field ->
      match field with
      | Fundecl { name; typ; sign; _ } ->
          (*ZZZ Check existing*)
          Tbl.add ctx.functions name (fundecl ctx.types typ sign)
      | GlobalDecl { name; typ; _ } ->
          Tbl.add ctx.globals name (globaltype type_context typ, typ)
      | Func { name; typ; sign; _ } ->
          Tbl.add ctx.functions name (fundecl ctx.types typ sign)
      | Tag { name; typ; _ } -> Tbl.add ctx.tags name (fundecl ctx.types typ)
      | _ -> ())
    fields;
*)
  let ctx =
    {
      ctx with
      subtyping_info = Wasm.Types.subtyping_info type_context.internal_types;
    }
  in
  globals type_context ctx fields;
  (*
  functions ctx fields
*)
  ()

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
