(*
TODO:
- fix typeuse validation (add a type if not already present)
- enforce expressions
- return a typed tree
- check that underscores are properly placed
- check for floating types? when an instruction may trap (Div)?
- error messages
- locations on the heap when push several values?
- more methods rather than global functions (no ambiguity)?
  rotl(..), rotr(..), min(..), max(..), copysign(..)
- tests:
  - short pieces of syntax, read / write / error
  - write the translated files, parse/validate them, translate/validate them back
- move lets at more appropriate places
- remove redundant type annotations/casts
- take into account that locals can shadow globals to get better local names
  (if a global is not used in a function, we can reuse its name)
- check constant expressions
- option to tighten casts to any/extern / eliminate redundant casts
  and type annotations

Comments
- process the flow of tokens
  => keep track of the line of the previous token
  => comment/newline ==>
     register in a side table the blank lines and comments,
     and do not propagate
- emission:
  before: grab all the comment/newline before the current location /
          output comments
  after: grab all the comment/newline after the current location and
     before the next sibling (and in the parent); split a last newline;
     push back the comments after last newline;
     output comments

Syntax changes:
- names in result type (symmetry with params)
- no need to have func type for tags (declaration tag : ty)
- we may not need Sequence (change branch expressions instead)
- what should the syntax of throw tag be (parentheses around parameters?)

Syntax ideas:
- dispatch foo ['a 'b ... else 'c] { 'a { } 'b { } ... }
- br 'a (e1, ..., en) if cond   / if cond br 'a (e1, ..., en)

Misc:
- blocks in an expression context return one value; otherwise, no value by default

Explicit types?
   fn(..)->(..)
==> for function types
==> for call_indirect
(We don't have a cast to a typeuse in WAT)
*)

open Ast

module Output = struct
  include Output

  let valtype f t = Utils.Printer.run f (fun pp -> Output.valtype pp t)
  let instr f i = Utils.Printer.run f (fun pp -> Output.instr pp i)
end


exception Type_error of location * string

module Namespace = struct
  type t = (string, string * location) Hashtbl.t

  let make () = Hashtbl.create 16

  let register ns kind x =
    match Hashtbl.find_opt ns x.desc with
    | None -> Hashtbl.replace ns x.desc (kind, x.info)
    | Some (kind', _loc') ->
        raise
          (Type_error
             ( x.info,
               Printf.sprintf "A %s named %s is already bound" kind' x.desc ))
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
    Hashtbl.replace env.tbl x.desc v

  let override env x v = Hashtbl.replace env.tbl x.desc v

  let find env x =
    try Hashtbl.find env.tbl x.desc
    with Not_found ->
      raise
        (Type_error (x.info, Printf.sprintf "Unbound %s %s\n" env.kind x.desc))

  let find_opt env x = Hashtbl.find_opt env.tbl x.desc
  let iter env f = Hashtbl.iter f env.tbl
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
    ty;
  i'

type inferred_valtype = { typ : valtype; internal : Internal.valtype }

type inferred_type =
  | Null
  | Number
  | Int8
  | Int16
  | Int
  | Float
  | Valtype of inferred_valtype

module StringMap = Map.Make (String)

type module_context = {
  type_context : type_context;
  subtyping_info : Wasm.Types.subtyping_info;
  types : (int * comptype) Tbl.t;
  functions : (int * string) Tbl.t;
  globals : (*mutable:*) (bool * inferred_valtype) Tbl.t;
  tags : functype Tbl.t;
  memories : limits Tbl.t;
  mutable locals : inferred_valtype StringMap.t;
  control_types : (string option * inferred_valtype list) list;
  return_types : inferred_valtype list;
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
  | Exn | NoExn -> Exn
  | Extern | NoExtern -> Extern
  | Type ty -> (
      match snd (Tbl.find ctx.types ty) with
      | Struct _ | Array _ -> Any
      | Func _ -> Func)

let diff_ref_type t1 t2 =
  { nullable = t1.nullable && not t2.nullable; typ = t1.typ }

let diff_ref_type_internal (t1 : Internal.reftype) (t2 : Internal.reftype) =
  { Internal.nullable = t1.nullable && not t2.nullable; typ = t1.typ }

let storage_subtype ctx ty ty' =
  match (ty, ty') with
  | Packed I8, Packed I8 | Packed I16, Packed I16 -> true
  | Value ty, Value ty' ->
      Wasm.Types.val_subtype ctx.subtyping_info
        (valtype ctx.type_context ty)
        (valtype ctx.type_context ty')
  | Packed I8, Packed I16
  | Packed I16, Packed I8
  | Packed _, Value _
  | Value _, Packed _ ->
      false

let storage_subtype' ctx (ty : Wasm.Ast.Binary.storagetype)
    (ty' : Wasm.Ast.Binary.storagetype) =
  match (ty, ty') with
  | Packed I8, Packed I8 | Packed I16, Packed I16 -> true
  | Value ty, Value ty' -> Wasm.Types.val_subtype ctx.subtyping_info ty ty'
  | Packed I8, Packed I16
  | Packed I16, Packed I8
  | Packed _, Value _
  | Value _, Packed _ ->
      false

let field_subtype info (ty : Wasm.Ast.Binary.fieldtype)
    (ty' : Wasm.Ast.Binary.fieldtype) =
  ty.mut = ty'.mut
  && storage_subtype' info ty.typ ty'.typ
  && ((not ty.mut) || storage_subtype' info ty'.typ ty.typ)

let subtype ctx ty ty' =
  let ity = UnionFind.find ty in
  let ity' = UnionFind.find ty' in
  match (ity, ity') with
  | Valtype ty, Valtype ty' ->
      Wasm.Types.val_subtype ctx.subtyping_info ty.internal ty'.internal
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
    )
  | (Int8 | Int16), _
  | _, (Int8 | Int16) ->
      false

let cast ctx ty ty' =
  (*ZZZ Cast between Any and Extern *)
  let ity = UnionFind.find ty in
  match (ity, ty') with
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
  | Null, Ref { typ = ty'; _ } ->
      let ty' = Ref { nullable = true; typ = top_heap_type ctx ty' } in
      let ity' = valtype ctx.type_context ty' in
      UnionFind.set ty (Valtype { typ = ty'; internal = ity' });
      true
  | Valtype { internal = F32 | F64; _ }, (F32 | F64)
  | Valtype { internal = I32 | I64; _ }, I32
  | Valtype { internal = I64; _ }, I64
  | Valtype { internal = V128; _ }, V128
  | Valtype { internal = I32; _ }, Ref { typ = I31; _ } ->
      true
  | Valtype { internal = Ref _ as ity; _ }, Ref { typ = ty'; nullable } -> (
      (let ty' = Ref { nullable = true; typ = top_heap_type ctx ty' } in
       let ity' = valtype ctx.type_context ty' in
       Wasm.Types.val_subtype ctx.subtyping_info ity ity')
      ||
      match ty' with
      | Extern ->
          let ty' = Ref { nullable; typ = Any } in
          let ity' = valtype ctx.type_context ty' in
          Wasm.Types.val_subtype ctx.subtyping_info ity ity'
      | Any ->
          let ty' = Ref { nullable; typ = Extern } in
          let ity' = valtype ctx.type_context ty' in
          Wasm.Types.val_subtype ctx.subtyping_info ity ity'
      | _ -> false)
  | ( (Number | Int | Float | Valtype { internal = I32 | F32 | I64 | F64; _ }),
      ( Ref
          {
            typ =
              ( Func | NoFunc | Exn | NoExn | Extern | NoExtern | Any | Eq
              | Array | Struct | Type _ | None_ );
            _;
          }
      | V128 | Tuple _ ) )
  | Valtype { internal = F32 | F64; _ }, (I32 | I64)
  | Valtype { internal = I32 | I64; _ }, (F32 | F64)
  | Valtype { internal = I32; _ }, I64
  | ( (Float | Valtype { internal = I64 | F32 | F64 | V128; _ }),
      (I32 | Ref { typ = I31; _ }) )
  | ( (Null | Valtype { internal = Ref _; _ }),
      (I32 | I64 | F32 | F64 | V128 | Tuple _) )
  | Valtype { internal = V128; _ }, (I64 | F32 | F64 | Ref _ | Tuple _)
  | Valtype { internal = Tuple _; _ }, _
  | (Int8 | Int16), _ ->
      false

let signed_cast ctx ty ty' =
  let ity = UnionFind.find ty in
  match (ity, ty') with
  | (Int8 | Int16), `I32 -> true
  | Valtype { internal = Ref _ as ity; _ }, `I32 ->
      let ty' = Ref { nullable = true; typ = Any } in
      let ity' = valtype ctx.type_context ty' in
      Wasm.Types.val_subtype ctx.subtyping_info ity ity'
  | Null, `I32 ->
      UnionFind.set ty
        (Valtype
           {
             typ = Ref { typ = Any; nullable = true };
             internal = Ref { typ = Any; nullable = true };
           });
      true
  | (Number | Int), `I64 ->
      UnionFind.set ty (Valtype { typ = I32; internal = I32 });
      true
  | Valtype { internal = I32; _ }, `I64
  | Valtype { internal = I32 | I64; _ }, (`F32 | `F64)
  | Valtype { internal = F32 | F64; _ }, (`I32 | `I64) ->
      true
  | (Number | Int), (`I32 | `F32 | `F64) (* Floating types can make this fail *)
  | Valtype { internal = I32; _ }, `I32
  | Valtype { internal = I64; _ }, (`I32 | `I64)
  | Valtype { internal = F32 | F64; _ }, (`F32 | `F64)
  | ( ( Int8 | Int16 | Null
      | Valtype
          {
            internal =
              Ref { typ = Type _ | None_ | Struct | Array | I31 | Eq | Any; _ };
            _;
          } ),
      (`I64 | `F32 | `F64) )
  | ( ( Float
      | Valtype
          {
            internal =
              ( V128
              | Ref { typ = Func | NoFunc | Exn | NoExn | Extern | NoExtern; _ }
              | Tuple _ );
            _;
          } ),
      _ ) ->
      false

type stack =
  | Unreachable
  | Empty
  | Cons of location * inferred_type UnionFind.t option * stack

let output_inferred_type f ty =
  match UnionFind.find ty with
  | Null -> Format.fprintf f "null"
  | Number -> Format.fprintf f "number"
  | Int -> Format.fprintf f "int"
  | Int16 -> Format.fprintf f "int16"
  | Int8 -> Format.fprintf f "int8"
  | Float -> Format.fprintf f "float"
  | Valtype ty -> Output.valtype f ty.typ

let rec output_stack f st =
  match st with
  | Empty -> ()
  | Unreachable -> Format.fprintf f "@ unreachable"
  | Cons (_, ty, st) ->
      Format.fprintf f "@ %a%a"
        (Format.pp_print_option
           ~none:(fun f _ -> Format.fprintf f "bot")
           output_inferred_type)
        ty output_stack st

let unreachable _ = (Unreachable, ())
let return v st = (st, v)

let ( let* ) e f st =
  let st, v = e st in
  f v st

let pop_any st =
  match st with
  | Unreachable -> (Unreachable, None)
  | Cons (_, ty, r) -> (r, ty)
  | Empty -> assert false

let pop ctx ty st =
  match st with
  | Unreachable -> (Unreachable, ())
  | Cons (_, None, r) -> (r, ())
  | Cons (_, Some ty', r) ->
      let ok = subtype ctx ty' ty in
      if not ok then
        Format.eprintf "%a <: %a@." output_inferred_type ty'
          output_inferred_type ty;
      assert ok;
      (r, ())
  | Empty -> assert false

let push_poly loc ty st = (Cons (loc, ty, st), ())
let push loc ty st = push_poly loc (Some ty) st

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

let print_stack st =
  Format.eprintf "@[<2>Stack: %a@]@." output_stack st;
  (st, ())

let rec repeat n f =
  if n = 0 then return ()
  else
    let* () = f in
    repeat (n - 1) f

let with_empty_stack f =
  let st, () = f Empty in
  match st with
  | Cons _ ->
      Format.eprintf "@[<2>Stack:%a@]@." output_stack st;
      assert false
  | Empty | Unreachable -> ()

let fieldtype ctx (f : fieldtype) =
  match f.typ with
  | Value typ -> Valtype { typ; internal = valtype ctx.type_context typ }
  | Packed I8 -> Int8
  | Packed I16 -> Int16

let unpack_type (f : fieldtype) =
  match f.typ with Value v -> v | Packed _ -> I32

let branch_target ctx label =
  let rec find l label =
    match l with
    | [] -> assert false (* ZZZ *)
    | (Some label', res) :: _ when label = label' -> res
    | _ :: rem -> find rem label
  in
  find ctx.control_types label

let check_int_bin_op i typ1 typ2 =
  (match (UnionFind.find typ1, UnionFind.find typ2) with
  | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
  | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
  | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int) ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ1)
  | (Number | Int), Valtype { internal = I32 | I64; _ } ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ2)
  | Number, Number -> UnionFind.merge typ1 typ2 Int
  | _ -> assert false (*ZZZ*));
  push i.info typ1

let check_float_bin_op i typ1 typ2 =
  (match (UnionFind.find typ1, UnionFind.find typ2) with
  | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
  | Valtype { internal = F64; _ }, Valtype { internal = F64; _ }
  | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float) ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ1)
  | (Number | Float), Valtype { internal = F32 | F64; _ } ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ2)
  | Number, Number -> UnionFind.merge typ1 typ2 Float
  | _ -> assert false (*ZZZ*));
  push i.info typ1

let with_current_stack f st = (st, f st)

let field_has_default (ty : fieldtype) =
  match ty.typ with
  | Packed _ -> true
  | Value ty -> (
      match ty with
      | I32 | I64 | F32 | F64 | V128 -> true
      | Ref { nullable; _ } -> nullable
      | Tuple _ -> assert false)

let rec instruction ctx i =
  (*
  let* () = print_stack in
*)
  if false then Format.eprintf "%a@." Output.instr i;
  match i.desc with
  | Block (label, bt, instrs) ->
      let { params; results } = bt in
      let params =
        Array.to_list
          (Array.map
             (fun typ ->
               UnionFind.make
                 (Valtype { typ; internal = valtype ctx.type_context typ }))
             params)
      in
      let results =
        Array.to_list
          (Array.map
             (fun typ -> { typ; internal = valtype ctx.type_context typ })
             results)
      in
      let* () = pop_args ctx params in
      block ctx i.info label params results results instrs;
      push_results
        (List.map
           (fun typ ->
             ( i.info,
               (*ZZZ*)
               UnionFind.make (Valtype typ) ))
           results)
  | Loop (label, bt, instrs) ->
      let { params; results } = bt in
      let params0 =
        Array.to_list
          (Array.map
             (fun typ -> { typ; internal = valtype ctx.type_context typ })
             params)
      in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params0 in
      let results =
        Array.to_list
          (Array.map
             (fun typ -> { typ; internal = valtype ctx.type_context typ })
             results)
      in
      let* () = pop_args ctx params in
      block ctx i.info label params results params0 instrs;
      push_results
        (List.map
           (fun typ ->
             ( i.info,
               (*ZZZ*)
               UnionFind.make (Valtype typ) ))
           results)
  | If (label, bt, i', if_block, else_block) ->
      let* () = instruction ctx i' in
      let { params; results } = bt in
      let params =
        Array.to_list
          (Array.map
             (fun typ ->
               UnionFind.make
                 (Valtype { typ; internal = valtype ctx.type_context typ }))
             params)
      in
      let results =
        Array.to_list
          (Array.map
             (fun typ -> { typ; internal = valtype ctx.type_context typ })
             results)
      in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* () = pop_args ctx params in
      block ctx i.info label params results results if_block;
      (match else_block with
      | None -> ()
      | Some else_block ->
          block ctx i.info label params results results else_block);
      push_results
        (List.map
           (fun typ ->
             ( i.info,
               (*ZZZ*)
               UnionFind.make (Valtype typ) ))
           results)
  | Unreachable -> unreachable
  | Nop -> return ()
  | Pop -> return ()
  | Null -> push i.info (UnionFind.make Null)
  | Get idx -> (
      match StringMap.find_opt idx.desc ctx.locals with
      | Some ty -> push i.info (UnionFind.make (Valtype ty))
      | None -> (
          match Tbl.find_opt ctx.globals idx with
          | Some (_, ty) -> push i.info (UnionFind.make (Valtype ty))
          | None -> (
              match Tbl.find_opt ctx.functions idx with
              | Some (ty, ty') ->
                  push i.info
                    (UnionFind.make
                       (Valtype
                          {
                            typ =
                              Ref
                                {
                                  nullable = false;
                                  typ = Type (Ast.no_loc ty');
                                };
                            internal = Ref { nullable = false; typ = Type ty };
                          }))
              | None ->
                  Format.eprintf "%a@." Output.instr i;
                  assert false)))
  | Set (None, i') ->
      let* () = instruction ctx i' in
      let* _ = pop_any in
      return ()
  | Set (Some idx, i') -> (
      let* () = instruction ctx i' in
      match StringMap.find_opt idx.desc ctx.locals with
      | Some ty -> pop ctx (UnionFind.make (Valtype ty))
      | None -> (
          match Tbl.find_opt ctx.globals idx with
          | Some (mut, ty) ->
              assert mut;
              (*ZZZ*)
              pop ctx (UnionFind.make (Valtype ty))
          | None -> (
              match Tbl.find_opt ctx.functions idx with
              | Some _ -> assert false (*ZZZ*)
              | None -> assert false)))
  | Tee (idx, i') -> (
      let* () = instruction ctx i' in
      (*ZZZ local *)
      match StringMap.find_opt idx.desc ctx.locals with
      | Some ty ->
          let typ = UnionFind.make (Valtype ty) in
          let* () = pop ctx typ in
          push i.info typ
      | None -> (
          match Tbl.find_opt ctx.globals idx with
          | Some _ -> assert false
          (*ZZZ*)
          | None -> (
              match Tbl.find_opt ctx.functions idx with
              | Some _ -> assert false (*ZZZ*)
              | None -> assert false)))
  | Call ({ desc = StructGet (a, { desc = "fill"; _ }); _ }, [ i; v; n ]) -> (
      let* () = instruction ctx a in
      let* () = instruction ctx i in
      let* () = instruction ctx v in
      let* () = instruction ctx n in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* ty' = pop_any in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some ty -> (
          match UnionFind.find ty with
          | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
              match Tbl.find ctx.types ty with
              | _, Array typ -> (
                  assert typ.mut;
                  let typ = unpack_type typ in
                  match ty' with
                  | None -> return ()
                  | Some ty' ->
                      let ty =
                        UnionFind.make
                          (Valtype
                             { typ; internal = valtype ctx.type_context typ })
                      in
                      let ok = subtype ctx ty' ty in
                      if not ok then
                        Format.eprintf "%a <: %a@." output_inferred_type ty'
                          output_inferred_type ty;
                      assert ok;
                      return ())
              | _ -> assert false)
          | _ -> assert false (*ZZZ*)))
  | Call ({ desc = StructGet (a1, { desc = "copy"; _ }); _ }, [ i1; a2; i2; n ])
    -> (
      let* () = instruction ctx a1 in
      let* () = instruction ctx i1 in
      let* () = instruction ctx a2 in
      let* () = instruction ctx i2 in
      let* () = instruction ctx n in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* ty' = pop_any in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* ty = pop_any in
      match (ty, ty') with
      | None, _ | _, None -> return ()
      | Some ty, Some ty' -> (
          match (UnionFind.find ty, UnionFind.find ty') with
          | ( Valtype { typ = Ref { typ = Type ty; _ }; _ },
              Valtype { typ = Ref { typ = Type ty'; _ }; _ } ) -> (
              match (Tbl.find ctx.types ty, Tbl.find ctx.types ty') with
              | (_, Array typ), (_, Array typ') ->
                  assert typ.mut;
                  let ok = storage_subtype ctx typ'.typ typ.typ in
                  assert ok;
                  return ()
              | _ -> assert false)
          | _ -> assert false (*ZZZ*)))
  | Call ({ desc = Get { desc = "rotl" | "rotr"; _ }; _ }, [ i1; i2 ]) -> (
      let* () = instruction ctx i1 in
      let* () = instruction ctx i2 in
      let* typ2 = pop_any in
      let* typ1 = pop_any in
      match (typ1, typ2) with
      | Some typ1, Some typ2 -> check_int_bin_op i typ1 typ2
      | _ -> assert false (*ZZZ*))
  | Call ({ desc = Get { desc = "copysign" | "min" | "max"; _ }; _ }, [ i1; i2 ])
    -> (
      let* () = instruction ctx i1 in
      let* () = instruction ctx i2 in
      let* typ2 = pop_any in
      let* typ1 = pop_any in
      match (typ1, typ2) with
      | Some typ1, Some typ2 -> check_float_bin_op i typ1 typ2
      | _ -> assert false (*ZZZ*))
  | Call (i', l) -> (
      let* () = instructions ctx l in
      let* () = instruction ctx i' in
      let* ty = pop_any in
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty -> (
          match UnionFind.find ty with
          | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
              match Tbl.find ctx.types ty with
              | _, Func typ ->
                  let* () =
                    pop_args ctx
                      (Array.to_list
                         (Array.map
                            (fun typ ->
                              UnionFind.make
                                (Valtype
                                   {
                                     typ;
                                     internal = valtype ctx.type_context typ;
                                   }))
                            typ.params))
                  in
                  push_results
                    (Array.to_list
                       (Array.map
                          (fun typ ->
                            ( i.info,
                              (*ZZZ*)
                              UnionFind.make
                                (Valtype
                                   {
                                     typ;
                                     internal = valtype ctx.type_context typ;
                                   }) ))
                          typ.results))
              | _ -> assert false)
          | _ -> assert false (*ZZZ*)))
  | TailCall (i', l) -> (
      let* () = instructions ctx l in
      let* () = instruction ctx i' in
      let* ty = pop_any in
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty -> (
          match UnionFind.find ty with
          | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
              match Tbl.find ctx.types ty with
              | _, Func typ ->
                  let* () =
                    pop_args ctx
                      (Array.to_list
                         (Array.map
                            (fun typ ->
                              UnionFind.make
                                (Valtype
                                   {
                                     typ;
                                     internal = valtype ctx.type_context typ;
                                   }))
                            typ.params))
                  in
                  with_empty_stack
                    (let* () =
                       push_results
                         (Array.to_list
                            (Array.map
                               (fun typ ->
                                 ( i.info,
                                   (*ZZZ*)
                                   UnionFind.make
                                     (Valtype
                                        {
                                          typ;
                                          internal =
                                            valtype ctx.type_context typ;
                                        }) ))
                               typ.results))
                     in
                     pop_args ctx
                       (List.map
                          (fun typ -> UnionFind.make (Valtype typ))
                          ctx.return_types));
                  unreachable
              | _ -> assert false)
          | _ ->
              Format.eprintf "%a@." Output.instr i;
              assert false (*ZZZ*)))
  | String (ty, _) -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          (match snd (Tbl.find ctx.types ty) with
          | Func _ | Struct _ -> assert false (*ZZZ*)
          | Array _ -> ());
          let typ = Ref { nullable = false; typ = Type ty } in
          push i.info
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | Int _ -> push i.info (UnionFind.make Number)
  | Float _ -> push i.info (UnionFind.make Float)
  | Cast (i, typ) -> (
      let* () = instruction ctx i in
      let* ty' = pop_any in
      match ty' with
      | None -> (
          match typ with
          | Valtype typ ->
              let ty = valtype ctx.type_context typ in
              push i.info (UnionFind.make (Valtype { typ; internal = ty }))
          | Signedtype { typ; _ } ->
              let typ, (ty : Internal.valtype) =
                match typ with
                | `I32 -> (I32, I32)
                | `I64 -> (I64, I64)
                | `F32 -> (F32, F32)
                | `F64 -> (F64, F64)
              in
              push i.info (UnionFind.make (Valtype { typ; internal = ty })))
      | Some ty' -> (
          match typ with
          | Valtype typ ->
              let ty = valtype ctx.type_context typ in
              let ok = cast ctx ty' typ in
              if not ok then (
                Format.eprintf "%a@." Output.instr i;
                Format.eprintf "cast %a => %a@." output_inferred_type ty'
                  Output.valtype typ);
              assert ok;
              push i.info (UnionFind.make (Valtype { typ; internal = ty }))
          | Signedtype { typ; _ } ->
              let ok = signed_cast ctx ty' typ in
              if not ok then (
                Format.eprintf "%a@." Output.instr i;
                Format.eprintf "signed cast %a => %s@." output_inferred_type ty'
                  (match typ with
                  | `I32 -> "i32"
                  | `I64 -> "i64"
                  | `F32 -> "f32"
                  | `F64 -> "f64"));
              assert ok;
              let typ, (ty : Internal.valtype) =
                match typ with
                | `I32 -> (I32, I32)
                | `I64 -> (I64, I64)
                | `F32 -> (F32, F32)
                | `F64 -> (F64, F64)
              in
              push i.info (UnionFind.make (Valtype { typ; internal = ty }))))
  | Test (i, ty) ->
      let* () = instruction ctx i in
      let typ = Ref { nullable = true; typ = top_heap_type ctx ty.typ } in
      let* () =
        pop ctx
          (UnionFind.make
             (Valtype { typ; internal = valtype ctx.type_context typ }))
      in
      push i.info (UnionFind.make (Valtype { typ = I32; internal = I32 }))
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
                    (fun prev (name, _) ->
                      match
                        List.find_opt
                          (fun (idx, _) -> name.desc = idx.desc)
                          fields
                      with
                      | None -> assert false (*ZZZ*)
                      | Some (_, i') ->
                          let* () = prev in
                          instruction ctx i')
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
          push i.info
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | StructDefault ty -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          (match snd (Tbl.find ctx.types ty) with
          | Struct fields ->
              (*ZZZ*)
              assert (Array.for_all (fun (_, ty) -> field_has_default ty) fields)
          | _ -> (*ZZZ *) assert false);
          let typ = Ref { nullable = false; typ = Type ty } in
          push i.info
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | StructGet (i', field) -> (
      let* () = instruction ctx i' in
      let* ty = pop_any in
      match ty with
      | None ->
          Format.eprintf "%a@." Output.instr i;
          assert false (*ZZZ*)
      | Some ty -> (
          match (UnionFind.find ty, field.desc) with
          | Valtype { typ = Ref { typ = Type ty; _ }; _ }, _ -> (
              match Tbl.find ctx.types ty with
              | _, Struct typ -> (
                  match
                    Array.find_map
                      (fun (nm, typ) ->
                        if nm.desc = field.desc then Some typ else None)
                      typ
                  with
                  | None -> assert false
                  | Some typ -> push i.info (UnionFind.make (fieldtype ctx typ))
                  )
              | _, Array _ when field.desc = "length" ->
                  push i.info
                    (UnionFind.make (Valtype { typ = I32; internal = I32 }))
              | _ -> assert false)
          | (Null | Valtype { typ = Ref { typ = Array; _ }; _ }), "length" ->
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Valtype { typ = I32; _ }, "from_bits" ->
              push i.info
                (UnionFind.make (Valtype { typ = F32; internal = F32 }))
          | Valtype { typ = I64; _ }, "from_bits" ->
              push i.info
                (UnionFind.make (Valtype { typ = F64; internal = F64 }))
          | Valtype { typ = F32; _ }, "to_bits" ->
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Valtype { typ = F64; _ }, "to_bits" ->
              push i.info
                (UnionFind.make (Valtype { typ = I64; internal = I64 }))
          | ( ((Number | Int | Valtype { typ = I32 | I64; _ }) as ty'),
              ("clz" | "ctz" | "popcnt") ) ->
              if ty' = Number then UnionFind.set ty Int;
              push i.info ty
          | ( ((Number | Float | Valtype { typ = F32 | F64; _ }) as ty'),
              ("abs" | "ceil" | "floor" | "trunc" | "nearest" | "sqrt") ) ->
              if ty' = Number then UnionFind.set ty Float;
              push i.info ty
          | _ ->
              Format.eprintf "??? %a %s@." output_inferred_type ty field.desc;
              assert false (*ZZZ*)))
  | StructSet (i, field, i') -> (
      let* () = instruction ctx i in
      let* () = instruction ctx i' in
      let* ty' = pop_any in
      let* ty = pop_any in
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty -> (
          match UnionFind.find ty with
          | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
              match Tbl.find ctx.types ty with
              | _, Struct typ -> (
                  match
                    Array.find_map
                      (fun (nm, typ) ->
                        if nm.desc = field.desc then Some typ else None)
                      typ
                  with
                  | None ->
                      Format.eprintf "struct.set %s/%s@." ty.desc field.desc;
                      assert false
                  | Some typ -> (
                      assert typ.mut;
                      let typ = unpack_type typ in
                      let ty =
                        UnionFind.make
                          (Valtype
                             { typ; internal = valtype ctx.type_context typ })
                      in
                      match ty' with
                      | None -> assert false (*ZZZ*)
                      | Some ty' ->
                          let ok = subtype ctx ty' ty in
                          if not ok then
                            Format.eprintf "%a <: %a@." output_inferred_type ty'
                              output_inferred_type ty;
                          assert ok;
                          return ()))
              | _ -> assert false (*ZZZ*))
          | _ -> assert false (*ZZZ*)))
  | Array (ty, i1, i2) -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          let* () = instruction ctx i1 in
          let* () = instruction ctx i2 in
          let* () =
            pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          in
          let* () =
            match snd (Tbl.find ctx.types ty) with
            | Array field' ->
                let typ = unpack_type field' in
                pop ctx
                  (UnionFind.make
                     (Valtype { typ; internal = valtype ctx.type_context typ }))
            | _ -> (*ZZZ *) assert false
          in
          let typ = Ref { nullable = false; typ = Type ty } in
          push i.info
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | ArrayDefault (ty, i) -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          let* () = instruction ctx i in
          let* () =
            pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          in
          (match snd (Tbl.find ctx.types ty) with
          | Array field -> assert (field_has_default field)
          | _ -> (*ZZZ *) assert false);
          let typ = Ref { nullable = false; typ = Type ty } in
          push i.info
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | ArrayFixed (ty, instrs) -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          let* () =
            match snd (Tbl.find ctx.types ty) with
            | Array field' ->
                let* () =
                  List.fold_left
                    (fun prev i' ->
                      let* () = prev in
                      instruction ctx i')
                    (return ()) instrs
                in
                let typ = unpack_type field' in
                let typ = { typ; internal = valtype ctx.type_context typ } in
                repeat (List.length instrs)
                  (pop ctx (UnionFind.make (Valtype typ)))
            | _ -> (*ZZZ *) assert false
          in
          let typ = Ref { nullable = false; typ = Type ty } in
          push i.info
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | ArrayGet (i1, i2) -> (
      let* () = instruction ctx i1 in
      let* () = instruction ctx i2 in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* ty = pop_any in
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty -> (
          match UnionFind.find ty with
          | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
              match Tbl.find ctx.types ty with
              | _, Array typ -> push i.info (UnionFind.make (fieldtype ctx typ))
              | _ -> assert false)
          | _ -> assert false (*ZZZ*)))
  | ArraySet (i1, i2, i3) -> (
      let* () = instruction ctx i1 in
      let* () = instruction ctx i2 in
      let* () = instruction ctx i3 in
      let* ty3 = pop_any in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some ty -> (
          match UnionFind.find ty with
          | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
              match Tbl.find ctx.types ty with
              | _, Array typ -> (
                  assert typ.mut;
                  let typ = unpack_type typ in
                  match ty3 with
                  | None -> return ()
                  | Some ty' ->
                      let ty =
                        UnionFind.make
                          (Valtype
                             { typ; internal = valtype ctx.type_context typ })
                      in
                      let ok = subtype ctx ty' ty in
                      if not ok then
                        Format.eprintf "%a <: %a@." output_inferred_type ty'
                          output_inferred_type ty;
                      assert ok;
                      return ())
              | _ -> assert false)
          | _ -> assert false (*ZZZ*)))
  | BinOp (op, i1, i2) -> (
      let* () = instruction ctx i1 in
      let* () = instruction ctx i2 in
      let* typ2 = pop_any in
      let* typ1 = pop_any in
      match (typ1, typ2) with
      | Some typ1, Some typ2 -> (
          match op with
          | Eq ->
              (match (UnionFind.find typ1, UnionFind.find typ2) with
              | ( Valtype { internal = Ref _ as ty1; _ },
                  Valtype { internal = Ref _ as ty2; _ } ) ->
                  assert (
                    Wasm.Types.val_subtype ctx.subtyping_info ty1
                      (Ref { nullable = true; typ = Eq }));
                  assert (
                    Wasm.Types.val_subtype ctx.subtyping_info ty2
                      (Ref { nullable = true; typ = Eq }))
              | Valtype { internal = Ref _ as ty1; _ }, Null ->
                  assert (
                    Wasm.Types.val_subtype ctx.subtyping_info ty1
                      (Ref { nullable = true; typ = Eq }));
                  UnionFind.merge typ1 typ2 (UnionFind.find typ2)
              | Null, Valtype { internal = Ref _ as ty2; _ } ->
                  assert (
                    Wasm.Types.val_subtype ctx.subtyping_info ty2
                      (Ref { nullable = true; typ = Eq }));
                  UnionFind.merge typ1 typ2 (UnionFind.find typ2)
              | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
              | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }, Valtype { internal = F64; _ } ->
                  ()
              | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int)
              | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
              | Number, Number ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ1)
              | (Number | Int), Valtype { internal = I32 | I64; _ }
              | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ2)
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Add | Sub | Mul ->
              (match (UnionFind.find typ1, UnionFind.find typ2) with
              | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
              | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }, Valtype { internal = F64; _ } ->
                  ()
              | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int)
              | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
              | Number, Number ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ1)
              | (Number | Int), Valtype { internal = I32 | I64; _ }
              | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ2)
              | _ -> assert false (*ZZZ*));
              push i.info typ1
          | Div (Some _) | Rem _ | And | Or | Xor | Shl | Shr _ ->
              check_int_bin_op i typ1 typ2
          | Div None -> check_float_bin_op i typ1 typ2
          | Lt (Some _) | Gt (Some _) | Le (Some _) | Ge (Some _) ->
              (match (UnionFind.find typ1, UnionFind.find typ2) with
              | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
              | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int) ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ1)
              | (Number | Int), Valtype { internal = I32 | I64; _ } ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ2)
              | Number, Number -> UnionFind.merge typ1 typ2 Int
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Lt None | Gt None | Le None | Ge None ->
              (match (UnionFind.find typ1, UnionFind.find typ2) with
              | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }, Valtype { internal = F64; _ }
              | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
                ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ1)
              | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ2)
              | Number, Number -> UnionFind.merge typ1 typ2 Float
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Ne ->
              (match (UnionFind.find typ1, UnionFind.find typ2) with
              | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
              | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }, Valtype { internal = F64; _ } ->
                  ()
              | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int)
              | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
              | Number, Number ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ1)
              | (Number | Int), Valtype { internal = I32 | I64; _ }
              | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                  UnionFind.merge typ1 typ2 (UnionFind.find typ2)
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 })))
      | Some typ, None | None, Some typ -> (
          match op with
          | Eq ->
              (match UnionFind.find typ with
              | Valtype { internal = Ref _ as ty; _ } ->
                  assert (
                    Wasm.Types.val_subtype ctx.subtyping_info ty
                      (Ref { nullable = true; typ = Eq }))
              | Null ->
                  UnionFind.set typ
                    (Valtype
                       {
                         typ = Ref { nullable = true; typ = Eq };
                         internal = Ref { nullable = true; typ = Eq };
                       })
              | Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }
              | Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }
              | Number | Int | Float ->
                  ()
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Add | Sub | Mul ->
              (match UnionFind.find typ with
              | Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }
              | Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }
              | Number | Int | Float ->
                  ()
              | _ -> assert false (*ZZZ*));
              push i.info typ
          | Div (Some _) | Rem _ | And | Or | Xor | Shl | Shr _ ->
              check_int_bin_op i typ typ
          | Div None -> check_float_bin_op i typ typ
          | Lt (Some _) | Gt (Some _) | Le (Some _) | Ge (Some _) ->
              (match UnionFind.find typ with
              | Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }
              | Number | Int | Float ->
                  ()
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Lt None | Gt None | Le None | Ge None ->
              (match UnionFind.find typ with
              | Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }
              | Number | Int | Float ->
                  ()
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Ne ->
              (match UnionFind.find typ with
              | Valtype { internal = I32; _ }
              | Valtype { internal = I64; _ }
              | Valtype { internal = F32; _ }
              | Valtype { internal = F64; _ }
              | Number | Int | Float ->
                  ()
              | _ -> assert false (*ZZZ*));
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 })))
      | None, None -> (
          match op with
          | Add | Sub | Mul -> push i.info (UnionFind.make Number)
          | Div (Some _) | Rem _ | And | Or | Xor | Shl | Shr _ ->
              push i.info (UnionFind.make Int)
          | Div None -> push i.info (UnionFind.make Float)
          | Eq
          | Lt (Some _)
          | Gt (Some _)
          | Le (Some _)
          | Ge (Some _)
          | Lt None
          | Gt None
          | Le None
          | Ge None
          | Ne ->
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))))
  | UnOp (op, i') -> (
      let* () = instruction ctx i' in
      let* typ = pop_any in
      match typ with
      | Some typ -> (
          match op with
          | Not ->
              (match UnionFind.find typ with
              | Valtype { internal = I32 | I64 | Ref _; _ } | Null | Int -> ()
              | Number -> UnionFind.set typ Int
              | _ -> assert false);
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Neg | Pos ->
              (match UnionFind.find typ with
              | Valtype { internal = I32 | I64 | F32 | F64; _ }
              | Int | Float | Number ->
                  ()
              | _ -> assert false);
              push i.info typ)
      | None -> (
          match op with
          | Not ->
              push i.info
                (UnionFind.make (Valtype { typ = I32; internal = I32 }))
          | Neg | Pos -> push i.info (UnionFind.make Number)))
  (*
  | BinOp of binop * instr * instr
  | UnOp of unop * instr
*)
  | Let ([ (Some name, Some typ) ], None) ->
      let typ = { typ; internal = valtype ctx.type_context typ } in
      ctx.locals <- StringMap.add name.desc typ ctx.locals;
      return ()
  | Let ([ (None, None) ], Some i') ->
      let* () = instruction ctx i' in
      let* _ = pop_any in
      return ()
  (*
  | Let of (idx option * valtype option) list * instr option
*)
  | Br (label, i') ->
      (* Sequence of instructions *)
      let* () =
        match i' with Some i' -> instruction ctx i' | None -> return ()
      in
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      let* () = pop_args ctx params in
      unreachable
  | Br_if (label, i') ->
      let* () = instruction ctx i' in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      let* () = pop_args ctx params in
      push_results (List.map (fun p -> (i.info, p) (*ZZZ*)) params)
  | Br_table (labels, i') ->
      let* () = instruction ctx i' in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let len = List.length (branch_target ctx (List.hd labels)) in
      let* () =
        with_current_stack (fun st ->
            List.iter
              (fun label ->
                let params = branch_target ctx label in
                assert (List.length params = len);
                (*ZZZ*)
                ignore
                  (pop_args ctx
                     (List.map (fun typ -> UnionFind.make (Valtype typ)) params)
                     st))
              labels)
      in
      unreachable
  | Br_on_null (idx, i') -> (
      let* () = instruction ctx i' in
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some typ -> (
          let typ = UnionFind.find typ in
          match typ with
          | Valtype
              {
                typ = Ref { nullable = _; typ; _ };
                internal = Ref { nullable = _; typ = ityp; _ };
              } ->
              let params = branch_target ctx idx in
              let params =
                List.map (fun typ -> UnionFind.make (Valtype typ)) params
              in
              let* () = pop_args ctx params in
              let* () =
                push_results (List.map (fun p -> (i.info, p) (*ZZZ*)) params)
              in
              push i.info
                (UnionFind.make
                   (Valtype
                      {
                        typ = Ref { nullable = false; typ };
                        internal = Ref { nullable = false; typ = ityp };
                      }))
          | _ -> assert false (*ZZZ*)))
  | Br_on_non_null (idx, i') -> (
      let* () = instruction ctx i' in
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some typ -> (
          let typ = UnionFind.find typ in
          match typ with
          | Valtype
              {
                typ = Ref { nullable = _; typ; _ };
                internal = Ref { nullable = _; typ = ityp; _ };
              } ->
              let params = branch_target ctx idx in
              let params =
                List.map (fun typ -> UnionFind.make (Valtype typ)) params
              in
              let* () =
                push i.info
                  (UnionFind.make
                     (Valtype
                        {
                          typ = Ref { nullable = false; typ };
                          internal = Ref { nullable = false; typ = ityp };
                        }))
              in
              let* () = pop_args ctx params in
              let* () =
                push_results (List.map (fun p -> (i.info, p) (*ZZZ*)) params)
              in
              let* _ = pop_any in
              return ()
          | _ -> assert false (*ZZZ*)))
  | Br_on_cast (label, ty, i') -> (
      let* () = instruction ctx i' in
      let* typ' = pop_any in
      let ityp = reftype ctx.type_context ty in
      let typ =
        UnionFind.make (Valtype { typ = Ref ty; internal = Ref ityp })
      in
      let* () = push i.info typ in
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      let* () = pop_args ctx params in
      let* () = push_results (List.map (fun p -> (i.info, p) (*ZZZ*)) params) in
      let* _ = pop_any in
      match typ' with
      | None -> push_poly i.info None
      | Some typ' ->
          let typ =
            match UnionFind.find typ' with
            | Valtype { typ = Ref ty'; internal = Ref ityp' } ->
                Valtype
                  {
                    typ = Ref (diff_ref_type ty' ty);
                    internal = Ref (diff_ref_type_internal ityp' ityp);
                  }
            | _ -> assert false
          in
          (*ZZZ*)
          push i.info (UnionFind.make typ))
  | Br_on_cast_fail (label, ty, i') ->
      let* () = instruction ctx i' in
      let* typ' = pop_any in
      let ityp = reftype ctx.type_context ty in
      let typ =
        UnionFind.make (Valtype { typ = Ref ty; internal = Ref ityp })
      in
      let* () =
        match typ' with
        | None -> push_poly i.info None
        | Some typ' ->
            let typ =
              match UnionFind.find typ' with
              | Valtype { typ = Ref ty'; internal = Ref ityp' } ->
                  Valtype
                    {
                      typ = Ref (diff_ref_type ty' ty);
                      internal = Ref (diff_ref_type_internal ityp' ityp);
                    }
              | _ -> assert false
            in
            push i.info
              (*ZZZ*)
              (UnionFind.make typ)
      in
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      let* () = pop_args ctx params in
      let* () = push_results (List.map (fun p -> (i.info, p) (*ZZZ*)) params) in
      let* _ = pop_any in
      push i.info typ
  | Throw (tag, lst) ->
      let* () = instructions ctx lst in
      let { params; results } = Tbl.find ctx.tags tag in
      assert (results = [||]);
      let* () =
        pop_args ctx
          (Array.to_list
             (Array.map
                (fun typ ->
                  UnionFind.make
                    (Valtype { typ; internal = valtype ctx.type_context typ }))
                params))
      in
      unreachable
  | ThrowRef i' ->
      let* () = instruction ctx i' in
      let* () =
        let typ = Ref { nullable = true; typ = Exn } in
        pop ctx
          (UnionFind.make
             (Valtype { typ; internal = valtype ctx.type_context typ }))
      in
      unreachable
  | NonNull i' -> (
      let* () = instruction ctx i' in
      let* ty = pop_any in
      match ty with
      | None -> return ()
      | Some typ -> (
          let typ = UnionFind.find typ in
          match typ with
          | Valtype
              {
                typ = Ref { nullable = _; typ; _ };
                internal = Ref { nullable = _; typ = ityp; _ };
              } ->
              push i.info
                (UnionFind.make
                   (Valtype
                      {
                        typ = Ref { nullable = false; typ };
                        internal = Ref { nullable = false; typ = ityp };
                      }))
          | _ -> assert false (*ZZZ*)))
  | Return i' ->
      (*ZZZ List of instructions? *)
      let* () =
        match i' with Some i' -> instruction ctx i' | None -> return ()
      in
      let* () =
        pop_args ctx
          (List.map (fun typ -> UnionFind.make (Valtype typ)) ctx.return_types)
      in
      unreachable
  | Sequence l -> instructions ctx l
  | Select (i1, i2, i3) -> (
      let* () = instruction ctx i2 in
      let* () = instruction ctx i3 in
      let* () = instruction ctx i1 in
      let* () =
        pop ctx (UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      let* ty1 = pop_any in
      let* ty2 = pop_any in
      match (ty1, ty2) with
      | None, None -> return ()
      | Some ty, None | None, Some ty -> push i.info ty
      | Some ty1, Some ty2 -> (
          match (UnionFind.find ty1, UnionFind.find ty2) with
          | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
          | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
          | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
          | Valtype { internal = F64; _ }, Valtype { internal = F64; _ } ->
              push i.info ty2
          | (Int | Number), (Int | Valtype { internal = I32 | I64; _ })
          | (Float | Number), (Float | Valtype { internal = F32 | F64; _ })
          | Number, Number ->
              UnionFind.merge ty1 ty2 (UnionFind.find ty2);
              push i.info ty2
          | ( (Valtype { internal = I32; _ } | Valtype { internal = I64; _ }),
              (Int | Number) )
          | ( (Valtype { internal = F32; _ } | Valtype { internal = F64; _ }),
              (Float | Number) )
          | (Int | Float), Number ->
              UnionFind.merge ty1 ty2 (UnionFind.find ty1);
              push i.info ty1
          | Valtype { internal = typ1; _ }, Valtype { internal = typ2; _ }
            when typ1 = typ2 ->
              (*ZZZ fragile *)
              push i.info ty1
          | Valtype { typ = typ1; _ }, Valtype { typ = typ2; _ } ->
              Format.eprintf "AAAA %a %a@." Output.valtype typ1 Output.valtype
                typ2;
              assert false
          | _ -> (*ZZZ*) assert false))
  | _ ->
      Format.eprintf "%a@." Output.instr i;
      assert false

and instructions ctx l =
  match l with
  | [] -> return ()
  | i :: r ->
      let* () = instruction ctx i in
      instructions ctx r

and block ctx loc label params results br_params block =
  with_empty_stack
    (let* () = push_results (List.map (fun ty -> (loc, ty)) params) in
     let* () =
       instructions
         { ctx with control_types = (label, br_params) :: ctx.control_types }
         block
     in
     pop_args ctx (List.map (fun typ -> UnionFind.make (Valtype typ)) results))

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

let check_type_definitions ctx =
  (*ZZZ In-order check? *)
  Tbl.iter ctx.types (fun _ (i, _) ->
      let ty = Wasm.Types.get_subtype ctx.subtyping_info i in
      match ty.supertype with
      | None -> ()
      | Some j -> (
          let ty' = Wasm.Types.get_subtype ctx.subtyping_info j in
          assert (not ty'.final);
          match (ty.typ, ty'.typ) with
          | ( Func { params; results },
              Func { params = params'; results = results' } ) ->
              assert (Array.length params = Array.length params');
              assert (Array.length results = Array.length results');
              Array.iter2
                (fun p p' ->
                  assert (Wasm.Types.val_subtype ctx.subtyping_info p' p))
                params params';
              Array.iter2
                (fun r r' ->
                  assert (Wasm.Types.val_subtype ctx.subtyping_info r r'))
                results results'
          | Struct fields, Struct fields' ->
              assert (Array.length fields' <= Array.length fields);
              for i = 0 to Array.length fields' - 1 do
                assert (field_subtype ctx fields.(i) fields'.(i))
              done
          | Array field, Array field' -> assert (field_subtype ctx field field')
          | Func _, (Struct _ | Array _)
          | Struct _, (Func _ | Array _)
          | Array _, (Func _ | Struct _) ->
              assert false))

let globals type_context ctx fields =
  List.iter
    (fun field ->
      match field with
      | Global { name; mut; typ = Some typ; def; _ } ->
          (*ZZZ check constant instructions *)
          (*ZZZ handle typ= None *)
          with_empty_stack
            (let* () = instruction ctx def in
             let internal = valtype type_context typ in
             let typ = { typ; internal } in
             Tbl.add ctx.globals name (mut, typ);
             pop ctx (UnionFind.make (Valtype typ)))
      | _ -> ())
    fields

let functions ctx fields =
  List.iter
    (fun field ->
      match field with
      | Func { name; sign; body; _ } ->
          let func_typ =
            match
              let func_typ = Tbl.find ctx.functions name in
              Tbl.find ctx.types { name with desc = snd func_typ }
            with
            | _, Func typ -> typ
            | _ -> assert false
          in
          let return_types =
            Array.to_list
              (Array.map
                 (fun typ -> { typ; internal = valtype ctx.type_context typ })
                 func_typ.results)
          in
          let locals = ref StringMap.empty in
          (match sign with
          | Some { named_params; _ } ->
              List.iter
                (fun (id, typ) ->
                  match id with
                  | Some id ->
                      locals :=
                        StringMap.add id.desc
                          { typ; internal = valtype ctx.type_context typ }
                          !locals
                  | None -> ())
                named_params
          | _ -> ());
          if false then Format.eprintf "=== %s@." name.desc;
          with_empty_stack
            (let ctx =
               {
                 ctx with
                 locals = !locals;
                 control_types = [ (fst body, return_types) ];
                 return_types;
               }
             in
             let* () = instructions ctx (snd body) in
             pop_args ctx
               (List.map (fun typ -> UnionFind.make (Valtype typ)) return_types))
      | _ -> ())
    fields

let funsig _ctx sign =
  (*ZZZ Check signature (unique names) *)
  {
    params = Array.of_list (List.map snd sign.named_params);
    results = Array.of_list sign.results;
  }

let fundecl ctx name typ sign =
  match typ with
  | Some typ -> (*ZZZ Check signature*) (fst (Tbl.find ctx.types typ), typ.desc)
  | None -> (
      match sign with
      | Some sign ->
          let name = { name with desc = "func:" ^ name.desc } in
          ( add_type ctx.type_context
              [|
                ( name,
                  {
                    supertype = None;
                    typ = Func (funsig ctx sign);
                    final = true;
                  } );
              |],
            name.desc )
      | None -> assert false (*ZZZ*))

let f fields =
  let type_context =
    {
      internal_types = Wasm.Types.create ();
      types = Tbl.make (Namespace.make ()) "type";
    }
  in
  List.iter
    (fun (field : _ modulefield) ->
      match field with
      | Type rectype -> ignore (add_type type_context rectype)
      | _ -> ())
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
      locals = StringMap.empty;
      control_types = [];
      return_types = [];
    }
  in
  check_type_definitions ctx;
  List.iter
    (fun field ->
      match field with
      | Fundecl { name; typ; sign; _ } ->
          (*ZZZ Check existing*)
          Tbl.add ctx.functions name (fundecl ctx name typ sign)
      | GlobalDecl { name; mut; typ; _ } ->
          Tbl.add ctx.globals name
            (mut, { internal = valtype type_context typ; typ })
      | Func { name; typ; sign; _ } ->
          (*ZZZ Check existing*)
          Tbl.add ctx.functions name (fundecl ctx name typ sign)
      | Tag { name; typ; sign; _ } ->
          let typ =
            match (typ, sign) with
            | Some typ, _ -> (
                match snd (Tbl.find ctx.types typ) with
                | Func typ -> typ
                | _ -> assert false)
            | None, Some sign -> funsig ctx sign
            | None, None -> assert false (*ZZZ*)
          in
          Tbl.add ctx.tags name typ
      | _ -> ())
    fields;
  let ctx =
    {
      ctx with
      subtyping_info = Wasm.Types.subtyping_info type_context.internal_types;
    }
  in
  globals type_context ctx fields;
  if true then functions ctx fields;
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
