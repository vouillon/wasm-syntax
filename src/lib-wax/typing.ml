(*
TODO:
- we need to decide on an order to visit subexpression in structs
- fix typeuse validation (add a type if not already present)
- check that underscores are properly placed
- error messages
- locations on the heap when push several values?
- more methods rather than global functions (no ambiguity)?
  rotl(..), rotr(..), min(..), max(..), copysign(..)
- move lets at more appropriate places
- remove redundant type annotations/casts
- take into account that locals can shadow globals to get better local names
  (if a global is not used in a function, we can reuse its name)
- check constant expressions
- option to tighten casts to any/extern / eliminate redundant casts
  and type annotations
- framework for dealing with errors (that can handle several errors)

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
- br 'a (e1, ..., en) if cond   / if cond br 'a (e1, ..., en) / br_if 'a cond (...)

Misc:
- blocks in an expression context return one value;
  otherwise, no value by default

Explicit types?
   fn(..)->(..)
==> for function types
==> for call_indirect
(But we don't have a cast to a typeuse in WAT)
*)

open Ast

type typed_module_annotation = Ast.storagetype list * Ast.location

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
  | Any
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
  (*  memories : limits Tbl.t;*)
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
  | Any, _ -> true
  | _, Any -> assert false

let cast ctx ty ty' =
  (*ZZZ Cast between Any and Extern *)
  let ity = UnionFind.find ty in
  match (ity, ty') with
  | (Number | Int), Ref { typ = I31; _ } ->
      UnionFind.set ty (Valtype { typ = I32; internal = I32 });
      true
  | (Number | Int | Any), I32 | Int, F32 ->
      UnionFind.set ty (Valtype { typ = I32; internal = I32 });
      true
  | (Number | Int | Any), I64 | Int, F64 ->
      UnionFind.set ty (Valtype { typ = I64; internal = I64 });
      true
  | (Number | Float | Any), F32 | Float, I32 ->
      UnionFind.set ty (Valtype { typ = F32; internal = F32 });
      true
  | (Number | Float | Any), F64 | Float, I64 ->
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
  | Any, Ref { typ = ty'; nullable } ->
      let ty' = Ref { nullable; typ = top_heap_type ctx ty' } in
      let ity' = valtype ctx.type_context ty' in
      UnionFind.set ty (Valtype { typ = ty'; internal = ity' });
      true
  | Any, V128 ->
      UnionFind.set ty (Valtype { typ = V128; internal = V128 });
      true
  | Any, Tuple _ -> assert false

let signed_cast ctx ty ty' =
  let ity = UnionFind.find ty in
  match (ity, ty') with
  | Any, `I32 ->
      UnionFind.set ty (Valtype { typ = F32; internal = F32 });
      true
  | Any, (`F32 | `F64) ->
      UnionFind.set ty (Valtype { typ = I32; internal = I32 });
      true
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
  | (Any | Number | Int), `I64 ->
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
  | Cons of location * inferred_type UnionFind.t * stack

type state = { stack : stack; available : bool }

let output_inferred_type f ty =
  match UnionFind.find ty with
  | Any -> Format.fprintf f "any"
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
      Format.fprintf f "@ %a%a" output_inferred_type ty output_stack st

let print_stack st =
  Format.eprintf "@[Stack:%a@]@." output_stack st.stack;
  (st, ())

let _ = print_stack

let unreachable e st =
  let _, v = e st in
  ({ stack = Unreachable; available = true }, v)

let return v st = (st, v)

let ( let* ) e f st =
  let st, v = e st in
  f v st

let pop_any _i st =
  assert st.available;
  (*ZZZ*)
  match st.stack with
  | Unreachable -> ({ st with stack = Unreachable }, UnionFind.make Any)
  | Cons (_, ty, r) -> ({ st with stack = r }, ty)
  | Empty -> assert false (*ZZZ*)

let pop ctx ty st =
  match st.stack with
  | Unreachable -> ({ st with stack = Unreachable }, ())
  | Cons (_, ty', r) ->
      let ok = subtype ctx ty' ty in
      if not ok then
        Format.eprintf "%a <: %a@." output_inferred_type ty'
          output_inferred_type ty;
      assert ok;
      ({ st with stack = r }, ())
  | Empty -> assert false

let rec pop_args ctx args =
  match args with
  | [] -> return ()
  | ty :: rem ->
      let* () = pop_args ctx rem in
      pop ctx ty

let push loc ty st = ({ stack = Cons (loc, ty, st.stack); available = true }, ())

let rec push_results results =
  match results with
  | [] ->
      if false then prerr_endline "PUSH";
      return ()
  | (loc, ty) :: rem ->
      let* () = push loc ty in
      push_results rem

let with_empty_stack f =
  if false then prerr_endline "START";
  let st, res = f { stack = Empty; available = true } in
  if false then prerr_endline "DONE";
  match st.stack with
  | Cons _ ->
      Format.eprintf "@[<2>Stack:%a@]@." output_stack st.stack;
      assert false
  | Empty | Unreachable -> res

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

let check_int_bin_op typ1 typ2 =
  (match (UnionFind.find typ1, UnionFind.find typ2) with
  | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
  | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
  | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int) ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ1)
  | (Number | Int), Valtype { internal = I32 | I64; _ } ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ2)
  | Number, Number -> UnionFind.merge typ1 typ2 Int
  | _ -> assert false (*ZZZ*));
  typ1

let check_float_bin_op typ1 typ2 =
  (match (UnionFind.find typ1, UnionFind.find typ2) with
  | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
  | Valtype { internal = F64; _ }, Valtype { internal = F64; _ }
  | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float) ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ1)
  | (Number | Float), Valtype { internal = F32 | F64; _ } ->
      UnionFind.merge typ1 typ2 (UnionFind.find typ2)
  | Number, Number -> UnionFind.merge typ1 typ2 Float
  | _ -> assert false (*ZZZ*));
  typ1

let field_has_default (ty : fieldtype) =
  match ty.typ with
  | Packed _ -> true
  | Value ty -> (
      match ty with
      | I32 | I64 | F32 | F64 | V128 -> true
      | Ref { nullable; _ } -> nullable
      | Tuple _ -> assert false)

let return_statement ?(pop = false) (i : location instr)
    (desc : (inferred_type UnionFind.t list * location) instr_desc) ty st =
  if false then
    Format.eprintf "%b %b -> %b @[%a@]@." pop st.available
      (st.available && (pop || ty = []))
      Output.instr i;
  ( { st with available = st.available && (pop || ty = []) },
    { desc; info = (ty, i.info) } )

let return_expression ?pop i desc ty = return_statement ?pop i desc [ ty ]

let expression_type i =
  match i.info with [ ty ], _ -> ty | _ -> assert false (*ZZZ*)

let check_subtype ctx ty' ty =
  let ok = subtype ctx ty' ty in
  if not ok then
    Format.eprintf "%a <: %a@." output_inferred_type ty' output_inferred_type ty;
  assert ok

let check_subtypes ctx types' types =
  assert (List.length types' = List.length types);
  List.iter2 (fun ty' ty -> check_subtype ctx ty' ty) types' types

let check_type ctx i ty = check_subtype ctx (expression_type i) ty

let rec instruction ctx (i : location instr) : _ -> _ * (_ * location) instr =
  (*
  let* () = print_stack in
*)
  if false then Format.eprintf "%a@." Output.instr i;
  match i.desc with
  | Block (label, bt, instrs) ->
      (*ZZZ Blocks take argument from the stack *)
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
      let instrs' = block ctx i.info label params results results instrs in
      return_statement i
        (Block (label, bt, instrs'))
        (List.map (fun typ -> UnionFind.make (Valtype typ)) results)
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
      let instrs' = block ctx i.info label params results params0 instrs in
      return_statement i
        (Loop (label, bt, instrs'))
        (List.map (fun typ -> UnionFind.make (Valtype typ)) results)
  | If (label, bt, i', if_block, else_block) ->
      let* i' = instruction ctx i' in
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
      let if_block' = block ctx i.info label params results results if_block in
      let else_block' =
        Option.map
          (fun b -> block ctx i.info label params results results b)
          else_block
      in
      return_statement i
        (If (label, bt, i', if_block', else_block'))
        (List.map (fun typ -> UnionFind.make (Valtype typ)) results)
  | Unreachable ->
      (* ZZZ Only at top_level *)
      return_statement i Unreachable [] |> unreachable
  | Nop ->
      (* ZZZ Only at top_level *)
      return_statement i Nop []
  | Pop ->
      let* ty = pop_any i in
      return_expression ~pop:true i Pop ty
  | Null -> return_expression i Null (UnionFind.make Null)
  | Get idx as desc ->
      let ty =
        match StringMap.find_opt idx.desc ctx.locals with
        | Some ty -> ty
        | None -> (
            match Tbl.find_opt ctx.globals idx with
            | Some (_, ty) -> ty
            | None -> (
                match Tbl.find_opt ctx.functions idx with
                | Some (ty, ty') ->
                    {
                      typ =
                        Ref { nullable = false; typ = Type (Ast.no_loc ty') };
                      internal = Ref { nullable = false; typ = Type ty };
                    }
                | None ->
                    Format.eprintf "%a@." Output.instr i;
                    assert false))
      in
      return_expression i desc (UnionFind.make (Valtype ty))
  | Set (None, i') ->
      let* i' = instruction ctx i' in
      return_statement i (Set (None, i')) []
  | Set (Some idx, i') ->
      let ty =
        match StringMap.find_opt idx.desc ctx.locals with
        | Some ty -> ty
        | None -> (
            match Tbl.find_opt ctx.globals idx with
            | Some (mut, ty) ->
                assert mut;
                (*ZZZ*)
                ty
            | None -> (
                match Tbl.find_opt ctx.functions idx with
                | Some _ -> assert false (*ZZZ*)
                | None -> assert false))
      in
      let* i' = instruction ctx i' in
      check_type ctx i' (UnionFind.make (Valtype ty));
      return_statement i (Set (Some idx, i')) []
  | Tee (idx, i') ->
      let* i' = instruction ctx i' in
      (*ZZZ local *)
      let ty =
        match StringMap.find_opt idx.desc ctx.locals with
        | Some ty -> UnionFind.make (Valtype ty)
        | None -> (
            match Tbl.find_opt ctx.globals idx with
            | Some _ -> assert false
            (*ZZZ*)
            | None -> (
                match Tbl.find_opt ctx.functions idx with
                | Some _ -> assert false (*ZZZ*)
                | None -> assert false))
      in
      check_type ctx i' ty;
      return_expression i (Tee (idx, i')) ty
  | Call
      ( ({ desc = StructGet (a, ({ desc = "fill"; _ } as meth)); _ } as func),
        [ j; v; n ] ) ->
      let* n' = instruction ctx n in
      let* v' = instruction ctx v in
      let* j' = instruction ctx j in
      let* a' = instruction ctx a in
      check_type ctx n' (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      check_type ctx j' (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      (match UnionFind.find (expression_type a') with
      | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
          match Tbl.find ctx.types ty with
          | _, Array typ ->
              assert typ.mut;
              let typ = unpack_type typ in
              let ty' = expression_type v' in
              let ty =
                UnionFind.make
                  (Valtype { typ; internal = valtype ctx.type_context typ })
              in
              let ok = subtype ctx ty' ty in
              if not ok then
                Format.eprintf "%a <: %a@." output_inferred_type ty'
                  output_inferred_type ty;
              assert ok
          | _ -> assert false)
      | _ -> assert false (*ZZZ*));
      return_statement i
        (Call
           ( { desc = StructGet (a', meth); info = ([], func.info) },
             [ j'; v'; n' ] ))
        []
  | Call
      ( ({ desc = StructGet (a1, ({ desc = "copy"; _ } as meth)); _ } as func),
        [ i1; a2; i2; n ] ) ->
      let* n' = instruction ctx n in
      let* i2' = instruction ctx i2 in
      let* a2' = instruction ctx a2 in
      let* i1' = instruction ctx i1 in
      let* a1' = instruction ctx a1 in
      check_type ctx n' (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      check_type ctx i2'
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      let ty' = expression_type a2' in
      check_type ctx i1'
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      let ty = expression_type a1' in
      (match (UnionFind.find ty, UnionFind.find ty') with
      | Any, _ | _, Any -> ()
      | ( Valtype { typ = Ref { typ = Type ty; _ }; _ },
          Valtype { typ = Ref { typ = Type ty'; _ }; _ } ) -> (
          match (Tbl.find ctx.types ty, Tbl.find ctx.types ty') with
          | (_, Array typ), (_, Array typ') ->
              assert typ.mut;
              let ok = storage_subtype ctx typ'.typ typ.typ in
              assert ok;
              ()
          | _ -> assert false)
      | _ -> assert false (*ZZZ*));
      return_statement i
        (Call
           ( { desc = StructGet (a1', meth); info = ([], func.info) },
             [ i1'; a2'; i2'; n' ] ))
        []
  | Call
      ( ({ desc = Get ({ desc = "rotl" | "rotr"; _ } as meth); _ } as func),
        [ i1; i2 ] ) ->
      let* i2' = instruction ctx i2 in
      let* i1' = instruction ctx i1 in
      let ty = check_int_bin_op (expression_type i1') (expression_type i2') in
      return_expression i
        (Call ({ desc = Get meth; info = ([], func.info) }, [ i1'; i2' ]))
        ty
  | Call
      ( ({ desc = Get ({ desc = "copysign" | "min" | "max"; _ } as meth); _ } as
         func),
        [ i1; i2 ] ) ->
      let* i2' = instruction ctx i2 in
      let* i1' = instruction ctx i1 in
      let ty = check_float_bin_op (expression_type i1') (expression_type i2') in
      return_expression i
        (Call ({ desc = Get meth; info = ([], func.info) }, [ i1'; i2' ]))
        ty
  | Call (i', l) -> (
      let* i' = instruction ctx i' in
      let* l' = instructions ctx l in
      match UnionFind.find (expression_type i') with
      | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
          match Tbl.find ctx.types ty with
          | _, Func typ ->
              let types =
                Array.to_list
                  (Array.map
                     (fun typ ->
                       UnionFind.make
                         (Valtype
                            { typ; internal = valtype ctx.type_context typ }))
                     typ.params)
              in
              assert (List.length types = List.length l');
              List.iter2 (fun i ty -> check_type ctx i ty) l' types;
              return_statement i
                (Call (i', l'))
                (Array.to_list
                   (Array.map
                      (fun typ ->
                        UnionFind.make
                          (Valtype
                             { typ; internal = valtype ctx.type_context typ }))
                      typ.results))
          | _ -> assert false)
      | _ -> assert false (*ZZZ*))
  | TailCall (i', l) -> (
      let* i' = instruction ctx i' in
      let* l' = instructions ctx l in
      match UnionFind.find (expression_type i') with
      | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
          match Tbl.find ctx.types ty with
          | _, Func typ ->
              let types =
                Array.to_list
                  (Array.map
                     (fun typ ->
                       UnionFind.make
                         (Valtype
                            { typ; internal = valtype ctx.type_context typ }))
                     typ.params)
              in
              assert (List.length types = List.length l');
              List.iter2 (fun i ty -> check_type ctx i ty) l' types;
              let types' =
                Array.to_list
                  (Array.map
                     (fun typ ->
                       UnionFind.make
                         (Valtype
                            { typ; internal = valtype ctx.type_context typ }))
                     typ.results)
              in
              let types =
                List.map
                  (fun typ -> UnionFind.make (Valtype typ))
                  ctx.return_types
              in
              check_subtypes ctx types' types;
              return_statement i (TailCall (i', l')) [] |> unreachable
          | _ -> assert false)
      | _ ->
          Format.eprintf "%a@." Output.instr i;
          assert false (*ZZZ*))
  | String (ty, _) as desc -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          (match snd (Tbl.find ctx.types ty) with
          | Func _ | Struct _ -> assert false (*ZZZ*)
          | Array _ -> ());
          let typ = Ref { nullable = false; typ = Type ty } in
          return_expression i desc
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | Int _ as desc -> return_expression i desc (UnionFind.make Number)
  | Float _ as desc -> return_expression i desc (UnionFind.make Float)
  | Cast (i', typ) ->
      let* i' = instruction ctx i' in
      let ty, skip =
        let ty' = expression_type i' in
        match typ with
        | Valtype typ ->
            let ty = valtype ctx.type_context typ in
            let ok = cast ctx ty' typ in
            if not ok then (
              Format.eprintf "%a@." Output.instr i;
              Format.eprintf "cast %a => %a@." output_inferred_type ty'
                Output.valtype typ);
            assert ok;
            let ty = UnionFind.make (Valtype { typ; internal = ty }) in
            (ty, subtype ctx ty' ty)
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
            (UnionFind.make (Valtype { typ; internal = ty }), false)
      in
      return_expression ~pop:skip i (Cast (i', typ)) ty
  | Test (i, ty) ->
      let* i' = instruction ctx i in
      let typ = Ref { nullable = true; typ = top_heap_type ctx ty.typ } in
      check_type ctx i'
        (UnionFind.make
           (Valtype { typ; internal = valtype ctx.type_context typ }));
      return_expression i
        (Test (i', ty))
        (UnionFind.make (Valtype { typ = I32; internal = I32 }))
  | Struct (ty, fields) -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some typ ->
          let field_types =
            match snd (Tbl.find ctx.types typ) with
            | Struct l -> l
            | _ -> (*ZZZ *) assert false
          in
          (* ZZZ We should check the evaluation order*)
          assert (List.length fields = Array.length field_types);
          (*ZZZ*)
          let* fields' =
            Array.fold_left
              (fun prev (name, (f : fieldtype)) ->
                match
                  List.find_opt (fun (idx, _) -> name.desc = idx.desc) fields
                with
                | None -> assert false (*ZZZ*)
                | Some (name, i') ->
                    let* l = prev in
                    let* i' = instruction ctx i' in
                    let typ = unpack_type f in
                    check_type ctx i'
                      (UnionFind.make
                         (Valtype
                            { typ; internal = valtype ctx.type_context typ }));
                    return ((name, i') :: l))
              (return []) field_types
          in
          let typ = Ref { nullable = false; typ = Type typ } in
          return_expression i
            (Struct (ty, fields'))
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | StructDefault ty as desc -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          (match snd (Tbl.find ctx.types ty) with
          | Struct fields ->
              (*ZZZ*)
              assert (Array.for_all (fun (_, ty) -> field_has_default ty) fields)
          | _ -> (*ZZZ *) assert false);
          let typ = Ref { nullable = false; typ = Type ty } in
          return_expression i desc
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | StructGet (i', field) ->
      let* i' = instruction ctx i' in
      let ty =
        let ty = expression_type i' in
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
                | Some typ -> UnionFind.make (fieldtype ctx typ))
            | _, Array _ when field.desc = "length" ->
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | _ -> assert false)
        | (Null | Valtype { typ = Ref { typ = Array; _ }; _ }), "length" ->
            UnionFind.make (Valtype { typ = I32; internal = I32 })
        | Valtype { typ = I32; _ }, "from_bits" ->
            UnionFind.make (Valtype { typ = F32; internal = F32 })
        | Valtype { typ = I64; _ }, "from_bits" ->
            UnionFind.make (Valtype { typ = F64; internal = F64 })
        | Valtype { typ = F32; _ }, "to_bits" ->
            UnionFind.make (Valtype { typ = I32; internal = I32 })
        | Valtype { typ = F64; _ }, "to_bits" ->
            UnionFind.make (Valtype { typ = I64; internal = I64 })
        | ( ((Number | Int | Valtype { typ = I32 | I64; _ }) as ty'),
            ("clz" | "ctz" | "popcnt") ) ->
            if ty' = Number then UnionFind.set ty Int;
            ty
        | ( ((Number | Float | Valtype { typ = F32 | F64; _ }) as ty'),
            ("abs" | "ceil" | "floor" | "trunc" | "nearest" | "sqrt") ) ->
            if ty' = Number then UnionFind.set ty Float;
            ty
        | _ ->
            Format.eprintf "??? %a %s@." output_inferred_type ty field.desc;
            assert false (*ZZZ*)
      in
      return_expression i (StructGet (i', field)) ty
  | StructSet (i1, field, i2) -> (
      let* i2' = instruction ctx i2 in
      let* i1' = instruction ctx i1 in
      let ty1 = expression_type i1' in
      match UnionFind.find ty1 with
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
              | Some typ ->
                  assert typ.mut;
                  let typ = unpack_type typ in
                  let ty =
                    UnionFind.make
                      (Valtype { typ; internal = valtype ctx.type_context typ })
                  in
                  check_type ctx i2' ty;
                  return_statement i (StructSet (i1', field, i2')) [])
          | _ -> assert false (*ZZZ*))
      | _ -> assert false (*ZZZ*))
  | Array (ty, i1, i2) -> (
      let* i2' = instruction ctx i2 in
      let* i1' = instruction ctx i1 in
      check_type ctx i2'
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          (match snd (Tbl.find ctx.types ty) with
          | Array field' ->
              let typ = unpack_type field' in
              check_type ctx i1'
                (UnionFind.make
                   (Valtype { typ; internal = valtype ctx.type_context typ }))
          | _ -> (*ZZZ *) assert false);
          let typ = Ref { nullable = false; typ = Type ty } in
          return_expression i
            (Array (Some ty, i1', i2'))
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | ArrayDefault (ty, i) -> (
      let* i' = instruction ctx i in
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          check_type ctx i'
            (UnionFind.make (Valtype { typ = I32; internal = I32 }));
          (match snd (Tbl.find ctx.types ty) with
          | Array field -> assert (field_has_default field)
          | _ -> (*ZZZ *) assert false);
          let typ = Ref { nullable = false; typ = Type ty } in
          return_expression i
            (ArrayDefault (Some ty, i'))
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | ArrayFixed (ty, instrs) -> (
      match ty with
      | None -> assert false (*ZZZ*)
      | Some ty ->
          let* instrs' =
            match snd (Tbl.find ctx.types ty) with
            | Array field' ->
                let typ = unpack_type field' in
                let typ = { typ; internal = valtype ctx.type_context typ } in
                let typ = UnionFind.make (Valtype typ) in
                List.fold_left
                  (fun prev i' ->
                    let* l = prev in
                    let* i' = instruction ctx i' in
                    check_type ctx i' typ;
                    return (i' :: l))
                  (return []) instrs
            | _ -> (*ZZZ *) assert false
          in
          let typ = Ref { nullable = false; typ = Type ty } in
          return_expression i
            (ArrayFixed (Some ty, instrs'))
            (UnionFind.make
               (Valtype { typ; internal = valtype ctx.type_context typ })))
  | ArrayGet (i1, i2) -> (
      let* i2' = instruction ctx i2 in
      let* i1' = instruction ctx i1 in
      check_type ctx i2'
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      match UnionFind.find (expression_type i1') with
      | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
          match Tbl.find ctx.types ty with
          | _, Array typ ->
              return_expression i
                (ArrayGet (i1', i2'))
                (UnionFind.make (fieldtype ctx typ))
          | _ -> assert false)
      | _ -> assert false (*ZZZ*))
  | ArraySet (i1, i2, i3) -> (
      let* i3' = instruction ctx i3 in
      let* i2' = instruction ctx i2 in
      let* i1' = instruction ctx i1 in
      check_type ctx i2'
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      match UnionFind.find (expression_type i1') with
      | Valtype { typ = Ref { typ = Type ty; _ }; _ } -> (
          match Tbl.find ctx.types ty with
          | _, Array typ ->
              assert typ.mut;
              let typ = unpack_type typ in
              let ty' = expression_type i3' in
              let ty =
                UnionFind.make
                  (Valtype { typ; internal = valtype ctx.type_context typ })
              in
              let ok = subtype ctx ty' ty in
              if not ok then
                Format.eprintf "%a <: %a@." output_inferred_type ty'
                  output_inferred_type ty;
              assert ok;
              return_statement i (ArraySet (i1', i2', i3')) []
          | _ -> assert false)
      | _ -> assert false (*ZZZ*))
  | BinOp (op, i1, i2) ->
      let* i2' = instruction ctx i2 in
      let* i1' = instruction ctx i1 in
      let ty =
        let ty1 = expression_type i1' in
        let ty2 = expression_type i2' in
        match (UnionFind.find ty1, UnionFind.find ty2) with
        | Any, Any -> (
            match op with
            | Add | Sub | Mul ->
                UnionFind.merge ty1 ty2 Number;
                ty1
            | Div (Some _) | Rem _ | And | Or | Xor | Shl | Shr _ ->
                UnionFind.merge ty1 ty2 Int;
                ty1
            | Lt (Some _) | Gt (Some _) | Le (Some _) | Ge (Some _) | Eq | Ne ->
                UnionFind.merge ty1 ty2 (Valtype { typ = I32; internal = I32 });
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Div None ->
                UnionFind.merge ty1 ty2 Float;
                ty1
            | Lt None | Gt None | Le None | Ge None ->
                UnionFind.merge ty1 ty2 (Valtype { typ = F32; internal = F32 });
                UnionFind.make (Valtype { typ = I32; internal = I32 }))
        | typ, Any | Any, typ -> (
            UnionFind.merge ty1 ty2 typ;
            match op with
            | Eq ->
                (match typ with
                | Valtype { internal = Ref _ as ty; _ } ->
                    assert (
                      Wasm.Types.val_subtype ctx.subtyping_info ty
                        (Ref { nullable = true; typ = Eq }))
                | Null ->
                    UnionFind.set ty1
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
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Add | Sub | Mul ->
                (match typ with
                | Valtype { internal = I32; _ }
                | Valtype { internal = I64; _ }
                | Valtype { internal = F32; _ }
                | Valtype { internal = F64; _ }
                | Number | Int | Float ->
                    ()
                | _ -> assert false (*ZZZ*));
                ty1
            | Div (Some _) | Rem _ | And | Or | Xor | Shl | Shr _ ->
                check_int_bin_op ty1 ty2
            | Div None -> check_float_bin_op ty1 ty2
            | Lt (Some _) | Gt (Some _) | Le (Some _) | Ge (Some _) ->
                (match typ with
                | Valtype { internal = I32; _ }
                | Valtype { internal = I64; _ }
                | Number | Int | Float ->
                    ()
                | _ -> assert false (*ZZZ*));
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Lt None | Gt None | Le None | Ge None ->
                (match typ with
                | Valtype { internal = F32; _ }
                | Valtype { internal = F64; _ }
                | Number | Int | Float ->
                    ()
                | _ -> assert false (*ZZZ*));
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Ne ->
                (match typ with
                | Valtype { internal = I32; _ }
                | Valtype { internal = I64; _ }
                | Valtype { internal = F32; _ }
                | Valtype { internal = F64; _ }
                | Number | Int | Float ->
                    ()
                | _ -> assert false (*ZZZ*));
                UnionFind.make (Valtype { typ = I32; internal = I32 }))
        | _ -> (
            match op with
            | Eq ->
                (match (UnionFind.find ty1, UnionFind.find ty2) with
                | ( Valtype { internal = Ref _ as ty1; _ },
                    Valtype { internal = Ref _ as ty2; _ } ) ->
                    assert (
                      Wasm.Types.val_subtype ctx.subtyping_info ty1
                        (Ref { nullable = true; typ = Eq }));
                    assert (
                      Wasm.Types.val_subtype ctx.subtyping_info ty2
                        (Ref { nullable = true; typ = Eq }))
                | Valtype { internal = Ref _ as typ1; _ }, Null ->
                    assert (
                      Wasm.Types.val_subtype ctx.subtyping_info typ1
                        (Ref { nullable = true; typ = Eq }));
                    UnionFind.merge ty1 ty2 (UnionFind.find ty2)
                | Null, Valtype { internal = Ref _ as typ2; _ } ->
                    assert (
                      Wasm.Types.val_subtype ctx.subtyping_info typ2
                        (Ref { nullable = true; typ = Eq }));
                    UnionFind.merge ty1 ty2 (UnionFind.find ty2)
                | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
                | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
                | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
                | Valtype { internal = F64; _ }, Valtype { internal = F64; _ }
                  ->
                    ()
                | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int)
                | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
                | Number, Number ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty1)
                | (Number | Int), Valtype { internal = I32 | I64; _ }
                | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty2)
                | _ -> assert false (*ZZZ*));
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Add | Sub | Mul ->
                (match (UnionFind.find ty1, UnionFind.find ty2) with
                | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
                | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
                | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
                | Valtype { internal = F64; _ }, Valtype { internal = F64; _ }
                  ->
                    ()
                | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int)
                | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
                | Number, Number ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty1)
                | (Number | Int), Valtype { internal = I32 | I64; _ }
                | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty2)
                | _ -> assert false (*ZZZ*));
                ty1
            | Div (Some _) | Rem _ | And | Or | Xor | Shl | Shr _ ->
                check_int_bin_op ty1 ty2
            | Div None -> check_float_bin_op ty1 ty2
            | Lt (Some _) | Gt (Some _) | Le (Some _) | Ge (Some _) ->
                (match (UnionFind.find ty1, UnionFind.find ty2) with
                | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
                | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
                | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int) ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty1)
                | (Number | Int), Valtype { internal = I32 | I64; _ } ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty2)
                | Number, Number -> UnionFind.merge ty1 ty2 Int
                | _ -> assert false (*ZZZ*));
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Lt None | Gt None | Le None | Ge None ->
                (match (UnionFind.find ty1, UnionFind.find ty2) with
                | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
                | Valtype { internal = F64; _ }, Valtype { internal = F64; _ }
                | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
                  ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty1)
                | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty2)
                | Number, Number -> UnionFind.merge ty1 ty2 Float
                | _ -> assert false (*ZZZ*));
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Ne ->
                (match (UnionFind.find ty1, UnionFind.find ty2) with
                | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
                | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
                | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
                | Valtype { internal = F64; _ }, Valtype { internal = F64; _ }
                  ->
                    ()
                | (Valtype { internal = I32 | I64; _ } | Int), (Number | Int)
                | (Valtype { internal = F32 | F64; _ } | Float), (Number | Float)
                | Number, Number ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty1)
                | (Number | Int), Valtype { internal = I32 | I64; _ }
                | (Number | Float), Valtype { internal = F32 | F64; _ } ->
                    UnionFind.merge ty1 ty2 (UnionFind.find ty2)
                | _ -> assert false (*ZZZ*));
                UnionFind.make (Valtype { typ = I32; internal = I32 }))
      in
      return_expression i (BinOp (op, i1', i2')) ty
  | UnOp (op, i') ->
      let* i' = instruction ctx i' in
      let typ = expression_type i' in
      let ty =
        match UnionFind.find typ with
        | Any -> (
            match op with
            | Not -> UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Neg | Pos -> UnionFind.make Number)
        | _ -> (
            match op with
            | Not ->
                (match UnionFind.find typ with
                | Valtype { internal = I32 | I64 | Ref _; _ } | Null | Int -> ()
                | Number -> UnionFind.set typ Int
                | _ -> assert false);
                UnionFind.make (Valtype { typ = I32; internal = I32 })
            | Neg | Pos ->
                (match UnionFind.find typ with
                | Valtype { internal = I32 | I64 | F32 | F64; _ }
                | Int | Float | Number ->
                    ()
                | _ -> assert false);
                typ)
      in
      return_expression i (UnOp (op, i')) ty
  | Let ([ (Some name, Some typ) ], None) as desc ->
      let typ = { typ; internal = valtype ctx.type_context typ } in
      ctx.locals <- StringMap.add name.desc typ ctx.locals;
      return_statement i desc []
  | Let ([ (None, None) ], Some i') ->
      let* i' = instruction ctx i' in
      return_statement i (Let ([ (None, None) ], Some i')) []
  (*
  | Let of (idx option * valtype option) list * instr option
*)
  | Br (label, i') ->
      (* Sequence of instructions *)
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      let* i' =
        match i' with
        | Some i' ->
            let* i' = instruction ctx i' in
            check_subtypes ctx (fst i'.info) params;
            return (Some i')
        | None ->
            assert (params = []);
            return None
      in
      return_statement i (Br (label, i')) [] |> unreachable
  | Br_if (label, i') ->
      let* i' = instruction ctx i' in
      let ty, types =
        match List.rev (fst i'.info) with
        | t :: r -> (t, r)
        | [] -> assert false
      in
      check_subtype ctx ty
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      check_subtypes ctx types params;
      return_statement i (Br_if (label, i')) params
  | Br_table (labels, i') ->
      let* i' = instruction ctx i' in
      let ty, types =
        match List.rev (fst i'.info) with
        | t :: r -> (t, r)
        | [] -> assert false
      in
      check_subtype ctx ty
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      let len = List.length (branch_target ctx (List.hd labels)) in
      List.iter
        (fun label ->
          let params = branch_target ctx label in
          assert (List.length params = len);
          (*ZZZ*)
          check_subtypes ctx types
            (List.map (fun typ -> UnionFind.make (Valtype typ)) params))
        labels;
      return_statement i (Br_table (labels, i')) [] |> unreachable
  | Br_on_null (idx, i') ->
      let* i' = instruction ctx i' in
      let typ, types =
        match List.rev (fst i'.info) with
        | t :: r -> (t, r)
        | [] -> assert false
      in
      let typ = UnionFind.find typ in
      let typ' =
        match typ with
        | Valtype
            {
              typ = Ref { nullable = _; typ; _ };
              internal = Ref { nullable = _; typ = ityp; _ };
            } ->
            UnionFind.make
              (Valtype
                 {
                   typ = Ref { nullable = false; typ };
                   internal = Ref { nullable = false; typ = ityp };
                 })
        | Any -> UnionFind.make Any
        | _ -> assert false (*ZZZ*)
      in
      let params = branch_target ctx idx in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      check_subtypes ctx types params;
      return_statement i (Br_on_null (idx, i')) (params @ [ typ' ])
  | Br_on_non_null (idx, i') ->
      let* i' = instruction ctx i' in
      let params = branch_target ctx idx in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      let typ, types =
        match List.rev (fst i'.info) with
        | t :: r -> (t, r)
        | [] -> assert false
      in
      let typ = UnionFind.find typ in
      (match typ with
      | Any -> ()
      | Valtype
          {
            typ = Ref { nullable = _; typ; _ };
            internal = Ref { nullable = _; typ = ityp; _ };
          } ->
          check_subtypes ctx
            (types
            @ [
                UnionFind.make
                  (Valtype
                     {
                       typ = Ref { nullable = false; typ };
                       internal = Ref { nullable = false; typ = ityp };
                     });
              ])
            params
      | _ -> assert false (*ZZZ*));
      return_statement i
        (Br_on_non_null (idx, i'))
        (List.rev (List.tl (List.rev params)))
  | Br_on_cast (label, ty, i') ->
      let* i' = instruction ctx i' in
      let typ', types =
        match List.rev (fst i'.info) with
        | t :: r -> (t, r)
        | [] -> assert false
      in
      let ityp = reftype ctx.type_context ty in
      let typ =
        UnionFind.make (Valtype { typ = Ref ty; internal = Ref ityp })
      in
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      check_subtypes ctx (types @ [ typ ]) params;
      let typ =
        match UnionFind.find typ' with
        | Valtype { typ = Ref ty'; internal = Ref ityp' } ->
            Valtype
              {
                typ = Ref (diff_ref_type ty' ty);
                internal = Ref (diff_ref_type_internal ityp' ityp);
              }
        | Any -> Any
        | _ -> assert false
      in
      return_statement i
        (Br_on_cast (label, ty, i'))
        (List.rev (List.tl (List.rev params)) @ [ UnionFind.make typ ])
  | Br_on_cast_fail (label, ty, i') ->
      let* i' = instruction ctx i' in
      let typ', types =
        match List.rev (fst i'.info) with
        | t :: r -> (t, r)
        | [] -> assert false
      in
      let ityp = reftype ctx.type_context ty in
      let typ =
        match UnionFind.find typ' with
        | Valtype { typ = Ref ty'; internal = Ref ityp' } ->
            Valtype
              {
                typ = Ref (diff_ref_type ty' ty);
                internal = Ref (diff_ref_type_internal ityp' ityp);
              }
        | Any -> Any
        | _ -> assert false
      in
      let params = branch_target ctx label in
      let params = List.map (fun typ -> UnionFind.make (Valtype typ)) params in
      check_subtypes ctx (types @ [ UnionFind.make typ ]) params;
      let typ =
        UnionFind.make (Valtype { typ = Ref ty; internal = Ref ityp })
      in
      return_statement i
        (Br_on_cast_fail (label, ty, i'))
        (List.rev (List.tl (List.rev params)) @ [ typ ])
  | Throw (tag, lst) ->
      let* lst' = instructions ctx lst in
      let { params; results } = Tbl.find ctx.tags tag in
      assert (results = [||]);
      check_subtypes ctx
        (List.map expression_type lst')
        (Array.to_list
           (Array.map
              (fun typ ->
                UnionFind.make
                  (Valtype { typ; internal = valtype ctx.type_context typ }))
              params));
      return_statement i (Throw (tag, lst')) [] |> unreachable
  | ThrowRef i' ->
      let* i' = instruction ctx i' in
      let typ = Ref { nullable = true; typ = Exn } in
      check_type ctx i'
        (UnionFind.make
           (Valtype { typ; internal = valtype ctx.type_context typ }));
      return_statement i (ThrowRef i') [] |> unreachable
  | NonNull i' -> (
      let* i' = instruction ctx i' in
      match UnionFind.find (expression_type i') with
      | Valtype
          {
            typ = Ref { nullable = _; typ; _ };
            internal = Ref { nullable = _; typ = ityp; _ };
          } ->
          return_expression i (NonNull i')
            (UnionFind.make
               (Valtype
                  {
                    typ = Ref { nullable = false; typ };
                    internal = Ref { nullable = false; typ = ityp };
                  }))
      | Any -> return_expression i (NonNull i') (expression_type i')
      | _ -> assert false (*ZZZ*))
  | Return i' ->
      (*ZZZ List of instructions? *)
      let* i' =
        match i' with
        | Some i' ->
            let* i' = instruction ctx i' in
            check_subtypes ctx (fst i'.info)
              (List.map
                 (fun typ -> UnionFind.make (Valtype typ))
                 ctx.return_types);
            return (Some i')
        | None ->
            assert (ctx.return_types = []);
            return None
      in
      return_statement i (Return i') [] |> unreachable
  | Sequence l ->
      let* l' = instructions ctx l in
      return_statement i (Sequence l') (List.map expression_type l')
  | Select (i1, i2, i3) ->
      let* i1' = instruction ctx i1 in
      let* i3' = instruction ctx i3 in
      let* i2' = instruction ctx i2 in
      check_type ctx i1'
        (UnionFind.make (Valtype { typ = I32; internal = I32 }));
      let ty =
        let ty1 = expression_type i2' in
        let ty2 = expression_type i3' in
        match (UnionFind.find ty1, UnionFind.find ty2) with
        | _, Any -> ty1
        | Any, _ -> ty2
        | Valtype { internal = I32; _ }, Valtype { internal = I32; _ }
        | Valtype { internal = I64; _ }, Valtype { internal = I64; _ }
        | Valtype { internal = F32; _ }, Valtype { internal = F32; _ }
        | Valtype { internal = F64; _ }, Valtype { internal = F64; _ } ->
            ty2
        | (Int | Number), (Int | Valtype { internal = I32 | I64; _ })
        | (Float | Number), (Float | Valtype { internal = F32 | F64; _ })
        | Number, Number ->
            UnionFind.merge ty1 ty2 (UnionFind.find ty2);
            ty2
        | ( (Valtype { internal = I32; _ } | Valtype { internal = I64; _ }),
            (Int | Number) )
        | ( (Valtype { internal = F32; _ } | Valtype { internal = F64; _ }),
            (Float | Number) )
        | (Int | Float), Number ->
            UnionFind.merge ty1 ty2 (UnionFind.find ty1);
            ty1
        | Valtype { internal = typ1; _ }, Valtype { internal = typ2; _ }
          when typ1 = typ2 ->
            (*ZZZ fragile *)
            ty1
        | Valtype { typ = typ1; _ }, Valtype { typ = typ2; _ } ->
            Format.eprintf "AAAA %a %a@." Output.valtype typ1 Output.valtype
              typ2;
            assert false
        | _ -> (*ZZZ*) assert false
      in
      return_expression i (Select (i1', i2', i3')) ty
  | _ ->
      Format.eprintf "%a@." Output.instr i;
      assert false

and instructions ctx l =
  match l with
  | [] -> return []
  | i :: r ->
      let* r' = instructions ctx r in
      let* i' = instruction ctx i in
      return (i' :: r')

and block_contents ctx l =
  match l with
  | [] -> return []
  | i :: r ->
      let* i' = instruction ctx i in
      let* () =
        push_results (List.map (fun ty -> (i.info, ty)) (fst i'.info))
      in
      let* r' = block_contents ctx r in
      return (i' :: r')

and block ctx loc label params results br_params block =
  with_empty_stack
    (let* () = push_results (List.map (fun ty -> (loc, ty)) params) in
     let* block' =
       block_contents
         { ctx with control_types = (label, br_params) :: ctx.control_types }
         block
     in
     let* () =
       pop_args ctx (List.map (fun typ -> UnionFind.make (Valtype typ)) results)
     in
     return block')

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

type ('before, 'after) phased = Before of 'before | After of 'after

let globals type_context ctx fields =
  List.map
    (fun field ->
      match field with
      | Global ({ name; mut; typ = Some typ; def; _ } as g) ->
          (*ZZZ check constant instructions *)
          (*ZZZ handle typ= None *)
          let def' = with_empty_stack (instruction ctx def) in
          let internal = valtype type_context typ in
          let typ = { typ; internal } in
          Tbl.add ctx.globals name (mut, typ);
          check_type ctx def' (UnionFind.make (Valtype typ));
          After (Global { g with def = def' })
      | f -> Before f)
    fields

let functions ctx fields =
  List.map
    (fun field ->
      match field with
      | Before (Func { name; sign; body = label, body; typ; attributes }) ->
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
          let ctx =
            {
              ctx with
              locals = !locals;
              control_types = [ (label, return_types) ];
              return_types;
            }
          in
          let body =
            with_empty_stack
              (let* body = block_contents ctx body in
               let* () =
                 pop_args ctx
                   (List.map
                      (fun typ -> UnionFind.make (Valtype typ))
                      return_types)
               in
               return body)
          in
          Func { name; sign; body = (label, body); typ; attributes }
      | Before (Global _) -> assert false
      | After f | Before ((Type _ | Fundecl _ | GlobalDecl _ | Tag _) as f) -> f)
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
      (*      memories = Tbl.make (Namespace.make ()) "memories";*)
      tags = Tbl.make (Namespace.make ()) "tag";
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
  let fields = globals type_context ctx fields in
  let fields = functions ctx fields in
  List.map
    (fun f ->
      Ast_utils.map_modulefield
        (fun (types, loc) ->
          ( List.map
              (fun ty ->
                match UnionFind.find ty with
                | Any | Null -> Value (Ref { nullable = true; typ = Any })
                | Number -> Value I32
                | Int8 -> Packed I8
                | Int16 -> Packed I16
                | Int -> Value I32
                | Float -> Value F64
                | Valtype { typ; _ } -> Value typ)
              types,
            loc ))
        f)
    fields
