module Src = Wasm.Ast.Text

let get_annot (a, _) = a
let get_type (_, t) = t
let name_field _st a = Option.value ~default:"foo" a
let name_type _st a = Option.value ~default:"foo" a
let name_func _st a = Option.value ~default:"foo" a
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

let functype st (t : Src.functype) : Ast.functype =
  {
    params = Array.map (fun t -> valtype st t) t.params;
    results = Array.map (fun t -> valtype st t) t.results;
  }

let packedtype _ (t : Src.packedtype) : Ast.packedtype = t

let storagetype st (t : Src.storagetype) : Ast.storagetype =
  match t with
  | Value t -> Value (valtype st t)
  | Packed t -> Packed (packedtype st t)

let muttype typ st (t : _ Src.muttype) : _ Ast.muttype =
  { t with typ = typ st t.typ }

let fieldtype st = muttype storagetype st

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

let globaltype st = muttype valtype st

(*
Step 1: traverse types and find existing names
Step 2: use this info to generate using names without reusing existing names
*)

(*ZZZ
  - first pass to see missing labels
  - explode tuples
*)

let unit = Ast.no_loc (Ast.Sequence [])
let sequence l = match l with [ i ] -> i | _ -> unit

let sequence_opt l =
  match l with
  | [] -> None
  | [ i ] -> Some i
  | l -> Some (Ast.no_loc (Ast.Sequence l))

let two_args l : Ast.instr * Ast.instr =
  match l with
  | [] -> (unit, unit)
  | [ x ] -> (unit, x)
  | [ x; y ] -> (x, y)
  | x :: r ->
      (*ZZZ Should take arity into account *)
      (x, Ast.no_loc (Ast.Sequence r))

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
  | LocalTee x -> no_loc (Tee (idx st `Local x, sequence args))
  | RefCast t -> no_loc (Cast (sequence args, reftype st t))
  | BinOp (I32 Add) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Add, e1, e2))
  | BinOp (I32 Sub) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Sub, e1, e2))
  | BinOp (I32 (Lt s)) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Lt s, e1, e2))
  | BinOp (I32 (Gt s)) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Gt s, e1, e2))
  | BinOp (I32 And) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (And, e1, e2))
  | BinOp (I32 Or) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Or, e1, e2))
  | BinOp (I32 Eq) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Eq, e1, e2))
  | BinOp (I32 Ne) ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Ne, e1, e2))
  | BinOp (I32 Rotr) -> no_loc (Call (no_loc (Get "rotr"), args))
  | BinOp (I32 Rotl) -> no_loc (Call (no_loc (Get "rotl"), args))
  | StructGet (_s, _t, _f) -> no_loc (StructGet (sequence args, "f"))
  | Call x -> no_loc (Call (no_loc (Get (idx st `Func x)), args))
  | ReturnCall x ->
      no_loc
        (Return (Some (no_loc (Call (no_loc (Get (idx st `Func x)), args)))))
  | TupleMake _ -> no_loc (Sequence args)
  | Const (I32 n) -> no_loc (Int (Int32.to_string n))
  | StructNew i ->
      (*ZZZZ*)
      ignore args;
      no_loc (Struct (Some (idx st `Type i), []))
  | RefI31 -> no_loc (Cast (sequence args, { nullable = false; typ = I31 }))
  | ArrayNewData _ -> no_loc (String "foo")
  | _ -> assert false

let modulefield st (f : Src.modulefield) : Ast.modulefield option =
  match f with
  | Types t -> Some (Type (rectype st t))
  | Func { id; instrs; _ } ->
      prerr_endline "AAA";
      Option.iter prerr_endline id;
      Some
        (Func
           {
             name = name_func st id;
             typ = None;
             sign = None;
             body = (None, List.map (fun i -> instr st i []) instrs);
           })
  | _ -> None

(*
    | Import of {
        module_ : string;
        name : string;
        id : id option;
        desc : importdesc;
        exports : string list;
      }
    | Func of {
        id : id option;
        typ : typeuse;
        locals : (id option * valtype) list;
        instrs : instr list;
        exports : string list;
      }
    | Memory of {
        id : id option;
        limits : limits;
        init : string option;
        exports : string list;
      }
    | Tag of { id : id option; typ : typeuse; exports : string list }
    | Global of {
        id : id option;
        typ : globaltype;
        init : expr;
        exports : string list;
      }
    | Export of { name : string; kind : exportable; index : idx }
    | Start of idx
    | Elem of { id : id option; typ : reftype; init : expr list }
    | Data of { id : id option; init : string; mode : datamode }
*)
let module_ (_, fields) =
  Format.eprintf "AAA %d@." (List.length fields);
  List.map (fun f -> modulefield () f) fields
