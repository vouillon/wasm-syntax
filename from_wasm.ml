module Src = Wasm.Ast.Text

let get_annot (a, _) = a
let get_type (_, t) = t
let name_field _st a = Option.value ~default:"foo" a
let name_type _st a = Option.value ~default:"foo" a

let name_func _st a exports =
  Option.value ~default:(match exports with nm :: _ -> nm | [] -> "foo") a

let name_local _st a = Option.value ~default:"x" a
let name_global _st a = Option.value ~default:"x" a
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
let sequence l = match l with [ i ] -> i | _ -> Ast.no_loc (Ast.Sequence l)

let is_integer =
  let int_re =
    Re.(
      compile
        (whole_string
           (alt
              [
                rep1 (alt [ rg '0' '9'; char '_' ]);
                seq
                  [
                    str "0x";
                    rep1 (alt [ rg '0' '9'; rg 'a' 'f'; rg 'A' 'F'; char '_' ]);
                  ];
              ])))
  in
  fun s -> Re.execp int_re s

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

let three_args l : Ast.instr * Ast.instr * Ast.instr =
  match l with
  | [] -> (unit, unit, unit)
  | [ x ] -> (unit, unit, x)
  | [ x; y ] -> (unit, x, y)
  | [ x; y; z ] -> (x, y, z)
  | x :: y :: r ->
      (*ZZZ Should take arity into account *)
      (x, y, Ast.no_loc (Ast.Sequence r))

let reasonable_string =
  Re.(
    compile
      (whole_string
         (rep
            (alt
               [ diff any (rg '\000' '\031'); char '\n'; char '\r'; char '\t' ]))))

let string_args n args =
  try
    if Int32.of_int (List.length args) <> n then raise Exit;
    let b = Bytes.create (Int32.to_int n) in
    List.iteri
      (fun i arg ->
        match arg.Ast.descr with
        | Ast.Int c
          when let c = int_of_string c in
               c >= 0 && c < 256 ->
            Bytes.set b i (Char.chr (int_of_string c))
        | _ -> raise Exit)
      args;
    let s = Bytes.to_string b in
    if String.is_valid_utf_8 s && Re.execp reasonable_string s then Some s
    else None
  with Exit -> None

let rec split_last l =
  match l with
  | [] -> assert false
  | [ x ] -> (x, [])
  | y :: r ->
      let x, l = split_last r in
      (x, y :: l)

let signage s args =
  match s with
  | None -> sequence args
  | Some s ->
      Ast.no_loc
        (Ast.Call
           ( Ast.no_loc
               (Ast.Get
                  (match s with
                  | Ast.Signed -> "signed"
                  | Unsigned -> "unsigned")),
             [ sequence args ] ))

let int_un_op sz (op : Src.int_un_op) args =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  match op with
  | Clz -> no_loc (Call (no_loc (Get "clz"), [ sequence args ]))
  | Ctz -> no_loc (Call (no_loc (Get "ctz"), [ sequence args ]))
  | Popcnt -> no_loc (Call (no_loc (Get "popcnt"), [ sequence args ]))
  | Eqz -> no_loc (UnOp (Not, sequence args))
  | Trunc (_, s) ->
      no_loc (Call (no_loc (Get "truncate"), [ signage (Some s) args ]))
  | TruncSat (_, s) -> no_loc (Cast (signage (Some s) args, sz))
  | Reinterpret -> no_loc (Call (no_loc (Get "reinterpret"), [ sequence args ]))
  | ExtendS _ -> no_loc Unreachable (* ZZZ *)

let int_bin_op (op : Src.int_bin_op) args =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  let symbol op =
    let e1, e2 = two_args args in
    no_loc (BinOp (op, e1, e2))
  in
  match op with
  | Add -> symbol Add
  | Sub -> symbol Sub
  | Mul -> symbol Mul
  | Div s -> symbol (Div (Some s))
  | Rem s -> symbol (Rem s)
  | And -> symbol And
  | Or -> symbol Or
  | Xor -> symbol Xor
  | Shl -> symbol Shl
  | Shr s -> symbol (Shr s)
  | Rotl -> no_loc (Call (no_loc (Get "rotl"), [ sequence args ]))
  | Rotr -> no_loc (Call (no_loc (Get "rotr"), [ sequence args ]))
  | Eq -> symbol Eq
  | Ne -> symbol Ne
  | Lt s -> symbol (Lt (Some s))
  | Gt s -> symbol (Gt (Some s))
  | Le s -> symbol (Le (Some s))
  | Ge s -> symbol (Ge (Some s))

let float_un_op sz (op : Src.float_un_op) args =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  match op with
  | Neg -> no_loc (UnOp (Neg, sequence args))
  | Abs -> no_loc (Call (no_loc (Get "abs"), [ sequence args ]))
  | Ceil -> no_loc (Call (no_loc (Get "ceil"), [ sequence args ]))
  | Floor -> no_loc (Call (no_loc (Get "floor"), [ sequence args ]))
  | Trunc -> no_loc (Call (no_loc (Get "trunc"), [ sequence args ]))
  | Nearest -> no_loc (Call (no_loc (Get "nearest"), [ sequence args ]))
  | Sqrt -> no_loc (Call (no_loc (Get "sqrt"), [ sequence args ]))
  | Convert (_, s) -> no_loc (Cast (signage (Some s) args, sz))
  | Reinterpret -> no_loc (Call (no_loc (Get "reinterpret"), [ sequence args ]))

let float_bin_op (op : Src.float_bin_op) args =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  let symbol op =
    let e1, e2 = two_args args in
    no_loc (BinOp (op, e1, e2))
  in
  match op with
  | Add -> symbol Add
  | Sub -> symbol Sub
  | Mul -> symbol Mul
  | Div -> symbol (Div None)
  | Min -> no_loc (Call (no_loc (Get "min"), [ sequence args ]))
  | Max -> no_loc (Call (no_loc (Get "max"), [ sequence args ]))
  | CopySign -> no_loc (Call (no_loc (Get "copysign"), [ sequence args ]))
  | Eq -> symbol Eq
  | Ne -> symbol Ne
  | Lt -> symbol (Lt None)
  | Gt -> symbol (Gt None)
  | Le -> symbol (Le None)
  | Ge -> symbol (Ge None)

let rec instr st (i : Src.instr) (args : Ast.instr list) : Ast.instr =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  match i with
  | Block { label; typ = _; block } ->
      assert (args = []);
      no_loc (Block (label, List.map (fun i -> instr st i []) block))
  | Loop { label; typ = _; block } ->
      assert (args = []);
      no_loc (Loop (label, List.map (fun i -> instr st i []) block))
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
  | Pop _ -> sequence []
  | Drop -> no_loc (Let ([ (None, None) ], Some (sequence args)))
  | Br i -> no_loc (Br (idx st `Label i, sequence_opt args))
  | Br_if i -> no_loc (Br_if (idx st `Label i, sequence args))
  | Br_on_null i -> no_loc (Br_on_null (idx st `Label i, sequence args))
  | Br_on_non_null i -> no_loc (Br_on_null (idx st `Label i, sequence args))
  | Br_on_cast (i, _, t) ->
      (* ZZZ cast? *)
      no_loc (Br_on_cast (idx st `Label i, reftype st t, sequence args))
  | Br_on_cast_fail (i, _, t) ->
      (* ZZZ cast? *)
      no_loc (Br_on_cast_fail (idx st `Label i, reftype st t, sequence args))
  | Br_table (labels, label) ->
      no_loc
        (Br_table
           ( List.map (fun i -> idx st `Label i) (labels @ [ label ]),
             sequence args ))
  | Folded (i, args') ->
      assert (args = []);
      instr st i (List.map (fun i -> instr st i []) args')
  | LocalGet x -> sequence (args @ [ no_loc (Get (idx st `Local x)) ])
  | GlobalGet x -> sequence (args @ [ no_loc (Get (idx st `Global x)) ])
  | LocalSet x -> no_loc (Set (idx st `Local x, sequence args))
  | GlobalSet x -> no_loc (Set (idx st `Global x, sequence args))
  | LocalTee x -> no_loc (Tee (idx st `Local x, sequence args))
  | BinOp (I32 op) | BinOp (I64 op) -> int_bin_op op args
  | BinOp (F32 op) | BinOp (F64 op) -> float_bin_op op args
  | UnOp (I64 op) -> int_un_op I64 op args
  | UnOp (I32 op) -> int_un_op I32 op args
  | UnOp (F64 op) -> float_un_op F64 op args
  | UnOp (F32 op) -> float_un_op F32 op args
  | StructNew i ->
      (*ZZZZ Use type *)
      no_loc (Struct (Some (idx st `Type i), List.map (fun i -> ("f", i)) args))
  | StructGet (s, _t, _f) ->
      signage s [ no_loc (StructGet (sequence args, "f")) ]
  | StructSet (_t, _f) ->
      let e1, e2 = two_args args in
      no_loc (StructSet (e1, "f", e2))
  | ArrayNew t ->
      let e1, e2 = two_args args in
      no_loc (Array (Some (idx st `Type t), e1, e2))
  | ArrayNewFixed (t, n) -> (
      match string_args n args with
      | Some s ->
          no_loc
            (Cast
               ( no_loc (String (Some (idx st `Type t), s)),
                 Ref { nullable = false; typ = Type (idx st `Type t) } ))
      | None ->
          (*ZZZ take n into account *)
          no_loc (ArrayFixed (Some (idx st `Type t), args)))
  | ArrayGet (s, _t) ->
      let e1, e2 = two_args args in
      signage s [ no_loc (ArrayGet (e1, e2)) ]
  | ArraySet _t ->
      let e1, e2, e3 = three_args args in
      no_loc (ArraySet (e1, e2, e3))
  | Call x -> no_loc (Call (no_loc (Get (idx st `Func x)), args))
  | CallRef _ ->
      (* ZZZ cast? *)
      if args = [] then no_loc (Call (unit, []))
      else
        let f, l = split_last args in
        no_loc (Call (f, l))
  | ReturnCall x ->
      no_loc
        (Return (Some (no_loc (Call (no_loc (Get (idx st `Func x)), args)))))
  | ReturnCallRef _ ->
      (* ZZZ cast? *)
      no_loc
        (Return
           (Some
              (if args = [] then no_loc (Call (unit, []))
               else
                 let f, l = split_last args in
                 no_loc (Call (f, l)))))
  | Return -> no_loc (Return (sequence_opt args))
  | TupleMake _ -> no_loc (Sequence args)
  | Const (I32 n) | Const (I64 n) ->
      no_loc (Int n) (*ZZZ Negative ints / floats *)
  | Const (F32 f) | Const (F64 f) ->
      let f = if is_integer f then f ^ "." else f in
      no_loc (Float f)
  | RefI31 -> no_loc (Cast (sequence args, Ref { nullable = false; typ = I31 }))
  | I31Get s ->
      no_loc
        (Call
           ( no_loc
               (Get (match s with Signed -> "signed" | Unsigned -> "unsigned")),
             args ))
  | I64ExtendI32 s -> no_loc (Cast (signage (Some s) args, I64))
  | I32WrapI64 -> no_loc (Cast (sequence args, I32))
  | F64PromoteF32 -> no_loc (Cast (sequence args, F64))
  | F32DemoteF64 -> no_loc (Cast (sequence args, F32))
  | ExternConvertAny ->
      no_loc (Cast (sequence args, Ref { nullable = true; typ = Extern }))
  | AnyConvertExtern ->
      no_loc (Cast (sequence args, Ref { nullable = true; typ = Any }))
  | ArrayNewData (t, _) ->
      no_loc (String (Some (idx st `Type t), "foo")) (*ZZZ*)
  | ArrayLen -> no_loc (Call (no_loc (Get "array_len"), args))
  | RefCast t -> no_loc (Cast (sequence args, Ref (reftype st t)))
  | RefTest t -> no_loc (Test (sequence args, reftype st t))
  | RefEq ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Eq, e1, e2))
  | RefFunc f -> no_loc (Get (idx st `Func f))
  | RefNull t ->
      no_loc (Cast (no_loc Null, Ref { nullable = true; typ = heaptype st t }))
  | RefIsNull -> no_loc (UnOp (Not, sequence args))
  | Select _ ->
      let e1, e2, e = three_args args in
      no_loc (Select (e, e1, e2))
  (* To implement now *)
  | Try _ | Throw _ | TupleExtract _ | ArrayFill _ | ArrayCopy _
  (* signed(x as i8) as i32 ? *)
  (* Later *)
  | ReturnCallIndirect _ | CallIndirect _ | RefAsNonNull | ArrayInitElem _
  | ArrayInitData _ | ArrayNewElem _ | StructNewDefault _ | ArrayNewDefault _
  | I32Load8 _ | I32Store8 _ ->
      no_loc Unreachable (* ZZZ *)

let bind_locals st l =
  List.map
    (fun (id, t) ->
      Ast.no_loc
        (Ast.Let ([ (Some (name_local st id), Some (valtype st t)) ], None)))
    l

let typeuse st (typ, sign) =
  ( Option.map (fun i -> idx st `Type i) typ,
    Option.map
      (fun (p, r) ->
        {
          Ast.named_params = List.map (fun (id, t) -> (id, valtype st t)) p;
          results = List.map (fun t -> valtype st t) r;
        })
      sign )

let exports e =
  List.map (fun nm -> ("export", Ast.no_loc (Ast.String (None, nm)))) e

let import (module_, name) =
  ( "import",
    Ast.no_loc
      (Ast.Sequence
         [
           Ast.no_loc (Ast.String (None, module_));
           Ast.no_loc (Ast.String (None, name));
         ]) )

let modulefield st (f : Src.modulefield) : Ast.modulefield option =
  match f with
  | Types t -> Some (Type (rectype st t))
  | Func { id; locals; instrs; typ; exports = e } ->
      let locals = bind_locals st locals in
      let typ, sign = typeuse st typ in
      Some
        (Func
           {
             name = name_func st id e;
             typ;
             sign;
             body = (None, locals @ List.map (fun i -> instr st i []) instrs);
             attributes = exports e;
           })
  | Import { module_; name; id; desc = Func typ; exports = e } ->
      let typ, sign = typeuse st typ in
      Some
        (Fundecl
           {
             name = name_func st id e;
             typ;
             sign;
             attributes = import (module_, name) :: exports e;
           })
  | Global { id; typ; init; exports = e } ->
      Some
        (Global
           {
             name = name_global st id;
             typ = Some (globaltype st typ);
             def = sequence (List.map (fun i -> instr st i []) init);
             attributes = exports e;
           })
  | _ -> None

(*
    | Memory of {
        id : id option;
        limits : limits;
        init : string option;
        exports : string list;
      }
    | Tag of { id : id option; typ : typeuse; exports : string list }
    | Export of { name : string; kind : exportable; index : idx }
    | Start of idx
    | Elem of { id : id option; typ : reftype; init : expr list }
    | Data of { id : id option; init : string; mode : datamode }
*)
let module_ (_, fields) = List.map (fun f -> modulefield () f) fields

(*
Use imports and exports as hints
Collect exports to associate them to the corresponding module field

- get existing type names
- scan types, allocating new types + build array of type definitions
  rename if not valid identifier
  + mapping name -> index / index -> name / name -> new_name
- get existing function names
- get existing names for other components; rename if already used
- build mappings: name -> new_name / index -> name
- get existing local names


List of reserved names:
- all keywords
- signed, unsigned, array_len, min, max, copysign, rotr, rotl, reinterpret ...
*)
