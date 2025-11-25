(*
- field names
- type checker
- proper folding
*)

module Src = Wasm.Ast.Text
module StringMap = Map.Make (String)

module Namespace = struct
  type t = { mutable existing_names : int StringMap.t }

  let rec add_indexed ns x i =
    let y = Printf.sprintf "%s_%d" x i in
    if StringMap.mem y ns.existing_names then add_indexed ns x (i + 1)
    else (
      ns.existing_names <-
        ns.existing_names |> StringMap.add y 1 |> StringMap.add x i;
      y)

  let add ns x =
    match StringMap.find_opt x ns.existing_names with
    | Some i -> add_indexed ns x (i + 1)
    | None ->
        ns.existing_names <- ns.existing_names |> StringMap.add x 1;
        x

  let dup { existing_names } = { existing_names }
  let make () = { existing_names = StringMap.empty }
end

module Sequence = struct
  type t = {
    name : string;
    index_mapping : (int, string) Hashtbl.t;
    label_mapping : (string, string) Hashtbl.t;
    mutable last_index : int;
    mutable current_index : int;
    namespace : Namespace.t;
    default : string;
  }

  let make namespace name default =
    {
      name;
      index_mapping = Hashtbl.create 16;
      label_mapping = Hashtbl.create 16;
      last_index = 0;
      current_index = 0;
      namespace;
      default;
    }

  let register' seq id exports =
    let name =
      let name =
        match (id, exports) with
        | Some nm, _ -> nm
        | None, nm :: _ -> nm
        | _ -> seq.default
      in
      Namespace.add seq.namespace name
    in
    let idx = seq.last_index in
    seq.last_index <- seq.last_index + 1;
    Hashtbl.add seq.index_mapping idx name;
    Option.iter (fun id -> Hashtbl.add seq.label_mapping id name) id;
    name

  let register seq id exports = ignore (register' seq id exports)

  let get seq (idx : Src.idx) =
    Ast.no_loc
    @@
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
          if true then raise Not_found else exit 1)

  let get_current seq =
    let i = seq.current_index in
    seq.current_index <- i + 1;
    Ast.no_loc (Hashtbl.find seq.index_mapping i)

  let consume_currents seq = seq.current_index <- seq.last_index
end

module LabelStack = struct
  type t = {
    ns : Namespace.t;
    stack : (string option * (string * bool ref)) list;
  }

  let push st label =
    let ns = Namespace.dup st.ns in
    let used = ref false in
    let name = Namespace.add ns (Option.value ~default:"l" label) in
    ( (fun () -> if !used then Some name else None),
      { ns; stack = (label, (name, used)) :: st.stack } )

  let get st (idx : Src.idx) =
    let name, used =
      match idx with
      | Num n -> snd (List.nth st.stack (Int32.to_int n))
      | Id id -> List.assoc (Some id) st.stack
    in
    used := true;
    name

  let make () = { ns = Namespace.make (); stack = [] }
end

type ctx = {
  common_namespace : Namespace.t;
  types : Sequence.t;
  struct_fields : (string, Sequence.t * string list) Hashtbl.t;
  globals : Sequence.t;
  functions : Sequence.t;
  memories : Sequence.t;
  tags : Sequence.t;
  locals : Sequence.t;
  labels : LabelStack.t;
}

let get_annot (a, _) = a
let get_type (_, t) = t
let annotated a t = (a, t)

let idx ctx kind i =
  match kind with
  | `Type -> Sequence.get ctx.types i
  | `Global -> Sequence.get ctx.globals i
  | `Func -> Sequence.get ctx.functions i
  | `Tag -> Sequence.get ctx.tags i
  | `Local -> Sequence.get ctx.locals i

let label ctx i = LabelStack.get ctx.labels i

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

let comptype st name (t : Src.comptype) : Ast.comptype =
  match t with
  | Func t -> Func (functype st t)
  | Struct l ->
      let seq = fst (Hashtbl.find st.struct_fields name) in
      Struct
        (Array.mapi
           (fun i t ->
             annotated
               (Sequence.get seq
                  (match get_annot t with
                  | None -> Num (Int32.of_int i)
                  | Some id -> Id id))
               (fieldtype st (get_type t)))
           l)
  | Array t -> Array (fieldtype st t)

let subtype st name (t : Src.subtype) : Ast.subtype =
  {
    typ = comptype st name t.typ;
    supertype = Option.map (fun i -> idx st `Type i) t.supertype;
    final = t.final;
  }

let rectype st (t : Src.rectype) : Ast.rectype =
  Array.map
    (fun t ->
      let name = Sequence.get_current st.types in
      annotated name (subtype st name.descr (get_type t)))
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

let numtype (ty : Src.valtype) =
  match ty with
  | I32 -> `I32
  | I64 -> `I64
  | F32 -> `F32
  | F64 -> `F64
  | _ -> assert false

let int_un_op sz (op : Src.int_un_op) args =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  match op with
  | Clz -> no_loc (Call (no_loc (Get (Ast.no_loc "clz")), [ sequence args ]))
  | Ctz -> no_loc (Call (no_loc (Get (Ast.no_loc "ctz")), [ sequence args ]))
  | Popcnt ->
      no_loc (Call (no_loc (Get (Ast.no_loc "popcnt")), [ sequence args ]))
  | Eqz -> no_loc (UnOp (Not, sequence args))
  | Trunc (_, signage) ->
      no_loc
        (Cast
           ( sequence args,
             Signedtype { typ = numtype sz; signage; strict = true } ))
  | TruncSat (_, signage) ->
      no_loc
        (Cast
           ( sequence args,
             Signedtype { typ = numtype sz; signage; strict = false } ))
  | Reinterpret -> no_loc (StructGet (sequence args, Ast.no_loc "to_bits"))
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
  | Rotl ->
      let e1, e2 = two_args args in
      no_loc (Call (no_loc (Get (Ast.no_loc "rotl")), [ e1; e2 ]))
  | Rotr ->
      let e1, e2 = two_args args in
      no_loc (Call (no_loc (Get (Ast.no_loc "rotr")), [ e1; e2 ]))
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
  | Abs -> no_loc (StructGet (sequence args, Ast.no_loc "abs"))
  | Ceil -> no_loc (StructGet (sequence args, Ast.no_loc "ceil"))
  | Floor -> no_loc (StructGet (sequence args, Ast.no_loc "floor"))
  | Trunc -> no_loc (StructGet (sequence args, Ast.no_loc "trunc"))
  | Nearest -> no_loc (StructGet (sequence args, Ast.no_loc "nearest"))
  | Sqrt -> no_loc (StructGet (sequence args, Ast.no_loc "sqrt"))
  | Convert (_, signage) ->
      no_loc
        (Cast
           ( sequence args,
             Signedtype { typ = numtype sz; signage; strict = false } ))
  | Reinterpret -> no_loc (StructGet (sequence args, Ast.no_loc "from_bits"))

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
  | Min -> no_loc (Call (no_loc (Get (Ast.no_loc "min")), [ sequence args ]))
  | Max -> no_loc (Call (no_loc (Get (Ast.no_loc "max")), [ sequence args ]))
  | CopySign ->
      let e1, e2 = two_args args in
      no_loc (Call (no_loc (Get (Ast.no_loc "copysign")), [ e1; e2 ]))
  | Eq -> symbol Eq
  | Ne -> symbol Ne
  | Lt -> symbol (Lt None)
  | Gt -> symbol (Gt None)
  | Le -> symbol (Le None)
  | Ge -> symbol (Ge None)

let blocktype ctx (typ : Src.blocktype option) =
  match typ with
  | None -> { Ast.params = [||]; results = [||] }
  | Some (Valtype ty) -> { Ast.params = [||]; results = [| valtype ctx ty |] }
  | Some (Typeuse (ty, sign)) -> (
      match (ty, sign) with
      | _, Some { Src.params; results } ->
          {
            Ast.params = Array.map (fun t -> valtype ctx t) params;
            results = Array.map (fun t -> valtype ctx t) results;
          }
      | _, None -> assert false (*ZZZ*))

let rec instr st (i : Src.instr) (args : Ast.instr list) : Ast.instr =
  let no_loc : Ast.instr_descr -> _ = Ast.no_loc in
  match i with
  | Block { label; typ; block } ->
      assert (args = []);
      let label, labels = LabelStack.push st.labels label in
      let st = { st with labels } in
      let body = List.map (fun i -> instr st i []) block in
      no_loc (Block (label (), blocktype st typ, body))
  | Loop { label; typ; block } ->
      assert (args = []);
      let label, labels = LabelStack.push st.labels label in
      let st = { st with labels } in
      let body = List.map (fun i -> instr st i []) block in
      no_loc (Loop (label (), blocktype st typ, body))
  | If { label; typ; if_block; else_block } ->
      let label, labels = LabelStack.push st.labels label in
      let st = { st with labels } in
      let if_body = List.map (fun i -> instr st i []) if_block in
      let else_body =
        if else_block = [] then None
        else Some (List.map (fun i -> instr st i []) else_block)
      in
      no_loc
        (If (label (), blocktype st typ, sequence args, if_body, else_body))
  | Unreachable -> sequence (args @ [ no_loc Unreachable ])
  | Nop -> sequence (args @ [ no_loc Nop ])
  | Pop _ -> sequence []
  | Drop -> no_loc (Let ([ (None, None) ], Some (sequence args)))
  | Br i -> no_loc (Br (label st i, sequence_opt args))
  | Br_if i -> no_loc (Br_if (label st i, sequence args))
  | Br_on_null i -> no_loc (Br_on_null (label st i, sequence args))
  | Br_on_non_null i -> no_loc (Br_on_null (label st i, sequence args))
  | Br_on_cast (i, _, t) ->
      (* ZZZ cast? *)
      no_loc (Br_on_cast (label st i, reftype st t, sequence args))
  | Br_on_cast_fail (i, _, t) ->
      (* ZZZ cast? *)
      no_loc (Br_on_cast_fail (label st i, reftype st t, sequence args))
  | Br_table (labels, lab) ->
      no_loc
        (Br_table
           (List.map (fun i -> label st i) (labels @ [ lab ]), sequence args))
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
      let type_name = idx st `Type i in
      let fields = snd (Hashtbl.find st.struct_fields type_name.descr) in
      no_loc
        (Struct
           ( Some (idx st `Type i),
             List.map2 (fun nm i -> (Ast.no_loc nm, i)) fields args ))
  | StructNewDefault i -> no_loc (StructDefault (Some (idx st `Type i)))
  | StructGet (s, t, f) -> (
      let type_name = idx st `Type t in
      let name =
        Sequence.get (fst (Hashtbl.find st.struct_fields type_name.descr)) f
      in
      let e = no_loc (StructGet (sequence args, name)) in
      match s with
      | None -> e
      | Some signage ->
          no_loc (Cast (e, Signedtype { typ = `I32; signage; strict = false })))
  | StructSet (t, f) ->
      let type_name = idx st `Type t in
      let name =
        Sequence.get (fst (Hashtbl.find st.struct_fields type_name.descr)) f
      in
      let e1, e2 = two_args args in
      no_loc (StructSet (e1, name, e2))
  | ArrayNew t ->
      let e1, e2 = two_args args in
      no_loc (Array (Some (idx st `Type t), e1, e2))
  | ArrayNewDefault t ->
      no_loc (ArrayDefault (Some (idx st `Type t), sequence args))
  | ArrayNewFixed (t, n) -> (
      match string_args n args with
      | Some s ->
          no_loc
            (Cast
               ( no_loc (String (Some (idx st `Type t), s)),
                 Valtype (Ref { nullable = false; typ = Type (idx st `Type t) })
               ))
      | None ->
          (*ZZZ take n into account *)
          no_loc (ArrayFixed (Some (idx st `Type t), args)))
  | ArrayGet (s, _t) -> (
      let e1, e2 = two_args args in
      let e = no_loc (ArrayGet (e1, e2)) in
      match s with
      | None -> e
      | Some signage ->
          no_loc (Cast (e, Signedtype { typ = `I32; signage; strict = false })))
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
  | ReturnCall x -> no_loc (TailCall (no_loc (Get (idx st `Func x)), args))
  | ReturnCallRef _ ->
      (* ZZZ cast? *)
      if args = [] then no_loc (TailCall (unit, []))
      else
        let f, l = split_last args in
        no_loc (TailCall (f, l))
  | Return -> no_loc (Return (sequence_opt args))
  | TupleMake _ -> no_loc Unreachable (*ZZZ*)
  | Const (I32 n) | Const (I64 n) ->
      no_loc (Int n) (*ZZZ Negative ints / floats *)
  | Const (F32 f) | Const (F64 f) ->
      let f = if is_integer f then f ^ "." else f in
      no_loc (Float f)
  | RefI31 ->
      no_loc
        (Cast (sequence args, Valtype (Ref { nullable = false; typ = I31 })))
  | I31Get signage ->
      no_loc
        (Cast (sequence args, Signedtype { typ = `I32; signage; strict = false }))
  | I64ExtendI32 signage ->
      no_loc
        (Cast (sequence args, Signedtype { typ = `I64; signage; strict = false }))
  | I32WrapI64 -> no_loc (Cast (sequence args, Valtype I32))
  | F64PromoteF32 -> no_loc (Cast (sequence args, Valtype F64))
  | F32DemoteF64 -> no_loc (Cast (sequence args, Valtype F32))
  | ExternConvertAny ->
      no_loc
        (Cast (sequence args, Valtype (Ref { nullable = true; typ = Extern })))
  | AnyConvertExtern ->
      no_loc
        (Cast (sequence args, Valtype (Ref { nullable = true; typ = Any })))
  | ArrayNewData (t, _) ->
      no_loc (String (Some (idx st `Type t), "foo")) (*ZZZ*)
  | ArrayLen -> no_loc (Call (no_loc (Get (Ast.no_loc "array_len")), args))
  | RefCast t -> no_loc (Cast (sequence args, Valtype (Ref (reftype st t))))
  | RefTest t -> no_loc (Test (sequence args, reftype st t))
  | RefEq ->
      let e1, e2 = two_args args in
      no_loc (BinOp (Eq, e1, e2))
  | RefFunc f -> no_loc (Get (idx st `Func f))
  | RefNull t ->
      no_loc
        (Cast
           (no_loc Null, Valtype (Ref { nullable = true; typ = heaptype st t })))
  | RefIsNull -> no_loc (UnOp (Not, sequence args))
  | Select _ ->
      let e1, e2, e = three_args args in
      no_loc (Select (e, e1, e2))
  (* ZZZZ To implement now *)
  | Try _ | TupleExtract _ | ArrayFill _ | ArrayCopy _ ->
      no_loc Unreachable (* ZZZ *)
  | Throw t -> no_loc (Throw (idx st `Tag t, args))
  | RefAsNonNull -> no_loc (NonNull (sequence args))
  (* signed(x as i8) as i32 ? *)
  (* Later *)
  | ReturnCallIndirect _ | CallIndirect _ | ArrayInitElem _ | ArrayInitData _
  | ArrayNewElem _ | I32Load8 _ | I32Store8 _ ->
      no_loc Unreachable (* ZZZ *)

let bind_locals st l =
  List.map
    (fun (_, t) ->
      Ast.no_loc
        (Ast.Let
           ( [ (Some (Sequence.get_current st.locals), Some (valtype st t)) ],
             None )))
    l

let typeuse kind st (typ, sign) =
  ( Option.map (fun i -> idx st `Type i) typ,
    Option.map
      (fun (p, r) ->
        {
          Ast.named_params =
            List.mapi
              (fun n (id, t) ->
                ( (match kind with
                  | `Func ->
                      Some
                        (idx st `Local
                           (match id with
                           | Some id -> Id id
                           | None -> Num (Int32.of_int n)))
                  | `Sig -> Option.map Ast.no_loc id),
                  valtype st t ))
              p;
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
  | Func { locals; instrs; typ; exports = e; _ } ->
      let st =
        let seq =
          Sequence.make (Namespace.dup st.common_namespace) "local" "x"
        in
        (match typ with
        | _, Some (params, _) ->
            List.iter (fun (id, _) -> Sequence.register seq id []) params;
            Sequence.consume_currents seq
        | _ -> ());
        List.iter (fun (id, _) -> Sequence.register seq id []) locals;
        { st with locals = seq }
      in
      let locals = bind_locals st locals in
      let typ, sign = typeuse `Func st typ in
      Some
        (Func
           {
             name = Sequence.get_current st.functions;
             typ;
             sign;
             body = (None, locals @ List.map (fun i -> instr st i []) instrs);
             attributes = exports e;
           })
  | Import { module_; name; desc; exports = e; _ } -> (
      match desc with
      | Func typ ->
          let typ, sign = typeuse `Sig st typ in
          Some
            (Fundecl
               {
                 name = Sequence.get_current st.functions;
                 typ;
                 sign;
                 attributes = import (module_, name) :: exports e;
               })
      | Tag typ ->
          let typ, sign = typeuse `Sig st typ in
          Some
            (Tag
               {
                 name = Sequence.get_current st.tags;
                 typ;
                 sign;
                 attributes = import (module_, name) :: exports e;
               })
      | Global typ ->
          let typ' = globaltype st typ in
          Some
            (GlobalDecl
               {
                 name = Sequence.get_current st.globals;
                 mut = typ'.mut;
                 typ = typ'.typ;
                 attributes = import (module_, name) :: exports e;
               })
      | Memory _ -> None)
  | Global { typ; init; exports = e; _ } ->
      let typ' = globaltype st typ in
      Some
        (Global
           {
             name = Sequence.get_current st.globals;
             mut = typ'.mut;
             typ = Some typ'.typ;
             def = sequence (List.map (fun i -> instr st i []) init);
             attributes = exports e;
           })
  | Tag { typ; exports = e; _ } ->
      let typ, sign = typeuse `Sig st typ in
      Some
        (Tag
           {
             name = Sequence.get_current st.tags;
             typ;
             sign;
             attributes = exports e;
           })
  | Memory _ | Start _ | Export _ | Elem _ | Data _ -> None

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
let register_names ctx fields =
  List.iter
    (fun (field : Src.modulefield) ->
      match field with
      | Import { id; desc; exports; _ } -> (
          (* ZZZ Check for non-import fields *)
          match desc with
          | Func _ -> ()
          | Memory _ -> Sequence.register ctx.memories id exports
          | Global _ -> Sequence.register ctx.globals id exports
          | Tag _ -> Sequence.register ctx.tags id exports)
      | Types rectype ->
          Array.iter
            (fun (id, ty) ->
              let name = Sequence.register' ctx.types id [] in
              match (ty : Src.subtype).typ with
              | Func _ | Array _ -> ()
              | Struct l ->
                  let seq = Sequence.make (Namespace.make ()) "field" "f" in
                  let fields =
                    Array.map
                      (fun t -> Sequence.register' seq (get_annot t) [])
                      l
                  in
                  Hashtbl.replace ctx.struct_fields name
                    (seq, Array.to_list fields))
            rectype
      | Global { id; exports; _ } -> Sequence.register ctx.globals id exports
      | Func _ | Export _ | Start _ -> ()
      | Elem _ | Data _ -> () (*ZZZ*)
      | Memory { id; exports; _ } -> Sequence.register ctx.memories id exports
      | Tag { id; exports; _ } -> Sequence.register ctx.tags id exports)
    fields;
  List.iter
    (fun (field : Src.modulefield) ->
      match field with
      | Import { id; desc; exports; _ } -> (
          (* ZZZ Check for non-import fields *)
          match desc with
          | Func _ -> Sequence.register ctx.functions id exports
          | Memory _ | Global _ | Tag _ -> ())
      | Func { id; exports; _ } -> Sequence.register ctx.functions id exports
      | Types _ | Global _ | Export _ | Start _ | Elem _ | Data _ | Memory _
      | Tag _ ->
          ())
    fields

let module_ (_, fields) =
  let ctx =
    let common_namespace = Namespace.make () in
    {
      common_namespace;
      types = Sequence.make (Namespace.make ()) "type" "t";
      struct_fields = Hashtbl.create 16;
      globals = Sequence.make common_namespace "global" "x";
      functions = Sequence.make common_namespace "function" "f";
      locals = Sequence.make common_namespace "local" "x";
      memories = Sequence.make (Namespace.make ()) "memories" "m";
      labels = LabelStack.make ();
      tags = Sequence.make (Namespace.make ()) "tag" "t";
    }
  in
  register_names ctx fields;
  List.map (fun f -> modulefield ctx f) fields

(*
Use imports and exports as hints for naming functions
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
