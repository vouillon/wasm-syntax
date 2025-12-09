open Wax
module Src = Wasm.Ast.Text
module Uint32 = Utils.Uint32
module StringMap = Map.Make (String)

module Sequence = struct
  type t = {
    index_mapping : (Uint32.t, string) Hashtbl.t;
    label_mapping : (string, string) Hashtbl.t;
    mutable last_index : int;
    mutable current_index : int;
    namespace : Namespace.t;
    default : string;
  }

  let make namespace default =
    {
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
        | (Some nm, _ | None, nm :: _) when Lexer.is_valid_identifier nm -> nm
        | _ -> seq.default
      in
      Namespace.add seq.namespace name
    in
    let idx = Uint32.of_int seq.last_index in
    seq.last_index <- seq.last_index + 1;
    Hashtbl.add seq.index_mapping idx name;
    Option.iter (fun id -> Hashtbl.add seq.label_mapping id name) id;
    name

  let register seq id exports = ignore (register' seq id exports)

  let get seq (idx : Src.idx) =
    {
      idx with
      desc =
        (match idx.desc with
        | Num n -> Hashtbl.find seq.index_mapping n
        | Id id -> Hashtbl.find seq.label_mapping id);
    }

  let get_current seq =
    let i = seq.current_index in
    seq.current_index <- i + 1;
    Ast.no_loc (Hashtbl.find seq.index_mapping (Uint32.of_int i))

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
    let name =
      Namespace.add ns
        (match label with
        | Some label when Lexer.is_valid_identifier label -> label
        | _ -> "l")
    in
    ( (fun () -> if !used then Some name else None),
      { ns; stack = (label, (name, used)) :: st.stack } )

  let get st (idx : Src.idx) =
    let name, used =
      match idx.desc with
      | Num n -> snd (List.nth st.stack (Uint32.to_int n))
      | Id id -> List.assoc (Some id) st.stack
    in
    used := true;
    name

  let make () = { ns = Namespace.make (); stack = [] }
end

module Tbl = struct
  type 'a t = (string, 'a) Hashtbl.t

  let make () = Hashtbl.create 16
  let find = Hashtbl.find
  let add = Hashtbl.add
end

type ctx = {
  common_namespace : Namespace.t;
  types : Sequence.t;
  struct_fields : (string, Sequence.t * string list) Hashtbl.t;
  globals : Sequence.t;
  functions : Sequence.t;
  memories : Sequence.t;
  tables : Sequence.t;
  tags : Sequence.t;
  locals : Sequence.t;
  labels : LabelStack.t;
  type_defs : Src.subtype Tbl.t;
  function_types : Src.typeuse Tbl.t;
  tag_types : Src.typeuse Tbl.t;
  label_arities : (Src.id option * int) list;
  return_arity : int;
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
    params =
      Array.map
        (fun (id, t) -> (Option.map Ast.no_loc id, valtype st t))
        t.params;
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
                  (Ast.no_loc
                     (match get_annot t with
                      | None -> Num (Uint32.of_int i)
                      | Some id -> Id id
                       : Src.idx_desc)))
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
      annotated name (subtype st name.desc (get_type t)))
    t

let globaltype st = muttype valtype st

type _ kind =
  | Type : Src.subtype kind
  | Func : Src.typeuse kind
  | Tag : Src.typeuse kind

let lookup_type (type typ) ctx (kind : typ kind) idx : typ =
  let get seq tbl idx = Tbl.find tbl (Sequence.get seq idx).desc in
  match kind with
  | Type -> get ctx.types ctx.type_defs idx
  | Func -> get ctx.functions ctx.function_types idx
  | Tag -> get ctx.tags ctx.tag_types idx

let register_type (type typ) ctx (kind : typ kind) idx exports (typ : typ) =
  let register seq tbl idx =
    Tbl.add tbl (Sequence.register' seq idx exports) typ
  in
  match kind with
  | Type -> assert false
  | Func -> register ctx.functions ctx.function_types idx
  | Tag -> register ctx.tags ctx.tag_types idx

let functype_no_bindings_arity { Src.params; results } =
  (Array.length params, Array.length results)

let functype_arity (params, results) = (List.length params, List.length results)

let type_arity ctx idx =
  match (lookup_type ctx Type idx).typ with
  | Func ty -> functype_no_bindings_arity ty
  | Struct _ | Array _ -> assert false

let typeuse_no_bindings_arity ctx (i, ty) =
  match (i, ty) with
  | _, Some t -> functype_no_bindings_arity t
  | Some i, None -> type_arity ctx i
  | None, None -> assert false

let typeuse_arity ctx (i, ty) =
  match (i, ty) with
  | _, Some t -> functype_arity t
  | Some i, None -> type_arity ctx i
  | None, None -> assert false

let blocktype_arity ctx (typ : Src.blocktype option) =
  match typ with
  | None -> (0, 0)
  | Some (Valtype _) -> (0, 1)
  | Some (Typeuse t) -> typeuse_no_bindings_arity ctx t

let function_arity ctx f = typeuse_arity ctx (lookup_type ctx Func f)
let tag_arity ctx t = typeuse_arity ctx (lookup_type ctx Tag t)

let label_arity ctx (idx : Src.idx) =
  match idx.desc with
  | Id id ->
      snd
        (List.find
           (fun e -> match e with Some id', _ -> id = id' | _ -> false)
           ctx.label_arities)
  | Num i -> snd (List.nth ctx.label_arities (Uint32.to_int i))

(*
Step 1: traverse types and find existing names
Step 2: use this info to generate using names without reusing existing names
*)

(*ZZZ
  - first pass to see missing labels
  - explode tuples
*)

module Stack = struct
  type stack = (int option * Ast.location Ast.instr) list
  type 'a t = stack -> stack * 'a

  (* 
  let rec consume n stack =
    if n = 0 then stack
    else
      match stack with
      | [] | (None, _) :: _ -> stack
      | (Some n', i) :: rem ->
          if n >= n' then (Some 0, i) :: consume (n - n') stack
          else (Some (n' - n), i) :: rem
*)

  let rec complete n cur =
    if n = 0 then cur else complete (n - 1) (Ast.no_loc Ast.Pop :: cur)

  let rec grab_rec n stack cur =
    if n = 0 then (stack, cur)
    else
      match stack with
      | (Some 1, instr) :: rem -> grab_rec (n - 1) rem (instr :: cur)
      | _ -> (stack, complete n cur)

  let consume inputs stack =
    if inputs = 0 then (stack, ())
    else
      ( (match stack with
        | (Some 1, instr) :: rem -> (None, instr) :: rem
        | _ -> stack),
        () )

  let grab n stack = grab_rec n stack []
  let push arity i stack = ((Some arity, i) :: stack, ())
  let push_poly i stack = ((None, i) :: stack, ())

  let pop stack =
    match stack with
    | (Some 1, i) :: rem -> (rem, i)
    | _ -> (stack, Ast.no_loc Ast.Pop)

  let try_pop stack =
    match stack with (Some 1, i) :: rem -> (rem, Some i) | _ -> (stack, None)

  let run f =
    let st, () = f [] in
    List.rev_map snd st
end

let ( let* ) e f st =
  let st, v = e st in
  f v st

let return v st = (st, v)
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

let reasonable_string =
  Re.(
    compile
      (whole_string
         (rep
            (alt
               [ diff any (rg '\000' '\031'); char '\n'; char '\r'; char '\t' ]))))

let string_args n args =
  try
    if Uint32.of_int (List.length args) <> n then raise Exit;
    List.iter
      (fun arg ->
        match arg.Ast.desc with
        | Ast.Int c
          when let c = int_of_string c in
               c >= 0 && c < 256 ->
            ()
        | _ -> raise Exit)
      args;
    let b = Bytes.create (Uint32.to_int n) in
    List.iteri
      (fun i arg ->
        match arg.Ast.desc with
        | Ast.Int c -> Bytes.set b i (Char.chr (int_of_string c))
        | _ -> assert false)
      args;
    let s = Bytes.to_string b in
    if String.is_valid_utf_8 s && Re.execp reasonable_string s then Some s
    else None
  with Exit -> None

let inttype ty : Ast.valtype =
  match ty with
  | `I32 -> I32
  | `I64 -> I64
  | `F32 -> I32
  | `F64 -> I64
  | _ -> assert false

let floattype ty : Ast.valtype =
  match ty with
  | `I32 -> F32
  | `I64 -> F64
  | `F32 -> F32
  | `F64 -> F64
  | _ -> assert false

let int_un_op i0 sz (op : Src.int_un_op) =
  let with_loc (i : _ Ast.instr_desc) = { i0 with Ast.desc = i } in
  let* e' = Stack.try_pop in
  let e ty =
    match e' with
    | Some e -> e
    | None -> Ast.no_loc (Ast.Cast (Ast.no_loc Ast.Pop, Valtype ty))
  in
  Stack.push 1
    (match op with
    | Clz -> with_loc (StructGet (e (inttype sz), Ast.no_loc "clz"))
    | Ctz -> with_loc (StructGet (e (inttype sz), Ast.no_loc "ctz"))
    | Popcnt -> with_loc (StructGet (e (inttype sz), Ast.no_loc "popcnt"))
    | Eqz -> with_loc (UnOp (Not, e (inttype sz)))
    | Trunc (_, signage) ->
        with_loc
          (Cast
             (e (floattype sz), Signedtype { typ = sz; signage; strict = true }))
    | TruncSat (_, signage) ->
        with_loc
          (Cast
             (e (floattype sz), Signedtype { typ = sz; signage; strict = false }))
    | Reinterpret ->
        with_loc
          (StructGet
             ( (let e = e (floattype sz) in
                if e' = None then e
                else { e with desc = Ast.Cast (e, Valtype (floattype sz)) }),
               Ast.no_loc "to_bits" ))
    | ExtendS `_32 ->
        (* i64.extend32_s *)
        with_loc
          (Cast
             ( (let e = e (inttype `I32) in
                if e' = None then e
                else { e with desc = Ast.Cast (e, Valtype (inttype `I32)) }),
               Signedtype { typ = sz; signage = Signed; strict = false } ))
    | ExtendS _ -> with_loc Unreachable (* ZZZ *))

let int_bin_op i0 (op : Src.int_bin_op) =
  let with_loc (i : _ Ast.instr_desc) = { i0 with Ast.desc = i } in
  let symbol op =
    let* e2 = Stack.pop in
    let* e1 = Stack.pop in
    Stack.push 1 (with_loc (BinOp (op, e1, e2)))
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
      let* args = Stack.grab 2 in
      Stack.push 1 (with_loc (Call (with_loc (Get (Ast.no_loc "rotl")), args)))
  | Rotr ->
      let* args = Stack.grab 2 in
      Stack.push 1 (with_loc (Call (with_loc (Get (Ast.no_loc "rotr")), args)))
  | Eq -> symbol Eq
  | Ne -> symbol Ne
  | Lt s -> symbol (Lt (Some s))
  | Gt s -> symbol (Gt (Some s))
  | Le s -> symbol (Le (Some s))
  | Ge s -> symbol (Ge (Some s))

let float_un_op i0 sz (op : Src.float_un_op) =
  let with_loc (i : _ Ast.instr_desc) = { i0 with Ast.desc = i } in
  let* e' = Stack.try_pop in
  let e ty =
    match e' with
    | Some e -> e
    | None -> Ast.no_loc (Ast.Cast (Ast.no_loc Ast.Pop, Valtype ty))
  in
  Stack.push 1
    (match op with
    | Neg -> with_loc (UnOp (Neg, e (floattype sz)))
    | Abs -> with_loc (StructGet (e (floattype sz), Ast.no_loc "abs"))
    | Ceil -> with_loc (StructGet (e (floattype sz), Ast.no_loc "ceil"))
    | Floor -> with_loc (StructGet (e (floattype sz), Ast.no_loc "floor"))
    | Trunc -> with_loc (StructGet (e (floattype sz), Ast.no_loc "trunc"))
    | Nearest -> with_loc (StructGet (e (floattype sz), Ast.no_loc "nearest"))
    | Sqrt -> with_loc (StructGet (e (floattype sz), Ast.no_loc "sqrt"))
    | Convert (sz', signage) ->
        with_loc
          (Cast
             ( e (inttype (sz' :> [ `I32 | `I64 | `F32 | `F64 ])),
               Signedtype { typ = sz; signage; strict = false } ))
    | Reinterpret ->
        with_loc
          (StructGet
             ( (let e = e (inttype sz) in
                if e' = None then e
                else { e with desc = Ast.Cast (e, Valtype (inttype sz)) }),
               Ast.no_loc "from_bits" )))

let float_bin_op i0 (op : Src.float_bin_op) =
  let with_loc (i : _ Ast.instr_desc) = { i0 with Ast.desc = i } in
  let symbol op =
    let* e2 = Stack.pop in
    let* e1 = Stack.pop in
    Stack.push 1 (with_loc (BinOp (op, e1, e2)))
  in
  match op with
  | Add -> symbol Add
  | Sub -> symbol Sub
  | Mul -> symbol Mul
  | Div -> symbol (Div None)
  | Min ->
      let* args = Stack.grab 2 in
      Stack.push 1 (with_loc (Call (with_loc (Get (Ast.no_loc "min")), args)))
  | Max ->
      let* args = Stack.grab 2 in
      Stack.push 1 (with_loc (Call (with_loc (Get (Ast.no_loc "max")), args)))
  | CopySign ->
      let* args = Stack.grab 2 in
      Stack.push 1
        (with_loc (Call (with_loc (Get (Ast.no_loc "copysign")), args)))
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
  | Some (Typeuse (ty_idx, sign)) -> (
      match (ty_idx, sign) with
      | _, Some { Src.params; results } ->
          {
            Ast.params =
              Array.map
                (fun (id, t) -> (Option.map Ast.no_loc id, valtype ctx t))
                params;
            results = Array.map (fun t -> valtype ctx t) results;
          }
      | _, None -> assert false (*ZZZ*))

let push_label ctx ~loop label typ =
  let arity = blocktype_arity ctx typ in
  let i = if loop then fst arity else snd arity in
  let label_arities = (label, i) :: ctx.label_arities in
  let label, labels = LabelStack.push ctx.labels label in
  (label, { ctx with labels; label_arities })

let rec instruction ctx (i : _ Src.instr) : unit Stack.t =
  let with_loc (i' : _ Ast.instr_desc) = { i with Ast.desc = i' } in
  match i.desc with
  | Block { label; typ; block } ->
      let label, ctx = push_label ctx ~loop:false label typ in
      let body = Stack.run (instructions ctx block) in
      let inputs, outputs = blocktype_arity ctx typ in
      let* () = Stack.consume inputs in
      Stack.push outputs (with_loc (Block (label (), blocktype ctx typ, body)))
  | Loop { label; typ; block } ->
      let label, ctx = push_label ctx ~loop:true label typ in
      let body = Stack.run (instructions ctx block) in
      let inputs, outputs = blocktype_arity ctx typ in
      let* () = Stack.consume inputs in
      Stack.push outputs (with_loc (Loop (label (), blocktype ctx typ, body)))
  | If { label; typ; if_block; else_block } ->
      let label, ctx = push_label ctx ~loop:false label typ in
      let if_body = Stack.run (instructions ctx if_block) in
      let else_body =
        if else_block = [] then None
        else Some (Stack.run (instructions ctx else_block))
      in
      let inputs, outputs = blocktype_arity ctx typ in
      let* cond = Stack.pop in
      let* () = Stack.consume inputs in
      Stack.push outputs
        (with_loc (If (label (), blocktype ctx typ, cond, if_body, else_body)))
  | Try { label; typ; block; catches; catch_all } ->
      let label, ctx = push_label ctx ~loop:false label typ in
      let block = Stack.run (instructions ctx block) in
      let catches =
        List.map
          (fun (t, block) ->
            (idx ctx `Tag t, Stack.run (instructions ctx block)))
          catches
      in
      let catch_all =
        Option.map (fun block -> Stack.run (instructions ctx block)) catch_all
      in
      let _, outputs = blocktype_arity ctx typ in
      Stack.push outputs
        (with_loc
           (Try
              {
                label = label ();
                typ = blocktype ctx typ;
                block;
                catches;
                catch_all;
              }))
  | Unreachable -> Stack.push_poly (with_loc Unreachable)
  | Nop -> Stack.push 0 (with_loc Nop)
  | Pop _ ->
      let* () = Stack.consume 1 in
      Stack.push 1 (with_loc Pop)
  | Drop ->
      let* e = Stack.pop in
      Stack.push 0 (with_loc (Set (None, e)))
  | Br i ->
      let input = label_arity ctx i in
      let* args = Stack.grab input in
      Stack.push_poly (with_loc (Br (label ctx i, sequence_opt args)))
  | Br_if i ->
      let input = label_arity ctx i in
      let* args = Stack.grab (input + 1) in
      Stack.push input (with_loc (Br_if (label ctx i, sequence args)))
  | Br_table (labels, lab) ->
      let input = label_arity ctx lab in
      let* args = Stack.grab (input + 1) in
      Stack.push_poly
        (with_loc
           (Br_table
              (List.map (fun i -> label ctx i) (labels @ [ lab ]), sequence args)))
  | Br_on_null i ->
      let input = label_arity ctx i in
      let* args = Stack.grab (input + 1) in
      Stack.push (input + 1)
        (with_loc (Br_on_null (label ctx i, sequence args)))
  | Br_on_non_null i ->
      let input = label_arity ctx i in
      let* args = Stack.grab input in
      Stack.push (input - 1)
        (with_loc (Br_on_non_null (label ctx i, sequence args)))
  | Br_on_cast (i, _, t) ->
      let input = label_arity ctx i in
      let* args = Stack.grab input in
      Stack.push input
        (with_loc (Br_on_cast (label ctx i, reftype ctx t, sequence args)))
  | Br_on_cast_fail (i, _, t) ->
      let input = label_arity ctx i in
      let* args = Stack.grab input in
      Stack.push input
        (with_loc (Br_on_cast_fail (label ctx i, reftype ctx t, sequence args)))
  | Folded (i, l) ->
      let* () = instructions ctx l in
      instruction ctx i
  | LocalGet x -> Stack.push 1 (with_loc (Get (idx ctx `Local x)))
  | GlobalGet x -> Stack.push 1 (with_loc (Get (idx ctx `Global x)))
  | LocalSet x ->
      let* e = Stack.pop in
      Stack.push 0 (with_loc (Set (Some (idx ctx `Local x), e)))
  | GlobalSet x ->
      let* e = Stack.pop in
      Stack.push 0 (with_loc (Set (Some (idx ctx `Global x), e)))
  | LocalTee x ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (Tee (idx ctx `Local x, e)))
  | BinOp (I32 op) | BinOp (I64 op) -> int_bin_op i op
  | BinOp (F32 op) | BinOp (F64 op) -> float_bin_op i op
  | UnOp (I64 op) -> int_un_op i `I64 op
  | UnOp (I32 op) -> int_un_op i `I32 op
  | UnOp (F64 op) -> float_un_op i `F64 op
  | UnOp (F32 op) -> float_un_op i `F32 op
  | StructNew i ->
      let type_name = idx ctx `Type i in
      let fields = snd (Hashtbl.find ctx.struct_fields type_name.desc) in
      let* args = Stack.grab (List.length fields) in
      Stack.push 1
        (with_loc
           (Struct
              ( Some (idx ctx `Type i),
                List.map2 (fun nm i -> (Ast.no_loc nm, i)) fields args )))
  | StructNewDefault i ->
      Stack.push 1 (with_loc (StructDefault (Some (idx ctx `Type i))))
  | StructGet (s, t, f) ->
      let type_name = idx ctx `Type t in
      let name =
        Sequence.get (fst (Hashtbl.find ctx.struct_fields type_name.desc)) f
      in
      let* arg = Stack.pop in
      let e = with_loc (StructGet (arg, name)) in
      Stack.push 1
        (match s with
        | None -> e
        | Some signage ->
            with_loc
              (Cast (e, Signedtype { typ = `I32; signage; strict = false })))
  | StructSet (t, f) ->
      let type_name = idx ctx `Type t in
      let name =
        Sequence.get (fst (Hashtbl.find ctx.struct_fields type_name.desc)) f
      in
      let* e2 = Stack.pop in
      let* e1 = Stack.pop in
      Stack.push 1 (with_loc (StructSet (e1, name, e2)))
  | ArrayNew t ->
      let* len = Stack.pop in
      let* v = Stack.pop in
      Stack.push 1 (with_loc (Array (Some (idx ctx `Type t), v, len)))
  | ArrayNewDefault t ->
      let* len = Stack.pop in
      Stack.push 1 (with_loc (ArrayDefault (Some (idx ctx `Type t), len)))
  | ArrayNewFixed (t, n) ->
      let* args = Stack.grab (Uint32.to_int n) in
      Stack.push 1
        (match string_args n args with
        | Some s ->
            with_loc
              (Cast
                 ( with_loc (String (Some (idx ctx `Type t), s)),
                   Valtype
                     (Ref { nullable = false; typ = Type (idx ctx `Type t) }) ))
        | None -> with_loc (ArrayFixed (Some (idx ctx `Type t), args)))
  | ArrayGet (s, t) ->
      let* e2 = Stack.pop in
      let* e1 = Stack.pop in
      let e1 =
        {
          e1 with
          desc =
            Ast.Cast
              ( e1,
                Valtype (Ref { nullable = true; typ = Type (idx ctx `Type t) })
              );
        }
      in
      let e = with_loc (ArrayGet (e1, e2)) in
      Stack.push 1
        (match s with
        | None -> e
        | Some signage ->
            with_loc
              (Cast (e, Signedtype { typ = `I32; signage; strict = false })))
  | ArraySet t ->
      let* e3 = Stack.pop in
      let* e2 = Stack.pop in
      let* e1 = Stack.pop in
      let e1 =
        {
          e1 with
          desc =
            Ast.Cast
              ( e1,
                Valtype (Ref { nullable = true; typ = Type (idx ctx `Type t) })
              );
        }
      in
      Stack.push 1 (with_loc (ArraySet (e1, e2, e3)))
  | Call f ->
      let input, output = function_arity ctx f in
      let* args = Stack.grab input in
      Stack.push output
        (with_loc (Call (with_loc (Get (idx ctx `Func f)), args)))
  | CallRef t ->
      let input, output = type_arity ctx t in
      let* f = Stack.pop in
      let f =
        {
          f with
          desc =
            Ast.Cast
              ( f,
                Valtype (Ref { nullable = true; typ = Type (idx ctx `Type t) })
              );
        }
      in
      let* args = Stack.grab input in
      Stack.push output (with_loc (Call (f, args)))
  | ReturnCall f ->
      let input, _ = function_arity ctx f in
      let* args = Stack.grab input in
      Stack.push_poly
        (with_loc (TailCall (with_loc (Get (idx ctx `Func f)), args)))
  | ReturnCallRef t ->
      let input, _ = type_arity ctx t in
      let* f = Stack.pop in
      let f =
        {
          f with
          desc =
            Ast.Cast
              ( f,
                Valtype (Ref { nullable = true; typ = Type (idx ctx `Type t) })
              );
        }
      in
      let* args = Stack.grab input in
      Stack.push_poly (with_loc (TailCall (f, args)))
  | Return ->
      let* args = Stack.grab ctx.return_arity in
      Stack.push_poly (with_loc (Return (sequence_opt args)))
  | TupleMake _ -> return ()
  | Const (I32 n) | Const (I64 n) ->
      Stack.push 1 (with_loc (Int n)) (*ZZZ Negative ints / floats *)
  | Const (F32 f) | Const (F64 f) ->
      let f = if is_integer f then f ^ "." else f in
      (*ZZZ ???*)
      Stack.push 1 (with_loc (Float f))
  | RefI31 ->
      let* e = Stack.pop in
      Stack.push 1
        (with_loc (Cast (e, Valtype (Ref { nullable = false; typ = I31 }))))
  | I31Get signage ->
      let* e = Stack.pop in
      Stack.push 1
        (with_loc
           (Cast (e, Signedtype { typ = `I32; signage; strict = false })))
  | I64ExtendI32 signage ->
      let* e = Stack.pop in
      Stack.push 1
        (with_loc
           (Cast (e, Signedtype { typ = `I64; signage; strict = false })))
  | I32WrapI64 ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (Cast (e, Valtype I32)))
  | F64PromoteF32 ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (Cast (e, Valtype F64)))
  | F32DemoteF64 ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (Cast (e, Valtype F32)))
  | ExternConvertAny ->
      let* e = Stack.pop in
      Stack.push 1
        (with_loc (Cast (e, Valtype (Ref { nullable = true; typ = Extern }))))
  | AnyConvertExtern ->
      let* e = Stack.pop in
      Stack.push 1
        (with_loc (Cast (e, Valtype (Ref { nullable = true; typ = Any }))))
  | ArrayNewData (t, _) ->
      (*ZZZ*)
      let* _ = Stack.grab 2 in
      Stack.push 1 (with_loc (String (Some (idx ctx `Type t), "foo")))
  | ArrayLen ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (StructGet (e, Ast.no_loc "length")))
  | RefCast t ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (Cast (e, Valtype (Ref (reftype ctx t)))))
  | RefTest t ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (Test (e, reftype ctx t)))
  | RefEq ->
      let* e2 = Stack.pop in
      let* e1 = Stack.pop in
      Stack.push 1 (with_loc (BinOp (Eq, e1, e2)))
  | RefFunc f -> Stack.push 1 (with_loc (Get (idx ctx `Func f)))
  | RefNull typ ->
      Stack.push 1
        (with_loc
           (Cast
              ( with_loc Null,
                Valtype (Ref { nullable = true; typ = heaptype ctx typ }) )))
  | RefIsNull ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (UnOp (Not, e)))
  | Select None ->
      let* cond = Stack.pop in
      let* e2 = Stack.pop in
      let* e1 = Stack.pop in
      Stack.push 1 (with_loc (Select (cond, e1, e2)))
  | Select (Some ty) ->
      let* cond = Stack.pop in
      let* e2 = Stack.pop in
      let* e1 = Stack.pop in
      let ty = match ty with [ ty ] -> valtype ctx ty | _ -> assert false in
      Stack.push 1
        (with_loc
           (Select
              ( cond,
                { e1 with desc = Cast (e1, Valtype ty) },
                { e1 with desc = Cast (e2, Valtype ty) } )))
  | Throw t ->
      let input, _ = tag_arity ctx t in
      let* args = Stack.grab input in
      Stack.push_poly (with_loc (Throw (idx ctx `Tag t, args)))
  | ThrowRef ->
      let* e = Stack.pop in
      Stack.push_poly (with_loc (ThrowRef e))
  | RefAsNonNull ->
      let* e = Stack.pop in
      Stack.push 1 (with_loc (NonNull e))
  | ArrayFill _ ->
      let* n = Stack.pop in
      let* v = Stack.pop in
      let* i = Stack.pop in
      let* a = Stack.pop in
      Stack.push 0
        (with_loc
           (Call (with_loc (StructGet (a, Ast.no_loc "fill")), [ i; v; n ])))
  | ArrayCopy _ ->
      let* n = Stack.pop in
      let* i2 = Stack.pop in
      let* a2 = Stack.pop in
      let* i1 = Stack.pop in
      let* a1 = Stack.pop in
      Stack.push 0
        (with_loc
           (Call
              (with_loc (StructGet (a1, Ast.no_loc "copy")), [ i1; a2; i2; n ])))
  (* Later *)
  | TryTable _ | ReturnCallIndirect _ | CallIndirect _ | ArrayInitElem _
  | ArrayInitData _ | ArrayNewElem _ | Load _ | LoadS _ | Store _ | StoreS _
  | MemorySize _ | MemoryGrow _ | MemoryFill _ | MemoryCopy _ | MemoryInit _
  | DataDrop _ | TableGet _ | TableSet _ | TableSize _ | TableGrow _
  | TableFill _ | TableCopy _ | TableInit _ | ElemDrop _ | TupleExtract _ ->
      Stack.push_poly (with_loc Unreachable)
  | VecConst _ | VecUnOp _ | VecBinOp _ | VecTest _ | VecShift _ | VecBitmask _
  | VecLoad _ | VecStore _ | VecLoadLane _ | VecStoreLane _ | VecLoadSplat _
  | VecExtract _ | VecReplace _ | VecSplat _ | VecShuffle _ | VecBitselect
  | VecTernOp _ ->
      failwith "SIMD instructions not supported in Wax"

(* ZZZ *)

and instructions ctx l =
  match l with
  | [] -> return ()
  | i :: rem ->
      let* () = instruction ctx i in
      instructions ctx rem

let bind_locals st l =
  List.map
    (fun (_, t) ->
      Ast.no_loc
        (Ast.Let
           ( [ (Some (Sequence.get_current st.locals), Some (valtype st t)) ],
             None )))
    l

let typeuse kind ctx (typ, sign) =
  ( Option.map (fun i -> idx ctx `Type i) typ,
    match (kind, typ, sign) with
    | `Sig, _, None -> None
    | `Sig, _, Some (p, r) ->
        Some
          {
            Ast.named_params =
              List.mapi
                (fun _ (id, t) -> (Option.map Ast.no_loc id, valtype ctx t))
                p;
            results = List.map (fun t -> valtype ctx t) r;
          }
    | `Func, None, None -> assert false
    | `Func, Some typ, None -> (
        match (lookup_type ctx Type typ).typ with
        | Struct _ | Array _ -> assert false
        | Func ty ->
            Some
              {
                Ast.named_params =
                  Array.to_list
                    (Array.mapi
                       (fun n (_, t) ->
                         ( Some
                             (idx ctx `Local
                                (Ast.no_loc
                                   (Num (Uint32.of_int n) : Src.idx_desc))),
                           valtype ctx t ))
                       ty.params);
                results =
                  Array.to_list (Array.map (fun t -> valtype ctx t) ty.results);
              })
    | `Func, _, Some (p, r) ->
        Some
          {
            Ast.named_params =
              List.mapi
                (fun n (id, t) ->
                  ( Some
                      (idx ctx `Local
                         (Ast.no_loc
                            (match id with
                             | Some id -> Id id
                             | None -> Num (Uint32.of_int n)
                              : Src.idx_desc))),
                    valtype ctx t ))
                p;
            results = List.map (fun t -> valtype ctx t) r;
          } )

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

let single_expression l = match l with [ e ] -> e | _ -> assert false

let modulefield ctx (f : _ Src.modulefield) : _ Ast.modulefield option =
  match f with
  | Types t -> Some (Type (rectype ctx t))
  | Func { locals; instrs; typ; exports = e; _ } ->
      let label, labels = LabelStack.push (LabelStack.make ()) None in
      let ctx =
        let return_arity = snd (typeuse_arity ctx typ) in
        {
          ctx with
          locals = Sequence.make (Namespace.dup ctx.common_namespace) "x";
          labels;
          label_arities = [ (None, return_arity) ];
          return_arity;
        }
      in
      (match typ with
      | _, Some (params, _) ->
          List.iter (fun (id, _) -> Sequence.register ctx.locals id []) params;
          Sequence.consume_currents ctx.locals
      | Some idx, _ -> (
          match (lookup_type ctx Type idx).typ with
          | Func ty ->
              Array.iter
                (fun _ -> Sequence.register ctx.locals None [])
                ty.params
          | Struct _ | Array _ -> assert false)
      | None, None -> ());
      List.iter (fun (id, _) -> Sequence.register ctx.locals id []) locals;
      let locals = bind_locals ctx locals in
      let typ, sign = typeuse `Func ctx typ in
      Some
        (Func
           {
             name = Sequence.get_current ctx.functions;
             typ;
             sign;
             body = (label (), locals @ Stack.run (instructions ctx instrs));
             attributes = exports e;
           })
  | Import { module_; name; desc; exports = e; _ } -> (
      match desc with
      | Func typ ->
          let typ, sign = typeuse `Sig ctx typ in
          Some
            (Fundecl
               {
                 name = Sequence.get_current ctx.functions;
                 typ;
                 sign;
                 attributes = import (module_, name) :: exports e;
               })
      | Tag typ ->
          let typ, sign = typeuse `Sig ctx typ in
          Some
            (Tag
               {
                 name = Sequence.get_current ctx.tags;
                 typ;
                 sign;
                 attributes = import (module_, name) :: exports e;
               })
      | Global typ ->
          let typ' = globaltype ctx typ in
          Some
            (GlobalDecl
               {
                 name = Sequence.get_current ctx.globals;
                 mut = typ'.mut;
                 typ = typ'.typ;
                 attributes = import (module_, name) :: exports e;
               })
      | Memory _ | Table _ -> None (*ZZZ*))
  | Global { typ; init; exports = e; _ } ->
      let typ' = globaltype ctx typ in
      Some
        (Global
           {
             name = Sequence.get_current ctx.globals;
             mut = typ'.mut;
             typ = Some typ'.typ;
             def = single_expression (Stack.run (instructions ctx init));
             attributes = exports e;
           })
  | Tag { typ; exports = e; _ } ->
      let typ, sign = typeuse `Sig ctx typ in
      Some
        (Tag
           {
             name = Sequence.get_current ctx.tags;
             typ;
             sign;
             attributes = exports e;
           })
  | Memory _ | Table _ | Start _ | Export _ | Elem _ | Data _ -> None

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
    (fun (field : _ Src.modulefield) ->
      match field with
      | Import { id; desc; exports; _ } -> (
          (* ZZZ Check for non-import fields *)
          match desc with
          | Func _ -> ()
          | Memory _ -> Sequence.register ctx.memories id exports
          | Table _ -> Sequence.register ctx.tables id exports
          | Global _ -> Sequence.register ctx.globals id exports
          | Tag ty -> register_type ctx Tag id exports ty)
      | Types rectype ->
          Array.iter
            (fun (id, ty) ->
              let name = Sequence.register' ctx.types id [] in
              Tbl.add ctx.type_defs name ty;
              match (ty : Src.subtype).typ with
              | Func _ | Array _ -> ()
              | Struct l ->
                  let seq = Sequence.make (Namespace.make ()) "f" in
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
      | Table { id; exports; _ } -> Sequence.register ctx.tables id exports
      | Tag { id; exports; typ; _ } -> register_type ctx Tag id exports typ)
    fields;
  List.iter
    (fun (field : _ Src.modulefield) ->
      match field with
      | Import { id; desc; exports; _ } -> (
          (* ZZZ Check for non-import fields *)
          match desc with
          | Func typ -> register_type ctx Func id exports typ
          | Memory _ | Table _ | Global _ | Tag _ -> ())
      | Func { id; exports; typ; _ } -> register_type ctx Func id exports typ
      | Types _ | Global _ | Export _ | Start _ | Elem _ | Data _ | Memory _
      | Table _ | Tag _ ->
          ())
    fields

let module_ (_, fields) =
  let ctx =
    let common_namespace = Namespace.make () in
    {
      common_namespace;
      types = Sequence.make (Namespace.make ()) "t";
      struct_fields = Hashtbl.create 16;
      globals = Sequence.make common_namespace "x";
      functions = Sequence.make common_namespace "f";
      locals = Sequence.make common_namespace "x";
      memories = Sequence.make (Namespace.make ()) "m";
      tables = Sequence.make (Namespace.make ()) "m";
      labels = LabelStack.make ();
      tags = Sequence.make (Namespace.make ()) "t";
      type_defs = Tbl.make ();
      function_types = Tbl.make ();
      tag_types = Tbl.make ();
      label_arities = [];
      return_arity = 0;
    }
  in
  register_names ctx fields;
  List.filter_map (fun f -> modulefield ctx f) fields

(*
- Collect exports to associate them to the corresponding module field

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
*)
