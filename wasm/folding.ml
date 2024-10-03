open Ast.Text

let map_instrs func (name, fields) =
  ( name,
    List.map
      (fun f ->
        match f with
        | Func ({ instrs; _ } as f) -> Func { f with instrs = func instrs }
        | Global ({ init; _ } as g) -> Global { g with init = func init }
        | Elem ({ init; _ } as e) -> Elem { e with init = List.map func init }
        | Types _ | Import _ | Memory _ | Tag _ | Export _ | Start _ | Data _ ->
            f)
      fields )

(****)

let functype_arity { params; results } =
  (Array.length params, Array.length results)

let typeuse_arity (i, ty) =
  match (i, ty) with
  | _, Some t -> functype_arity t
  | Some _, None -> assert false (* ZZZ lookup type in env *)
  | None, None -> assert false

let blocktype_arity t =
  match t with
  | None -> (0, 0)
  | Some (Valtype _) -> (0, 1)
  | Some (Typeuse t) -> typeuse_arity t

let unreachable = 100_000

let arity i =
  match i with
  | Block { typ; _ } | Loop { typ; _ } | If { typ; _ } | Try { typ; _ } ->
      blocktype_arity typ
  | Br _ | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _ | Br_on_cast _
  | Br_on_cast_fail _ | Return (* Label type *)
  | Call _ | ReturnCall _ (* Function type *) | CallRef _ | ReturnCallRef _
  | StructNew _ | StructNewDefault _ (* Types *) ->
      assert false
  | ReturnCallIndirect (_, ty) ->
      let i, _ = typeuse_arity ty in
      (i + 1, unreachable)
  | CallIndirect (_, ty) ->
      let i, o = typeuse_arity ty in
      (i + 1, o)
  | Unreachable -> (0, unreachable)
  | Nop -> (0, 0)
  | Throw _ -> (1, unreachable)
  | Drop -> (1, 0)
  | Select _ -> (3, 1)
  | LocalGet _ -> (0, 1)
  | LocalSet _ -> (1, 0)
  | LocalTee _ -> (1, 1)
  | GlobalGet _ -> (0, 1)
  | GlobalSet _ -> (1, 0)
  | I32Load8 _ -> (1, 1)
  | I32Store8 _ -> (1, 1)
  | RefNull _ -> (0, 1)
  | RefFunc _ -> (0, 1)
  | RefIsNull -> (1, 1)
  | RefAsNonNull -> (1, 1)
  | RefEq -> (2, 1)
  | RefTest _ -> (1, 1)
  | RefCast _ -> (1, 1)
  | StructGet _ -> (1, 1)
  | StructSet _ -> (2, 0)
  | ArrayNew _ -> (2, 1)
  | ArrayNewDefault _ -> (1, 1)
  | ArrayNewFixed (_, n) -> (Int32.to_int n, 1)
  | ArrayNewData _ -> (2, 1)
  | ArrayNewElem _ -> (2, 1)
  | ArrayGet _ -> (2, 1)
  | ArraySet _ -> (3, 0)
  | ArrayLen -> (1, 1)
  | ArrayFill _ -> (4, 0)
  | ArrayCopy _ -> (5, 0)
  | ArrayInitData _ -> (4, 0)
  | ArrayInitElem _ -> (4, 0)
  | RefI31 -> (1, 1)
  | I31Get _ -> (1, 1)
  | Const _ -> (0, 1)
  | UnOp _ -> (2, 1)
  | BinOp _ -> (1, 1)
  | I32WrapI64 -> (1, 1)
  | I64ExtendI32 _ -> (1, 1)
  | F32DemoteF64 -> (1, 1)
  | F64PromoteF32 -> (1, 1)
  | ExternConvertAny -> (1, 1)
  | AnyConvertExtern -> (1, 1)
  | Folded _ -> assert false
  (* Binaryen extensions *)
  | Pop _ -> (0, 1)
  | TupleMake n -> (Int32.to_int n, 1)
  | TupleExtract _ -> (1, 1)

(****)

let push_back tentative_args stream =
  List.rev_append (List.map (fun i -> (0, i)) tentative_args) stream

let rec consume n folded =
  if n = 0 then folded
  else
    match folded with
    | [] -> []
    | (n', i) :: rem ->
        if n >= n' then (0, i) :: consume (n - n') folded
        else (n' - n, i) :: rem

let rec fold_stream folded stream : Ast.Text.instr list =
  match stream with
  | [] -> List.rev (List.map snd folded)
  | (Block ({ block; _ } as b) as i) :: rem ->
      let block = fold_stream [] block in
      let inputs, outputs = arity i in
      let folded = consume inputs folded in
      fold_stream ((outputs, Folded (Block { b with block }, [])) :: folded) rem
  | (Loop ({ block; _ } as b) as i) :: rem ->
      let block = fold_stream [] block in
      let inputs, outputs = arity i in
      let folded = consume inputs folded in
      fold_stream ((outputs, Folded (Loop { b with block }, [])) :: folded) rem
  | (If ({ if_block; else_block; _ } as b) as i) :: rem ->
      let if_block = fold_stream [] if_block in
      let else_block = fold_stream [] else_block in
      let inputs, outputs = arity i in
      fold_instr folded [] [] rem
        (If { b with if_block; else_block })
        inputs outputs
  | Folded (i, l) :: rem -> fold_stream folded (l @ (i :: rem))
  | i :: rem ->
      let inputs, outputs = arity i in
      fold_instr folded [] [] rem i inputs outputs

and fold_instr folded args tentative_args stream i inputs outputs =
  if inputs = 0 then
    fold_stream
      ((outputs, Folded (i, args)) :: push_back tentative_args folded)
      stream
  else
    match folded with
    | [] ->
        fold_stream
          ((outputs, Folded (i, args)) :: push_back tentative_args folded)
          stream
    | (n, i') :: folded' ->
        if n <= inputs then
          if n > 0 then
            let args = (i' :: tentative_args) @ args in
            fold_instr folded' args [] stream i (inputs - n) outputs
          else
            let tentative_args = i' :: tentative_args in
            fold_instr folded' args tentative_args stream i inputs outputs
        else
          fold_stream
            ((outputs, Folded (i, args))
            :: push_back tentative_args ((n - inputs, i') :: folded'))
            stream

let fold m = map_instrs (fold_stream []) m

(****)

let rec unfold_stream stream start =
  List.fold_left
    (fun start i ->
      let unfold_block i =
        match i with
        | Block ({ block; _ } as b) ->
            Block { b with block = unfold_instrs block }
        | Loop ({ block; _ } as b) ->
            Loop { b with block = unfold_instrs block }
        | If ({ if_block; else_block; _ } as b) ->
            If
              {
                b with
                if_block = unfold_instrs if_block;
                else_block = unfold_instrs else_block;
              }
        | Folded _ -> assert false
        | _ -> i
      in
      match i with
      | Folded (i, l) -> unfold_block i :: unfold_stream l start
      | _ -> unfold_block i :: start)
    start stream

and unfold_instrs l = List.rev (unfold_stream l [])

let unfold m = map_instrs unfold_instrs m
