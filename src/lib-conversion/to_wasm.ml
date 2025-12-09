module Uint32 = Utils.Uint32
module Ast = Wasm.Ast
module Binary = Ast.Binary
module Text = Ast.Text
open Wax.Ast

exception Type_error of location * string

module StringMap = Map.Make (String)

type ctx = {
  globals : (string, unit) Hashtbl.t;
  functions : (string, unit) Hashtbl.t;
  mutable locals : string StringMap.t;
  allocated_locals : (string option * Text.valtype) list ref;
  namespace : Namespace.t;
  type_kinds : (string, [ `Struct | `Array | `Func ]) Hashtbl.t;
  struct_fields : (string, string list) Hashtbl.t;
  referenced_functions : (string, unit) Hashtbl.t;
}

let with_loc loc desc = { desc; info = loc }
let index wax_idx : Text.idx = with_loc wax_idx.info (Text.Id wax_idx.desc)

let rec heaptype (h : heaptype) : Text.heaptype =
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
  | Type idx -> Type (index idx)

and valtype ty : Text.valtype =
  match ty with
  | I32 -> I32
  | I64 -> I64
  | F32 -> F32
  | F64 -> F64
  | V128 -> V128
  | Ref { nullable; typ } -> Ref { nullable; typ = heaptype typ }
  | Tuple l -> Tuple (List.map valtype l)

let reftype r : Text.reftype = { nullable = r.nullable; typ = heaptype r.typ }
let unpack_type f = match f with Value v -> v | Packed _ -> I32

let functype typ : Text.functype =
  let params =
    Array.map
      (fun (id, t) -> (Option.map (fun id -> id.desc) id, valtype t))
      typ.params
  in
  let results = Array.map valtype typ.results in
  { params; results }

let blocktype typ : Text.blocktype option =
  match (typ.params, typ.results) with
  | [||], [||] -> None
  | [||], [| typ |] -> Some (Valtype (valtype typ))
  | _ -> Some (Typeuse (None, Some (functype typ)))

let print_instr i =
  Format.eprintf "%a@."
    (fun f i -> Utils.Printer.run f (fun pp -> Wax.Output.instr pp i))
    i

(*
let print_storagetype i =
  Format.eprintf "%a@."
    (fun f i -> Utils.Printer.run f (fun pp -> Wax.Output.storagetype pp i))
    i
*)
let print_valtype i =
  Format.eprintf "%a@."
    (fun f i -> Utils.Printer.run f (fun pp -> Wax.Output.valtype pp i))
    i

let binop i op operand_type : _ Text.instr_desc =
  match (op, operand_type) with
  | Add, I32 -> BinOp (I32 Add)
  | Sub, I32 -> BinOp (I32 Sub)
  | Mul, I32 -> BinOp (I32 Mul)
  | Div (Some Signed), I32 -> BinOp (I32 (Div Signed))
  | Div (Some Unsigned), I32 -> BinOp (I32 (Div Unsigned))
  | Rem Signed, I32 -> BinOp (I32 (Rem Signed))
  | Rem Unsigned, I32 -> BinOp (I32 (Rem Unsigned))
  | And, I32 -> BinOp (I32 And)
  | Or, I32 -> BinOp (I32 Or)
  | Xor, I32 -> BinOp (I32 Xor)
  | Shl, I32 -> BinOp (I32 Shl)
  | Shr Signed, I32 -> BinOp (I32 (Shr Signed))
  | Shr Unsigned, I32 -> BinOp (I32 (Shr Unsigned))
  | Eq, I32 -> BinOp (I32 Eq)
  | Ne, I32 -> BinOp (I32 Ne)
  | Lt (Some Signed), I32 -> BinOp (I32 (Lt Signed))
  | Lt (Some Unsigned), I32 -> BinOp (I32 (Lt Unsigned))
  | Gt (Some Signed), I32 -> BinOp (I32 (Gt Signed))
  | Gt (Some Unsigned), I32 -> BinOp (I32 (Gt Unsigned))
  | Le (Some Signed), I32 -> BinOp (I32 (Le Signed))
  | Le (Some Unsigned), I32 -> BinOp (I32 (Le Unsigned))
  | Ge (Some Signed), I32 -> BinOp (I32 (Ge Signed))
  | Ge (Some Unsigned), I32 -> BinOp (I32 (Ge Unsigned))
  | Add, I64 -> BinOp (I64 Add)
  | Sub, I64 -> BinOp (I64 Sub)
  | Mul, I64 -> BinOp (I64 Mul)
  | Div (Some Signed), I64 -> BinOp (I64 (Div Signed))
  | Div (Some Unsigned), I64 -> BinOp (I64 (Div Unsigned))
  | Rem Signed, I64 -> BinOp (I64 (Rem Signed))
  | Rem Unsigned, I64 -> BinOp (I64 (Rem Unsigned))
  | And, I64 -> BinOp (I64 And)
  | Or, I64 -> BinOp (I64 Or)
  | Xor, I64 -> BinOp (I64 Xor)
  | Shl, I64 -> BinOp (I64 Shl)
  | Shr Signed, I64 -> BinOp (I64 (Shr Signed))
  | Shr Unsigned, I64 -> BinOp (I64 (Shr Unsigned))
  | Eq, I64 -> BinOp (I64 Eq)
  | Ne, I64 -> BinOp (I64 Ne)
  | Lt (Some Signed), I64 -> BinOp (I64 (Lt Signed))
  | Lt (Some Unsigned), I64 -> BinOp (I64 (Lt Unsigned))
  | Gt (Some Signed), I64 -> BinOp (I64 (Gt Signed))
  | Gt (Some Unsigned), I64 -> BinOp (I64 (Gt Unsigned))
  | Le (Some Signed), I64 -> BinOp (I64 (Le Signed))
  | Le (Some Unsigned), I64 -> BinOp (I64 (Le Unsigned))
  | Ge (Some Signed), I64 -> BinOp (I64 (Ge Signed))
  | Ge (Some Unsigned), I64 -> BinOp (I64 (Ge Unsigned))
  | Add, F32 -> BinOp (F32 Add)
  | Sub, F32 -> BinOp (F32 Sub)
  | Mul, F32 -> BinOp (F32 Mul)
  | Div None, F32 -> BinOp (F32 Div)
  | Eq, F32 -> BinOp (F32 Eq)
  | Ne, F32 -> BinOp (F32 Ne)
  | Lt None, F32 -> BinOp (F32 Lt)
  | Gt None, F32 -> BinOp (F32 Gt)
  | Le None, F32 -> BinOp (F32 Le)
  | Ge None, F32 -> BinOp (F32 Ge)
  | Add, F64 -> BinOp (F64 Add)
  | Sub, F64 -> BinOp (F64 Sub)
  | Mul, F64 -> BinOp (F64 Mul)
  | Div None, F64 -> BinOp (F64 Div)
  | Eq, F64 -> BinOp (F64 Eq)
  | Ne, F64 -> BinOp (F64 Ne)
  | Lt None, F64 -> BinOp (F64 Lt)
  | Gt None, F64 -> BinOp (F64 Gt)
  | Le None, F64 -> BinOp (F64 Le)
  | Ge None, F64 -> BinOp (F64 Ge)
  | _ ->
      print_instr i;
      assert false

let folded loc desc args =
  [ with_loc loc (Text.Folded (with_loc loc desc, args)) ]

let typeuse typ sign =
  let idx = Option.map index typ in
  let type_info =
    Option.map
      (fun (s : funsig) ->
        let params =
          List.map
            (fun (id, t) -> (Option.map (fun x -> x.desc) id, valtype t))
            s.named_params
        in
        let results = List.map valtype s.results in
        (params, results))
      sign
  in
  (idx, type_info)

let expr_type i =
  match i.info with
  | [| Some t |], _ -> t
  | _ ->
      print_instr i;
      assert false

let expr_opt_valtype i =
  match i.info with
  | [| Some t |], _ -> Some (unpack_type t)
  | [| None |], _ -> None
  | _ ->
      print_instr i;
      assert false

let expr_valtype i = unpack_type (expr_type i)
let expr_reftype i = match expr_valtype i with Ref r -> r | _ -> assert false

let expr_type_name i =
  match expr_reftype i with
  | { typ = Type idx; _ } -> idx
  | _ ->
      print_valtype (Ref (expr_reftype i));
      print_instr i;
      assert false

let expr_type_kind ctx i =
  match expr_reftype i with
  | { typ = Array; _ } -> `Array
  | _ -> Hashtbl.find ctx.type_kinds (expr_type_name i).desc

let label i l = with_loc (snd i.info) (Text.Id l)

let rec instruction ctx i : location Text.instr list =
  let _, loc = i.info in
  match i.desc with
  | Block (label, typ, body) ->
      let inner_ctx = { ctx with locals = ctx.locals } in
      let block = List.concat_map (instruction inner_ctx) body in
      folded loc (Block { label; typ = blocktype typ; block }) []
  | Loop (label, typ, body) ->
      let inner_ctx = { ctx with locals = ctx.locals } in
      let block = List.concat_map (instruction inner_ctx) body in
      folded loc (Loop { label; typ = blocktype typ; block }) []
  | If (label, typ, cond, then_, else_) ->
      let cond_code = instruction ctx cond in
      let then_ctx = { ctx with locals = ctx.locals } in
      let if_block = List.concat_map (instruction then_ctx) then_ in
      let else_block =
        match else_ with
        | Some e ->
            let else_ctx = { ctx with locals = ctx.locals } in
            List.concat_map (instruction else_ctx) e
        | None -> []
      in
      folded loc
        (If { label; typ = blocktype typ; if_block; else_block })
        cond_code
  | Try _ (*ZZZZZZZZZZZZ*) | Unreachable -> folded loc Unreachable []
  | Nop -> folded loc Nop []
  | Pop -> []
  | Null -> folded loc (RefNull (heaptype (expr_reftype i).typ)) []
  | Get idx ->
      if StringMap.mem idx.desc ctx.locals then
        let wasm_name = StringMap.find idx.desc ctx.locals in
        folded loc (Text.LocalGet (with_loc idx.info (Text.Id wasm_name))) []
      else if Hashtbl.mem ctx.functions idx.desc then
        (Hashtbl.replace ctx.referenced_functions idx.desc ();
         folded loc (Text.RefFunc (index idx)))
          []
      else folded loc (Text.GlobalGet (index idx)) []
  | Set (None, expr) -> folded loc Drop (instruction ctx expr)
  | Set (Some idx, expr) ->
      let code = instruction ctx expr in
      if StringMap.mem idx.desc ctx.locals then
        let wasm_name = StringMap.find idx.desc ctx.locals in
        folded loc (LocalSet (with_loc idx.info (Text.Id wasm_name))) code
      else folded loc (GlobalSet (index idx)) code
  | Tee (idx, expr) ->
      let code = instruction ctx expr in
      let wasm_name = StringMap.find idx.desc ctx.locals in
      folded loc (LocalTee (with_loc idx.info (Text.Id wasm_name))) code
  | Call (f, args) -> (
      let arg_code = List.concat_map (instruction ctx) args in
      match f.desc with
      (* Check for intrinsics calls *)
      | Get idx -> (
          match idx.desc with
          | "rotl" -> (
              match expr_valtype i with
              | I32 -> folded loc (BinOp (I32 Rotl)) arg_code
              | I64 -> folded loc (BinOp (I64 Rotl)) arg_code
              | _ -> assert false)
          | "rotr" -> (
              match expr_valtype i with
              | I32 -> folded loc (BinOp (I32 Rotr)) arg_code
              | I64 -> folded loc (BinOp (I64 Rotr)) arg_code
              | _ -> assert false)
          | "min" -> (
              match expr_valtype i with
              | F32 -> folded loc (BinOp (F32 Min)) arg_code
              | F64 -> folded loc (BinOp (F64 Min)) arg_code
              | _ -> assert false)
          | "max" -> (
              match expr_valtype i with
              | F32 -> folded loc (BinOp (F32 Max)) arg_code
              | F64 -> folded loc (BinOp (F64 Max)) arg_code
              | _ -> assert false)
          | "copysign" -> (
              match expr_valtype i with
              | F32 -> folded loc (BinOp (F32 CopySign)) arg_code
              | F64 -> folded loc (BinOp (F64 CopySign)) arg_code
              | _ -> assert false)
          | _ ->
              if StringMap.mem idx.desc ctx.locals then
                let code = instruction ctx f in
                folded loc (CallRef (index (expr_type_name f))) (arg_code @ code)
              else folded loc (Call (index idx)) arg_code)
      | StructGet (obj, { desc = "fill"; _ }) ->
          let array_code = instruction ctx obj in
          let type_name_idx = expr_type_name obj in
          folded loc (ArrayFill (index type_name_idx)) (array_code @ arg_code)
      | StructGet (obj, { desc = "copy"; _ }) ->
          let a1_code = instruction ctx obj in
          let type_a1 = expr_type_name obj in
          let a2_code = List.nth args 1 in
          let type_a2 = expr_type_name a2_code in
          folded loc
            (ArrayCopy (index type_a1, index type_a2))
            (a1_code @ arg_code)
      | _ ->
          let code = instruction ctx f in
          folded loc (CallRef (index (expr_type_name f))) (arg_code @ code))
  | TailCall (f, args) -> (
      (*ZZZ handle intrinsics as well? (Or reject while typing?) *)
      let arg_code = List.concat_map (instruction ctx) args in
      match f.desc with
      | Get idx when Hashtbl.mem ctx.functions idx.desc ->
          folded loc (ReturnCall (index idx)) arg_code
      | _ ->
          let code = instruction ctx f in
          folded loc (ReturnCallRef (index (expr_type_name f))) (arg_code @ code)
      )
  | Int s -> (
      match expr_valtype i with
      | I32 -> folded loc (Const (I32 s)) []
      | I64 -> folded loc (Const (I64 s)) []
      | _ -> assert false)
  | Float s -> (
      match expr_valtype i with
      | F32 -> folded loc (Const (F32 s)) []
      | F64 -> folded loc (Const (F64 s)) []
      | _ -> assert false)
  | Cast (expr, cast_ty) -> (
      let default_cast () =
        match expr.desc with
        | Null -> (
            match cast_ty with
            | Valtype (Ref r) -> folded loc (RefNull (heaptype r.typ)) []
            | _ -> assert false)
        | _ -> (
            let code = instruction ctx expr in
            match expr_opt_valtype expr with
            | None -> code
            | Some in_ty ->
                let instr : _ Text.instr_desc =
                  match (in_ty, cast_ty) with
                  (* I31 *)
                  | I32, Valtype (Ref { typ = I31; _ }) -> RefI31
                  | Ref _, Signedtype { typ = `I32; signage; _ } ->
                      I31Get signage
                  (* Extern / Any *)
                  | Ref { typ = Any; _ }, Valtype (Ref { typ = Extern; _ }) ->
                      ExternConvertAny
                  | Ref { typ = Extern; _ }, Valtype (Ref { typ = Any; _ }) ->
                      AnyConvertExtern
                  (* RefCast *)
                  | Ref _, Valtype (Ref r) -> RefCast (reftype r)
                  (* Numeric conversions *)
                  | I64, Valtype I32 -> I32WrapI64
                  | F64, Valtype F32 -> F32DemoteF64
                  | F32, Valtype F64 -> F64PromoteF32
                  | I32, Signedtype { typ = `I64; signage; _ } ->
                      I64ExtendI32 signage
                  (* Trunc *)
                  | F32, Signedtype { typ = `I32; signage = s; strict } ->
                      UnOp
                        (I32
                           (if strict then Trunc (`F32, s)
                            else TruncSat (`F32, s)))
                  | F64, Signedtype { typ = `I32; signage = s; strict } ->
                      UnOp
                        (I32
                           (if strict then Trunc (`F64, s)
                            else TruncSat (`F64, s)))
                  | F32, Signedtype { typ = `I64; signage = s; strict } ->
                      UnOp
                        (I64
                           (if strict then Trunc (`F32, s)
                            else TruncSat (`F32, s)))
                  | F64, Signedtype { typ = `I64; signage = s; strict } ->
                      UnOp
                        (I64
                           (if strict then Trunc (`F64, s)
                            else TruncSat (`F64, s)))
                  (* Convert *)
                  | I32, Signedtype { typ = `F32; signage; _ } ->
                      UnOp (F32 (Convert (`I32, signage)))
                  | I64, Signedtype { typ = `F32; signage; _ } ->
                      UnOp (F32 (Convert (`I64, signage)))
                  | I32, Signedtype { typ = `F64; signage; _ } ->
                      UnOp (F64 (Convert (`I32, signage)))
                  | I64, Signedtype { typ = `F64; signage; _ } ->
                      UnOp (F64 (Convert (`I64, signage)))
                  (* Identity *)
                  | I32, Valtype I32
                  | I64, Valtype I64
                  | F32, Valtype F32
                  | F64, Valtype F64 ->
                      Nop
                  | _ ->
                      print_valtype in_ty;
                      print_instr i;
                      assert false
                in
                folded loc instr code)
      in
      match expr.desc with
      | StructGet (instr_val, field_idx) -> (
          match (expr_type expr, cast_ty) with
          | Packed _, Signedtype { typ = `I32; signage; _ } ->
              let type_name_idx = expr_type_name instr_val in
              folded loc
                (StructGet (Some signage, index type_name_idx, index field_idx))
                (instruction ctx instr_val)
          | _ -> default_cast ())
      | ArrayGet (arr_instr, idx_instr) -> (
          match (expr_type expr, cast_ty) with
          | Packed _, Signedtype { typ = `I32; signage; _ } ->
              let type_name_idx = expr_type_name arr_instr in
              folded loc
                (ArrayGet (Some signage, index type_name_idx))
                (instruction ctx arr_instr @ instruction ctx idx_instr)
          | _ -> default_cast ())
      | _ -> default_cast ())
  | Test (expr, typ) ->
      folded loc (RefTest (reftype typ)) (instruction ctx expr)
  | NonNull expr -> folded loc RefAsNonNull (instruction ctx expr)
  | Struct (opt_idx, fields) ->
      let idx = Option.value ~default:(expr_type_name i) opt_idx in
      let field_names = Hashtbl.find ctx.struct_fields idx.desc in
      let field_map =
        List.fold_left
          (fun acc (name, instr) -> StringMap.add name.desc instr acc)
          StringMap.empty fields
      in
      let instrs =
        List.map (fun name -> StringMap.find name field_map) field_names
      in
      let args_code = List.concat_map (instruction ctx) instrs in
      folded loc (StructNew (index idx)) args_code
  | StructDefault opt_idx ->
      let idx = Option.value ~default:(expr_type_name i) opt_idx in
      folded loc (StructNewDefault (index idx)) []
  | StructGet (instr_val, field) -> (
      if field.desc = "length" then
        match expr_type_kind ctx instr_val with
        | `Array -> folded loc ArrayLen (instruction ctx instr_val)
        | _ ->
            folded loc
              (StructGet (None, index (expr_type_name instr_val), index field))
              (instruction ctx instr_val)
      else
        match (field.desc, expr_valtype instr_val) with
        (* Int Unary *)
        | "clz", I32 -> folded loc (UnOp (I32 Clz)) (instruction ctx instr_val)
        | "ctz", I32 -> folded loc (UnOp (I32 Ctz)) (instruction ctx instr_val)
        | "popcnt", I32 ->
            folded loc (UnOp (I32 Popcnt)) (instruction ctx instr_val)
        | "clz", I64 -> folded loc (UnOp (I64 Clz)) (instruction ctx instr_val)
        | "ctz", I64 -> folded loc (UnOp (I64 Ctz)) (instruction ctx instr_val)
        | "popcnt", I64 ->
            folded loc (UnOp (I64 Popcnt)) (instruction ctx instr_val)
        (* Float Unary *)
        | "abs", F32 -> folded loc (UnOp (F32 Abs)) (instruction ctx instr_val)
        | "ceil", F32 ->
            folded loc (UnOp (F32 Ceil)) (instruction ctx instr_val)
        | "floor", F32 ->
            folded loc (UnOp (F32 Floor)) (instruction ctx instr_val)
        | "trunc", F32 ->
            folded loc (UnOp (F32 Trunc)) (instruction ctx instr_val)
        | "nearest", F32 ->
            folded loc (UnOp (F32 Nearest)) (instruction ctx instr_val)
        | "sqrt", F32 ->
            folded loc (UnOp (F32 Sqrt)) (instruction ctx instr_val)
        | "abs", F64 -> folded loc (UnOp (F64 Abs)) (instruction ctx instr_val)
        | "ceil", F64 ->
            folded loc (UnOp (F64 Ceil)) (instruction ctx instr_val)
        | "floor", F64 ->
            folded loc (UnOp (F64 Floor)) (instruction ctx instr_val)
        | "trunc", F64 ->
            folded loc (UnOp (F64 Trunc)) (instruction ctx instr_val)
        | "nearest", F64 ->
            folded loc (UnOp (F64 Nearest)) (instruction ctx instr_val)
        | "sqrt", F64 ->
            folded loc (UnOp (F64 Sqrt)) (instruction ctx instr_val)
        (* Reinterpret *)
        | "to_bits", F32 ->
            folded loc (UnOp (I32 Reinterpret)) (instruction ctx instr_val)
        | "from_bits", I32 ->
            folded loc (UnOp (F32 Reinterpret)) (instruction ctx instr_val)
        | "to_bits", F64 ->
            folded loc (UnOp (I64 Reinterpret)) (instruction ctx instr_val)
        | "from_bits", I64 ->
            folded loc (UnOp (F64 Reinterpret)) (instruction ctx instr_val)
        | _ ->
            (* Signed accesses are under a cast *)
            folded loc
              (StructGet (None, index (expr_type_name instr_val), index field))
              (instruction ctx instr_val))
  | StructSet (instr_val, field_idx, new_val) ->
      let code_val = instruction ctx instr_val in
      let code_new = instruction ctx new_val in
      folded loc
        (StructSet (index (expr_type_name instr_val), index field_idx))
        (code_val @ code_new)
  | Array (opt_idx, val_instr, len_instr) ->
      let idx = Option.value ~default:(expr_type_name i) opt_idx in
      folded loc
        (ArrayNew (index idx))
        (instruction ctx val_instr @ instruction ctx len_instr)
  | ArrayDefault (opt_idx, len_instr) ->
      let idx = Option.value ~default:(expr_type_name i) opt_idx in
      folded loc (ArrayNewDefault (index idx)) (instruction ctx len_instr)
  | ArrayFixed (opt_idx, instrs) ->
      let idx = Option.value ~default:(expr_type_name i) opt_idx in
      let args_code = List.concat_map (instruction ctx) instrs in
      let len = Uint32.of_int (List.length instrs) in
      folded loc (ArrayNewFixed (index idx, len)) args_code
  | ArrayGet (arr_instr, idx_instr) ->
      (* Signed accesses are under a cast *)
      folded loc
        (ArrayGet (None, index (expr_type_name arr_instr)))
        (instruction ctx arr_instr @ instruction ctx idx_instr)
  | ArraySet (arr_instr, idx_instr, val_instr) ->
      folded loc
        (ArraySet (index (expr_type_name arr_instr)))
        (instruction ctx arr_instr @ instruction ctx idx_instr
       @ instruction ctx val_instr)
  | BinOp (op, a, b) -> (
      let code_a = instruction ctx a in
      let code_b = instruction ctx b in
      let operand_type = expr_valtype a in
      match (op, operand_type) with
      | Eq, Ref _ -> folded loc RefEq (code_a @ code_b)
      | Ne, Ref _ ->
          (* !(a == b) *)
          (*ZZZ Support in types?*)
          folded loc (Text.UnOp (I32 Eqz)) (folded loc RefEq (code_a @ code_b))
      | _ ->
          let opcode = binop i op operand_type in
          folded loc opcode (code_a @ code_b))
  | UnOp (op, a) -> (
      let operand_type = expr_valtype a in
      match (op, operand_type) with
      | Neg, I32 ->
          (* 0 - a *)
          let zero = folded loc (Const (I32 "0")) [] in
          let sub = Text.BinOp (I32 Sub) in
          folded loc sub (zero @ instruction ctx a)
      | Neg, I64 ->
          let zero = folded loc (Const (I64 "0")) [] in
          let sub = Text.BinOp (I64 Sub) in
          folded loc sub (zero @ instruction ctx a)
      | Neg, F32 -> folded loc (UnOp (F32 Neg)) (instruction ctx a)
      | Neg, F64 -> folded loc (UnOp (F64 Neg)) (instruction ctx a)
      | Not, I32 -> folded loc (UnOp (I32 Eqz)) (instruction ctx a)
      | Not, I64 -> folded loc (UnOp (I64 Eqz)) (instruction ctx a)
      (* Ref IsNull *)
      | Not, Ref _ -> folded loc RefIsNull (instruction ctx a)
      | Pos, _ -> instruction ctx a
      | _ -> assert false)
  | Let (decls, None) ->
      let binding (id, ty) =
        match id with
        | Some name ->
            let ty = Option.get ty in
            let wasm_name = Namespace.add ctx.namespace name.desc in
            ctx.locals <- StringMap.add name.desc wasm_name ctx.locals;
            ctx.allocated_locals :=
              (Some wasm_name, valtype ty) :: !(ctx.allocated_locals)
        | None -> assert false
      in
      List.iter binding (List.rev decls);
      []
  | Let (decls, Some body) ->
      let binding (id, ty) e =
        match id with
        | Some name ->
            let ty = Option.value ~default:(expr_valtype i) ty in
            let wasm_name = Namespace.add ctx.namespace name.desc in
            ctx.locals <- StringMap.add name.desc wasm_name ctx.locals;
            ctx.allocated_locals :=
              (Some wasm_name, valtype ty) :: !(ctx.allocated_locals);
            folded loc
              (Text.LocalSet (with_loc name.info (Text.Id wasm_name)))
              (instruction ctx e)
        | None -> folded loc Text.Drop (instruction ctx e)
      in
      List.concat (List.map2 binding decls [ body ])
  | Br (l, None) ->
      (*ZZZ label should be located*)
      folded loc (Br (label i l)) []
  | Br (l, Some expr) -> folded loc (Br (label i l)) (instruction ctx expr)
  | Br_if (l, expr) -> folded loc (Br_if (label i l)) (instruction ctx expr)
  | Br_table (labels, expr) -> (
      let code = instruction ctx expr in
      match List.rev labels with
      | default_label_name :: other_labels_rev ->
          let default_idx = label i default_label_name in
          let other_idx = List.rev_map (fun l -> label i l) other_labels_rev in
          folded loc (Br_table (other_idx, default_idx)) code
      | _ -> assert false)
  | Br_on_null (l, expr) ->
      folded loc (Br_on_null (label i l)) (instruction ctx expr)
  | Br_on_non_null (l, expr) ->
      folded loc (Br_on_non_null (label i l)) (instruction ctx expr)
  | Br_on_cast (l, target_reftype, expr) ->
      (*ZZZ LUB for now *)
      folded loc
        (Br_on_cast
           (label i l, reftype (expr_reftype expr), reftype target_reftype))
        (instruction ctx expr)
  | Br_on_cast_fail (l, target_reftype, expr) ->
      folded loc
        (Br_on_cast_fail
           (label i l, reftype (expr_reftype expr), reftype target_reftype))
        (instruction ctx expr)
  | Throw (tag_idx, args) ->
      folded loc
        (Throw (index tag_idx))
        (List.concat_map (instruction ctx) args)
  | ThrowRef expr -> folded loc ThrowRef (instruction ctx expr)
  | Return None -> folded loc Return []
  | Return (Some expr) -> folded loc Return (instruction ctx expr)
  | Sequence body -> List.concat_map (instruction ctx) body
  | Select (cond, then_, else_) ->
      let code_then = instruction ctx then_ in
      let code_else = instruction ctx else_ in
      let code_cond = instruction ctx cond in
      let typ =
        match expr_valtype i with
        | I32 | I64 | F32 | F64 | V128 -> None
        | typ -> Some [ valtype typ ]
      in
      folded loc (Select typ) (code_then @ code_else @ code_cond)
  | String (Some idx, _) ->
      folded loc (ArrayNewFixed (index idx, Uint32.zero)) []
  | String (None, _) -> assert false

let import attributes =
  List.find_map
    (fun (k, v) ->
      match (k, v.desc) with
      | ( "import",
          Sequence [ { desc = String (_, m); _ }; { desc = String (_, n); _ } ]
        ) ->
          Some (m, n)
      | _ -> None)
    attributes

let exports attributes =
  List.filter_map
    (fun (k, v) ->
      match (k, v.desc) with "export", String (_, n) -> Some n | _ -> None)
    attributes

let globaltype mut t : Text.globaltype = { mut; typ = valtype t }

let storagetype typ : Text.storagetype =
  match typ with Value v -> Value (valtype v) | Packed p -> Packed p

let subtype s : Text.subtype =
  let typ : Text.comptype =
    match s.typ with
    | Func typ -> Func (functype typ)
    | Struct fields ->
        Struct
          (Array.map
             (fun (name, { mut; typ }) ->
               (Some name.desc, { Text.Types.mut; typ = storagetype typ }))
             fields)
    | Array { mut; typ } -> Array { mut; typ = storagetype typ }
  in
  { typ; supertype = Option.map index s.supertype; final = s.final }

let reorder_imports lst =
  let rec traverse acc (cur : _ Ast.Text.modulefield list) =
    match cur with
    | [] -> lst (* Nothing to do *)
    | ((Import _ | Types _ | Export _ | Start _ | Elem _ | Data _) as f) :: rem
      ->
        traverse (f :: acc) rem
    | (Func _ | Memory _ | Table _ | Tag _ | Global _) :: _ ->
        let imports, others =
          List.partition
            (fun f -> match f with Ast.Text.Import _ -> true | _ -> false)
            cur
        in
        List.rev_append acc (imports @ others)
  in
  traverse [] lst

let module_ fields =
  let func_refs_in_func = Hashtbl.create 16 in
  let func_refs_outside_func = Hashtbl.create 16 in
  let ctx =
    {
      globals = Hashtbl.create 16;
      functions = Hashtbl.create 16;
      locals = StringMap.empty;
      allocated_locals = ref [];
      namespace = Namespace.make ();
      type_kinds = Hashtbl.create 16;
      struct_fields = Hashtbl.create 16;
      referenced_functions = Hashtbl.create 16;
    }
  in
  List.iter
    (fun field ->
      match field with
      | Type rectype ->
          Array.iter
            (fun (idx, subtype) ->
              let kind =
                match subtype.typ with
                | Func _ -> `Func
                | Array _ -> `Array
                | Struct fields ->
                    let field_names =
                      Array.to_list
                        (Array.map (fun (name, _) -> name.desc) fields)
                    in
                    Hashtbl.add ctx.struct_fields idx.desc field_names;
                    `Struct
              in
              Hashtbl.add ctx.type_kinds idx.desc kind)
            rectype
      | Func { name; _ } -> Hashtbl.replace ctx.functions name.desc ()
      | GlobalDecl { name; _ } -> Hashtbl.replace ctx.globals name.desc ()
      | Global { name; _ } -> Hashtbl.replace ctx.globals name.desc ()
      | Fundecl { name; _ } -> Hashtbl.replace ctx.functions name.desc ()
      | _ -> ())
    fields;
  let wasm_fields =
    List.map
      (fun field ->
        match field with
        | Type rectype ->
            Text.Types
              (Array.map (fun (idx, s) -> (Some idx.desc, subtype s)) rectype)
        | Global { name; mut; typ; def; attributes } ->
            let typ = Option.value ~default:(expr_valtype def) typ in
            let ctx =
              { ctx with referenced_functions = func_refs_outside_func }
            in
            Text.Global
              {
                id = Some name.desc;
                typ = globaltype mut typ;
                init = instruction ctx def;
                exports = exports attributes;
              }
        | GlobalDecl { name; mut; typ; attributes } ->
            let module_, import_name = Option.get (import attributes) in
            Text.Import
              {
                module_;
                name = import_name;
                id = Some name.desc;
                desc = Global (globaltype mut typ);
                exports = exports attributes;
              }
        | Fundecl { name; typ; sign; attributes } ->
            let module_, import_name = Option.get (import attributes) in
            Text.Import
              {
                module_;
                name = import_name;
                id = Some name.desc;
                desc = Func (typeuse typ sign);
                exports = exports attributes;
              }
        | Tag { name; typ; sign; attributes } -> (
            let exports = exports attributes in
            match import attributes with
            | Some (module_, import_name) ->
                Text.Import
                  {
                    module_;
                    name = import_name;
                    id = Some name.desc;
                    desc = Tag (typeuse typ sign);
                    exports;
                  }
            | None ->
                Text.Tag
                  { id = Some name.desc; typ = typeuse typ sign; exports })
        | Func { name; sign; typ; body = _, instrs; attributes } ->
            let namespace = Namespace.make () in
            let allocated_locals = ref [] in
            let locals =
              List.fold_left
                (fun locals (id, _) ->
                  match id with
                  | Some id ->
                      let wasm_name = Namespace.add namespace id.desc in
                      StringMap.add id.desc wasm_name locals
                  | None -> locals)
                StringMap.empty
                (match sign with Some sign -> sign.named_params | None -> [])
            in
            let ctx =
              {
                ctx with
                namespace;
                allocated_locals;
                locals;
                referenced_functions = func_refs_in_func;
              }
            in
            let instrs = List.concat_map (instruction ctx) instrs in
            let func_locals = List.rev !allocated_locals in
            Text.Func
              {
                id = Some name.desc;
                typ = typeuse typ sign;
                locals = func_locals;
                instrs;
                exports = exports attributes;
              })
      fields
  in
  let elem_declare : _ Text.modulefield list =
    let funcs =
      Hashtbl.fold
        (fun k _ acc ->
          if Hashtbl.mem func_refs_outside_func k then acc else k :: acc)
        func_refs_in_func []
    in
    if funcs = [] then []
    else
      let init =
        List.map
          (fun name ->
            [ Ast.no_loc (Text.RefFunc (Ast.no_loc (Text.Id name))) ])
          funcs
      in
      [
        Text.Elem
          {
            id = None;
            typ = { nullable = false; typ = Func };
            init;
            mode = Declare;
          };
      ]
  in
  let wasm_fields = wasm_fields @ elem_declare in
  let wasm_fields = reorder_imports wasm_fields in
  (None, wasm_fields)
