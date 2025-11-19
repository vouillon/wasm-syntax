open Ast

let heaptype f (t : heaptype) =
  match t with
  | Func -> Format.pp_print_string f "func"
  | NoFunc -> Format.pp_print_string f "nofunc"
  | Extern -> Format.pp_print_string f "extern"
  | NoExtern -> Format.pp_print_string f "noextern"
  | Any -> Format.pp_print_string f "any"
  | Eq -> Format.pp_print_string f "eq"
  | I31 -> Format.pp_print_string f "i31"
  | Struct -> Format.pp_print_string f "struct"
  | Array -> Format.pp_print_string f "array"
  | None_ -> Format.pp_print_string f "none"
  | Type s -> Format.pp_print_string f s

let reftype f { nullable; typ } =
  if nullable then Format.fprintf f "&?%a" heaptype typ
  else Format.fprintf f "&%a" heaptype typ

let rec valtype f t =
  match t with
  | I32 -> Format.pp_print_string f "i32"
  | I64 -> Format.pp_print_string f "i64"
  | F32 -> Format.pp_print_string f "f32"
  | F64 -> Format.pp_print_string f "f64"
  | V128 -> Format.pp_print_string f "v128"
  | Ref t -> reftype f t
  | Tuple l -> tuple false f l

and tuple always_paren f l =
  match l with
  | [ t ] when not always_paren -> valtype f t
  | _ ->
      Format.fprintf f "(@[%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           valtype)
        l

let functype f { params; results } =
  if results = [||] then
    Format.fprintf f "@[<2>fn%a@]" (tuple true) (Array.to_list params)
  else
    Format.fprintf f "@[<2>fn%a@ ->@ %a@]" (tuple true) (Array.to_list params)
      (tuple false) (Array.to_list results)

let packedtype f t =
  match t with
  | I8 -> Format.pp_print_string f "i8"
  | I16 -> Format.pp_print_string f "i16"

let storagetype f t =
  match t with Value t -> valtype f t | Packed t -> packedtype f t

let muttype t f { mut; typ } =
  if mut then Format.fprintf f "@[<2>mut@ %a@]" t typ else t f typ

let fieldtype = muttype storagetype

let comptype f (t : comptype) =
  match t with
  | Func t -> Format.fprintf f "@]@ %a" functype t
  | Struct l ->
      Format.fprintf f "@ {@]@;<1 2>%a@ }"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@;<1 2>")
           (fun f (nm, t) -> Format.fprintf f "@[<2>%s:@ %a@]" nm fieldtype t))
        (Array.to_list l)
  | Array t -> Format.fprintf f "@]@ [@[%a@]]" fieldtype t

let subtype f (nm, { typ; supertype; final }) =
  Format.fprintf f "@[<hv>@[type@ %s%s" nm (if final then "" else " open");
  (match supertype with
  | Some supertype -> Format.fprintf f ":@ %s" supertype
  | None -> ());
  Format.fprintf f "@ =%a@]" comptype typ

let rectype f t =
  match Array.to_list t with
  | [ t ] -> subtype f t
  | l ->
      Format.fprintf f "@[<2>rec {%a}@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f "@ ")
           subtype)
        l

let globaltype = muttype valtype

let binop op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div None -> "/"
  | Div (Some Signed) -> "/s"
  | Div (Some Unsigned) -> "/u"
  | Rem Signed -> "%s"
  | Rem Unsigned -> "%u"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"
  | Shl -> "<<"
  | Shr Signed -> ">>s"
  | Shr Unsigned -> ">>u"
  | Eq -> "=="
  | Ne -> "!="
  | Lt None -> "<"
  | Lt (Some Signed) -> "<s"
  | Lt (Some Unsigned) -> "<u"
  | Gt None -> ">"
  | Gt (Some Signed) -> ">s"
  | Gt (Some Unsigned) -> ">u"
  | Le None -> "<="
  | Le (Some Signed) -> "<=s"
  | Le (Some Unsigned) -> "<=u"
  | Ge None -> ">="
  | Ge (Some Signed) -> ">=s"
  | Ge (Some Unsigned) -> ">=u"

let unop op = match op with Neg -> "-" | Pos -> "+" | Not -> "!"

type prec =
  | Instruction
  | Let
  | Assignement
  | Select
  | Comparison
  | LogicalOr
  | LogicalXor
  | LogicalAnd
  | Shift
  | Addition
  | Multiplication
  | Branch
  | Cast
  | UnaryOp
  | Call
  | FieldAccess
  | Block

let parentheses expected actual f g =
  if expected > actual then (
    Format.fprintf f "(@[";
    g ();
    Format.fprintf f "@])")
  else g ()

let prec_op op =
  (* out, left, right *)
  match op with
  | Add | Sub -> (Addition, Addition, Multiplication)
  | Mul | Div _ | Rem _ -> (Multiplication, Multiplication, Branch)
  | And -> (LogicalAnd, LogicalAnd, Shift)
  | Or -> (LogicalOr, LogicalOr, LogicalXor)
  | Xor -> (LogicalXor, LogicalXor, LogicalAnd)
  | Shl | Shr _ -> (Shift, Shift, Addition)
  | Gt _ | Lt _ | Ge _ | Le _ | Eq | Ne -> (Comparison, LogicalOr, LogicalOr)

let long_block l =
  let rec loop l n =
    if n <= 0 then 0
    else
      match l with
      | [] -> n
      | { descr = Ast.Block (_, l); _ } :: rem -> loop rem (loop l (n - 2))
      | { descr = Ast.Loop (_, l); _ } :: rem -> loop rem (loop l (n - 2))
      | { descr = Ast.If (_, _, l1, l2); _ } :: rem ->
          let n = loop l1 (n - 2) in
          let n = match l2 with None -> n | Some l2 -> loop l2 (n - 1) in
          loop rem n
      | _ :: rem -> loop rem (n - 1)
  in
  loop l 5 <= 0

let block_label f label =
  Option.iter (fun label -> Format.fprintf f "'%s:@ " label) label

let label_comment f (l, label) =
  if long_block l then
    Option.iter (fun label -> Format.fprintf f "@ /* '%s */" label) label

let simple_pat f p =
  match p with
  | Some x -> Format.pp_print_string f x
  | None -> Format.pp_print_string f "_"

let rec instr prec f (i : instr) =
  match i.descr with
  | Block (label, l) ->
      parentheses prec Block f @@ fun () -> block f label None l
  | Loop (label, l) ->
      parentheses prec Block f @@ fun () -> block f label (Some "loop") l
  | If (label, i, l1, l2) -> (
      parentheses prec Block f @@ fun () ->
      Format.fprintf f "@[@[<hv>@[%aif@ %a@ {@]%a" block_label label
        (instr Instruction) i block_contents l1;
      match l2 with
      | Some l2 ->
          Format.fprintf f "@[<hv>@[}@ else@ {@]%a@[}%a@]@]@]@]" block_contents
            l2 label_comment
            (l1 @ l2, label)
      | None -> Format.fprintf f "@[}%a@]@]@]" label_comment (l1, label))
  | Unreachable -> Format.pp_print_string f "unreachable"
  | Nop -> Format.pp_print_string f "nop"
  | Get x -> Format.pp_print_string f x
  | Set (x, i) ->
      parentheses prec Assignement f @@ fun () ->
      Format.fprintf f "@[<2>%s@ =@ %a@]" x (instr Assignement) i
  | Tee (x, i) ->
      parentheses prec Assignement f @@ fun () ->
      Format.fprintf f "@[<2>%s@ :=@ %a@]" x (instr Assignement) i
  | Call (i, l) ->
      parentheses prec Call f @@ fun () ->
      Format.fprintf f "@[<2>%a@,(@[%a@])@]" (instr Call) i
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           (instr Instruction))
        l
  | String (t, s) ->
      Format.fprintf f "@[";
      Option.iter (fun t -> Format.fprintf f "%s#" t) t;
      Format.fprintf f "\"%a\"@]"
        (fun f s ->
          let len, s = Wasm.Output.escape_string s in
          Format.pp_print_as f len s)
        s
  | Int s | Float s -> Format.pp_print_string f s
  | Cast (i, t) ->
      parentheses prec Cast f @@ fun () ->
      Format.fprintf f "@[<2>%a@ @[as@ %a@]@]" (instr Cast) i valtype t
  | Test (i, t) ->
      parentheses prec Cast f @@ fun () ->
      Format.fprintf f "@[<2>%a@ @[is@ %a@]@]" (instr Cast) i reftype t
  | Struct (nm, l) ->
      Format.fprintf f "@[<hv>@[{";
      Option.iter (fun nm -> Format.fprintf f "%s|" nm) nm;
      Format.fprintf f "@]@;<1 2>%a@ }@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@;<1 2>")
           (fun f (nm, i) ->
             Format.fprintf f "@[<2>%s:@ %a@]" nm (instr Instruction) i))
        l
  | StructGet (i, s) ->
      parentheses prec FieldAccess f @@ fun () ->
      Format.fprintf f "%a.%s" (instr FieldAccess) i s
  | StructSet (i, s, i') ->
      parentheses prec FieldAccess f @@ fun () ->
      Format.fprintf f "@[<2>%a.%s@ =@ %a@]" (instr FieldAccess) i s
        (instr Assignement) i'
  | Array (t, i, n) ->
      Format.fprintf f "[@[";
      Option.iter (fun t -> Format.fprintf f "%s|@ " t) t;
      Format.fprintf f "%a;@ %a@]]" (instr Instruction) i (instr Instruction) n
  | ArrayFixed (t, l) ->
      Format.fprintf f "[@[";
      Option.iter (fun t -> Format.fprintf f "%s|@ " t) t;
      Format.fprintf f "%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           (instr Instruction))
        l
  | ArrayGet (i1, i2) ->
      Format.fprintf f "@[<2>%a@,@[[%a]@]@]" (instr Call) i1 (instr Instruction)
        i2
  | ArraySet (i1, i2, i3) ->
      Format.fprintf f "@[<2>%a@,@[[%a]@]@ =@ %a@]" (instr Call) i1
        (instr Instruction) i2 (instr Assignement) i3
  | BinOp (op, i, i') ->
      let out, left, right = prec_op op in
      parentheses prec out f @@ fun () ->
      Format.fprintf f "@[<2>%a@ %s@ %a@]" (instr left) i (binop op)
        (instr right) i'
  | UnOp (op, i) ->
      parentheses prec UnaryOp f @@ fun () ->
      Format.fprintf f "@[%s%a@]" (unop op) (instr UnaryOp) i
  | Let (l, i) ->
      parentheses prec Let f @@ fun () ->
      let single f (x, t) =
        Format.fprintf f "@[<2>%a" simple_pat x;
        Option.iter (fun t -> Format.fprintf f ":@ %a" valtype t) t;
        Format.fprintf f "@]"
      in
      Format.fprintf f "@[<2>let@ %a"
        (fun f l ->
          match l with
          | [ p ] -> single f p
          | l ->
              Format.fprintf f "(@[%a@])"
                (Format.pp_print_list
                   ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
                   single)
                l)
        l;
      Option.iter (fun i -> Format.fprintf f "@ =@ %a" (instr Assignement) i) i;
      Format.fprintf f "@]"
  | Br (label, i) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>br@ '%s" label;
      Option.iter (fun i -> Format.fprintf f "@ %a" (instr Branch) i) i;
      Format.fprintf f "@]"
  | Br_if (label, i) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>br_if@ '%s@ %a@]" label (instr Branch) i
  | Br_on_null (label, i) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>br_on_null@ '%s@ %a@]" label (instr Branch) i
  | Br_on_non_null (label, i) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>br_on_non_null@ '%s@ %a@]" label (instr Branch) i
  | Br_on_cast (label, ty, i) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>br_on_cast@ '%s@ %a@ %a@]" label reftype ty
        (instr Branch) i
  | Br_on_cast_fail (label, ty, i) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>br_on_cast_fail@ '%s@ %a@ %a@]" label reftype ty
        (instr Branch) i
  | Br_table (labels, i) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>br_table@ @[<2>{@ %a@ }@]@ %a@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f "@ ")
           (fun f label -> Format.fprintf f "'%s" label))
        labels (instr Branch) i
  | Return i ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>return";
      Option.iter (fun i -> Format.fprintf f "@ %a" (instr Branch) i) i;
      Format.fprintf f "@]"
  | Throw (tag, l) ->
      parentheses prec Branch f @@ fun () ->
      Format.fprintf f "@[<2>throw@ %s@,(@[%a@])@]@]" tag
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           (instr Instruction))
        l
  | Sequence l ->
      Format.fprintf f "(@[%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           (instr Instruction))
        l
  | Select (i1, i2, i3) ->
      parentheses prec Select f @@ fun () ->
      Format.fprintf f "@[@<2>%a@,?%a@,:%a@]" (instr Select) i1 (instr Select)
        i2 (instr Select) i3
  | Null -> Format.pp_print_string f "null"

and block f label kind l =
  Format.fprintf f "@[<hv>@[%a%a{@]%a@[}%a@]@]" block_label label
    (fun f kind -> Option.iter (fun kind -> Format.fprintf f "%s@ " kind) kind)
    kind block_contents l label_comment (l, label)

and deliminated_instr f (i : instr) =
  match i.descr with
  | Block _ | Loop _ | If _ -> instr Instruction f i
  | Unreachable | Nop | Get _ | Set _ | Tee _ | Call _ | String _ | Int _
  | Float _ | Cast _ | Test _ | Struct _ | StructGet _ | StructSet _ | Array _
  | ArrayFixed _ | ArrayGet _ | ArraySet _ | BinOp _ | UnOp _ | Let _ | Br _
  | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _ | Br_on_cast _
  | Br_on_cast_fail _ | Return _ | Throw _ | Sequence _ | Null | Select _ ->
      Format.fprintf f "@[%a;@]" (instr Instruction) i

and block_instrs f l =
  match l with
  | [] -> ()
  | [ i ] -> instr Instruction f i
  | i :: rem ->
      Format.fprintf f "%a@;<1 2>%a" deliminated_instr i block_instrs rem

and block_contents f l =
  match l with [] -> () | _ -> Format.fprintf f "@;<1 2>%a@ " block_instrs l

let fundecl ~tag f (name, typ, sign) =
  Format.fprintf f "%s@ %s" (if tag then "tag" else "fn") name;
  Option.iter (fun typ -> Format.fprintf f ": %s@ " typ) typ;
  Option.iter
    (fun { named_params; results } ->
      Format.fprintf f "@,(@[%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           (fun f (id, t) ->
             Format.fprintf f "@[<2>%a:@ %a@]" simple_pat id valtype t))
        named_params;
      if results <> [] then Format.fprintf f "@ ->@ %a" (tuple false) results)
    sign

let attributes f attributes =
  List.iter
    (fun (name, i) ->
      Format.fprintf f "@[<2>#[%s@ =@ %a]@]@ " name (instr Instruction) i)
    attributes

let modulefield f field =
  match field with
  | Type t -> rectype f t
  | Func { name; typ; sign; body = _lab, body; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[<hv>@[%a@ {@]%a}@]@]" attributes a
        (fundecl ~tag:false) (name, typ, sign) block_contents body
  | Global { name; typ; def; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[<2>let@ %s" attributes a name;
      Option.iter (fun t -> Format.fprintf f ":@ %a" globaltype t) typ;
      Format.fprintf f "@ =@ %a@]@]" (instr Instruction) def
  | Fundecl { name; typ; sign; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[%a@]@]" attributes a (fundecl ~tag:false)
        (name, typ, sign)
  | Tag { name; typ; sign; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[%a@]@]" attributes a (fundecl ~tag:true)
        (name, typ, sign)
  | GlobalDecl { name; typ; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[<2>let@ %s" attributes a name;
      Option.iter (fun t -> Format.fprintf f ":@ %a" globaltype t) typ;
      Format.fprintf f "@]@]"

let module_ f l =
  Format.fprintf f "@[<hv>%a@]@."
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@ ")
       modulefield)
    l
