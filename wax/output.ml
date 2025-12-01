open Utils.Printer
open Ast

let list ?(sep = space) f pp l =
  match l with
  | [] -> ()
  | [ x ] -> f pp x
  | x :: xs ->
      f pp x;
      List.iter
        (fun x ->
          sep pp ();
          f pp x)
        xs

let list_commasep f pp l =
  list
    ~sep:(fun pp () ->
      string pp ",";
      space pp ())
    f pp l

let print_paren_list f pp l =
  string pp "(";
  box pp (fun () -> list_commasep f pp l);
  string pp ")"

let heaptype pp (t : heaptype) =
  string pp
    (match t with
    | Func -> "func"
    | NoFunc -> "nofunc"
    | Exn -> "exn"
    | NoExn -> "noexn"
    | Extern -> "extern"
    | NoExtern -> "noextern"
    | Any -> "any"
    | Eq -> "eq"
    | I31 -> "i31"
    | Struct -> "struct"
    | Array -> "array"
    | None_ -> "none"
    | Type s -> s.desc)

let reftype pp { nullable; typ } =
  string pp (if nullable then "&?" else "&");
  heaptype pp typ

let rec valtype pp t =
  match t with
  | I32 -> string pp "i32"
  | I64 -> string pp "i64"
  | F32 -> string pp "f32"
  | F64 -> string pp "f64"
  | V128 -> string pp "v128"
  | Ref t -> reftype pp t
  | Tuple l -> tuple false pp l

and tuple always_paren pp l =
  match l with
  | [ t ] when not always_paren -> valtype pp t
  | _ -> print_paren_list valtype pp l

let functype pp { params; results } =
  box pp ~indent:2 (fun () ->
      string pp "fn";
      tuple true pp (Array.to_list params);
      if results <> [||] then (
        space pp ();
        string pp "->";
        space pp ();
        tuple false pp (Array.to_list results)))

let blocktype pp { params; results } =
  match (params, results) with
  | [||], [| ty |] -> valtype pp ty
  | _ ->
      box pp ~indent:2 (fun () ->
          tuple true pp (Array.to_list params);
          if results <> [||] then (
            space pp ();
            string pp "->";
            space pp ();
            tuple false pp (Array.to_list results)))

let packedtype pp t = string pp (match t with I8 -> "i8" | I16 -> "i16")

let storagetype pp t =
  match t with Value t -> valtype pp t | Packed t -> packedtype pp t

let muttype t pp { mut; typ } =
  if mut then
    box pp ~indent:2 (fun () ->
        string pp "mut";
        space pp ();
        t pp typ)
  else t pp typ

let fieldtype = muttype storagetype

let comptype pp (t : comptype) =
  match t with
  | Func t -> functype pp t
  | Struct l ->
      (* The opening brace is printed in [subtype] *)
      indent pp 2 (fun () ->
          space pp ();
          list_commasep
            (fun pp (nm, t) ->
              box pp ~indent:2 (fun () ->
                  string pp nm.desc;
                  string pp ":";
                  space pp ();
                  fieldtype pp t))
            pp (Array.to_list l));
      space pp ();
      string pp "}"
  | Array t ->
      string pp "[";
      box pp (fun () -> fieldtype pp t);
      string pp "]"

let subtype pp (nm, { typ; supertype; final }) =
  hvbox pp (fun () ->
      let is_struct = match typ with Struct _ -> true | _ -> false in
      box pp (fun () ->
          string pp "type";
          space pp ();
          string pp nm.desc;
          (match supertype with
          | Some supertype ->
              string pp ":";
              space pp ();
              string pp supertype.desc
          | None -> ());
          space pp ();
          string pp "=";
          if not final then (
            space pp ();
            string pp "open");
          if is_struct then (
            space pp ();
            string pp "{"));
      space pp ();
      comptype pp typ)

let rectype pp t =
  match Array.to_list t with
  | [ t ] -> subtype pp t
  | l ->
      hvbox pp (fun () ->
          string pp "rec {";
          indent pp 2 (fun () ->
              space pp ();
              list ~sep:space subtype pp l);
          space pp ();
          string pp "}")

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

let parentheses expected actual pp g =
  if expected > actual then (
    string pp "(";
    box pp (fun () ->
        g ();
        string pp ")"))
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
  let rec loop (l : _ instr list) n =
    if n <= 0 then 0
    else
      match l with
      | [] -> n
      | { desc = Block (_, _, l); _ } :: rem -> loop rem (loop l (n - 2))
      | { desc = Loop (_, _, l); _ } :: rem -> loop rem (loop l (n - 2))
      | { desc = If (_, _, _, l1, l2); _ } :: rem ->
          let n = loop l1 (n - 2) in
          let n = match l2 with None -> n | Some l2 -> loop l2 (n - 1) in
          loop rem n
      | _ :: rem -> loop rem (n - 1)
  in
  loop l 5 <= 0

let block_label pp label =
  Option.iter
    (fun label ->
      string pp "'";
      string pp label;
      string pp ":";
      space pp ())
    label

let label_comment pp (l, label) =
  if long_block l then
    Option.iter
      (fun label ->
        space pp ();
        string pp "/* '";
        string pp label;
        string pp " */")
      label

let simple_pat pp p = string pp (match p with Some x -> x.desc | None -> "_")
let need_blocktype bt = bt.params <> [||] || bt.results <> [||]

let casttype pp ty =
  match ty with
  | Valtype ty -> valtype pp ty
  | Signedtype { typ; signage; strict } ->
      string pp (Ast.format_signed_type typ signage strict)

let print_optional_type_prefix pp opt =
  Option.iter
    (fun t ->
      string pp t.desc;
      string pp "|")
    opt

let branch_instr instr prec pp name label i =
  parentheses prec Branch pp @@ fun () ->
  box pp ~indent:2 (fun () ->
      string pp name;
      space pp ();
      string pp "'";
      string pp label;
      Option.iter
        (fun i ->
          space pp ();
          instr Branch pp i)
        i)

let branch_ref_instr instr prec pp name label ty i =
  parentheses prec Branch pp @@ fun () ->
  box pp ~indent:2 (fun () ->
      string pp name;
      space pp ();
      string pp "'";
      string pp label;
      space pp ();
      reftype pp ty;
      space pp ();
      instr Branch pp i)

let call_instr instr prec pp ?prefix i l =
  parentheses prec Call pp @@ fun () ->
  box pp ~indent:2 (fun () ->
      Option.iter
        (fun s ->
          string pp s;
          space pp ())
        prefix;
      instr Call pp i;
      cut pp ();
      print_paren_list (instr Instruction) pp l)

let struct_instr pp nm f =
  hvbox pp (fun () ->
      box pp (fun () ->
          string pp "{";
          print_optional_type_prefix pp nm);
      f ();
      space pp ();
      string pp "}")

let array_instr pp nm f =
  hvbox pp ~indent:2 (fun () ->
      box pp (fun () ->
          string pp "[";
          print_optional_type_prefix pp nm);
      space pp ();
      f ();
      string pp "]")

let rec instr prec pp (i : _ instr) =
  match i.desc with
  | Block (label, bt, l) ->
      parentheses prec Block pp @@ fun () ->
      block pp label (if need_blocktype bt then Some "do" else None) bt l
  | Loop (label, bt, l) ->
      parentheses prec Block pp @@ fun () -> block pp label (Some "loop") bt l
  | If (label, bt, i, l1, l2) ->
      parentheses prec Block pp @@ fun () ->
      hvbox pp (fun () ->
          box pp (fun () ->
              block_label pp label;
              string pp "if";
              indent pp 2 (fun () ->
                  string pp " ";
                  instr Instruction pp i;
                  if need_blocktype bt then (
                    space pp ();
                    box pp ~indent:2 (fun () ->
                        string pp "=>";
                        space pp ();
                        blocktype pp bt)));
              space pp ();
              string pp "{");
          block_contents pp l1;
          match l2 with
          | Some l2 ->
              hvbox pp (fun () ->
                  box pp (fun () ->
                      string pp "}";
                      space pp ();
                      string pp "else";
                      space pp ();
                      string pp "{");
                  block_contents pp l2;
                  string pp "}")
          | None -> string pp "}")
  | Unreachable -> string pp "unreachable"
  | Nop -> string pp "_"
  | Pop -> string pp "_"
  | Get x -> string pp x.desc
  | Set (x, i) ->
      parentheses prec Assignement pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          simple_pat pp x;
          space pp ();
          string pp "=";
          space pp ();
          instr Assignement pp i)
  | Tee (x, i) ->
      parentheses prec Assignement pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          string pp x.desc;
          space pp ();
          string pp ":=";
          space pp ();
          instr Assignement pp i)
  | Call (i, l) -> call_instr instr prec pp i l
  | TailCall (i, l) -> call_instr instr prec pp ~prefix:"become" i l
  | String (t, s) ->
      Option.iter
        (fun t ->
          string pp t.desc;
          string pp "#")
        t;
      let len, s = Wasm.Output.escape_string s in
      string pp "\"";
      string_as pp len s;
      string pp "\""
  | Int s | Float s -> string pp s
  | Cast (i, t) ->
      parentheses prec Cast pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          instr Cast pp i;
          space pp ();
          box pp (fun () ->
              string pp "as";
              space pp ();
              casttype pp t))
  | NonNull i ->
      parentheses prec UnaryOp pp @@ fun () ->
      instr UnaryOp pp i;
      string pp "!"
  | Test (i, t) ->
      parentheses prec Cast pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          instr Cast pp i;
          space pp ();
          box pp (fun () ->
              string pp "is";
              space pp ();
              reftype pp t))
  | Struct (nm, l) ->
      struct_instr pp nm (fun () ->
          indent pp 2 (fun () ->
              space pp ();
              list_commasep
                (fun pp (nm, i) ->
                  box pp ~indent:2 (fun () ->
                      string pp nm.desc;
                      string pp ":";
                      space pp ();
                      instr Instruction pp i))
                pp l))
  | StructDefault nm ->
      struct_instr pp nm (fun () ->
          indent pp 2 (fun () ->
              space pp ();
              string pp ".."))
  | StructGet (i, s) ->
      parentheses prec FieldAccess pp @@ fun () ->
      instr FieldAccess pp i;
      string pp ".";
      string pp s.desc
  | StructSet (i, s, i') ->
      parentheses prec FieldAccess pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          instr FieldAccess pp i;
          string pp ".";
          string pp s.desc;
          space pp ();
          string pp "=";
          space pp ();
          instr Assignement pp i')
  | Array (nm, i, n) ->
      array_instr pp nm (fun () ->
          instr Instruction pp i;
          string pp ";";
          space pp ();
          instr Instruction pp n)
  | ArrayDefault (nm, n) ->
      array_instr pp nm (fun () ->
          string pp "..;";
          space pp ();
          instr Instruction pp n)
  | ArrayFixed (nm, l) ->
      array_instr pp nm (fun () -> list_commasep (instr Instruction) pp l)
  | ArrayGet (i1, i2) ->
      box pp ~indent:2 (fun () ->
          instr Call pp i1;
          cut pp ();
          box pp (fun () ->
              string pp "[";
              instr Instruction pp i2;
              string pp "]"))
  | ArraySet (i1, i2, i3) ->
      box pp ~indent:2 (fun () ->
          instr Call pp i1;
          cut pp ();
          box pp (fun () ->
              string pp "[";
              instr Instruction pp i2;
              string pp "]");
          space pp ();
          string pp "=";
          space pp ();
          instr Assignement pp i3)
  | BinOp (op, i, i') ->
      let out, left, right = prec_op op in
      parentheses prec out pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          instr left pp i;
          space pp ();
          string pp (binop op);
          space pp ();
          instr right pp i')
  | UnOp (op, i) ->
      parentheses prec UnaryOp pp @@ fun () ->
      string pp (unop op);
      instr UnaryOp pp i
  | Let (l, i) ->
      parentheses prec Let pp @@ fun () ->
      let single pp (x, t) =
        box pp ~indent:2 (fun () ->
            simple_pat pp x;
            Option.iter
              (fun t ->
                string pp ":";
                space pp ();
                valtype pp t)
              t)
      in
      box pp ~indent:2 (fun () ->
          string pp "let";
          space pp ();
          (match l with
          | [ p ] -> single pp p
          | l -> print_paren_list single pp l);
          Option.iter
            (fun i ->
              space pp ();
              string pp "=";
              space pp ();
              instr Assignement pp i)
            i)
  | Br (label, i) -> branch_instr instr prec pp "br" label i
  | Br_if (label, i) -> branch_instr instr prec pp "br_if" label (Some i)
  | Br_on_null (label, i) ->
      branch_instr instr prec pp "br_on_null" label (Some i)
  | Br_on_non_null (label, i) ->
      branch_instr instr prec pp "br_on_non_null" label (Some i)
  | Br_on_cast (label, ty, i) ->
      branch_ref_instr instr prec pp "br_on_cast" label ty i
  | Br_on_cast_fail (label, ty, i) ->
      branch_ref_instr instr prec pp "br_on_cast_fail" label ty i
  | Br_table (labels, i) ->
      let labels = List.rev labels in
      parentheses prec Branch pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          string pp "br_table";
          space pp ();
          box pp ~indent:2 (fun () ->
              string pp "[";
              list
                ~sep:(fun _ () -> ())
                (fun pp label ->
                  space pp ();
                  string pp "'";
                  string pp label)
                pp
                (List.rev (List.tl labels));
              space pp ();
              box pp (fun () ->
                  string pp "else";
                  space pp ();
                  string pp "'";
                  string pp (List.hd labels));
              space pp ();
              string pp "]");
          space pp ();
          instr Branch pp i)
  | Return i ->
      parentheses prec Branch pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          string pp "return";
          Option.iter
            (fun i ->
              space pp ();
              instr Branch pp i)
            i)
  | Throw (tag, l) ->
      parentheses prec Branch pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          string pp "throw";
          space pp ();
          string pp tag.desc;
          space pp ();
          print_paren_list (instr Instruction) pp l)
  | ThrowRef i ->
      parentheses prec Branch pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          string pp "throw";
          space pp ();
          instr Branch pp i)
  | Sequence l -> print_paren_list (instr Instruction) pp l
  | Select (i1, i2, i3) ->
      parentheses prec Select pp @@ fun () ->
      box pp ~indent:2 (fun () ->
          instr Select pp i1;
          cut pp ();
          string pp "?";
          instr Select pp i2;
          cut pp ();
          string pp ":";
          instr Select pp i3)
  | Null -> string pp "null"

and block pp label kind bt (l : _ instr list) =
  hvbox pp (fun () ->
      box pp (fun () ->
          block_label pp label;
          Option.iter
            (fun kind ->
              string pp kind;
              space pp ())
            kind;
          if need_blocktype bt then (
            blocktype pp bt;
            space pp ());
          string pp "{");
      block_contents pp l;
      box pp (fun () ->
          string pp "}";
          label_comment pp (l, label)))

and deliminated_instr pp (i : _ instr) =
  match i.desc with
  | Block _ | Loop _ | If _ -> instr Instruction pp i
  | Unreachable | Nop | Pop | Get _ | Set _ | Tee _ | Call _ | TailCall _
  | String _ | Int _ | Float _ | Cast _ | NonNull _ | Test _ | Struct _
  | StructDefault _ | StructGet _ | StructSet _ | Array _ | ArrayDefault _
  | ArrayFixed _ | ArrayGet _ | ArraySet _ | BinOp _ | UnOp _ | Let _ | Br _
  | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _ | Br_on_cast _
  | Br_on_cast_fail _ | Return _ | Throw _ | ThrowRef _ | Sequence _ | Null
  | Select _ ->
      box pp (fun () ->
          instr Instruction pp i;
          string pp ";")

and block_contents pp (l : _ instr list) =
  if l <> [] then (
    indent pp 2 (fun () ->
        List.iter
          (fun i ->
            space pp ();
            deliminated_instr pp i)
          l);
    space pp ())

let fundecl ~tag pp (name, typ, sign) =
  string pp (if tag then "tag" else "fn");
  space pp ();
  string pp name.desc;
  Option.iter
    (fun typ ->
      string pp ":";
      space pp ();
      string pp typ.desc;
      space pp ())
    typ;
  Option.iter
    (fun { named_params; results } ->
      cut pp ();
      print_paren_list
        (fun pp (id, t) ->
          box pp ~indent:2 (fun () ->
              simple_pat pp id;
              string pp ":";
              space pp ();
              valtype pp t))
        pp named_params;
      if results <> [] then (
        space pp ();
        string pp "->";
        space pp ();
        tuple false pp results))
    sign

let print_attribute pp (name, i) =
  box pp ~indent:2 (fun () ->
      string pp "#[";
      string pp name;
      space pp ();
      string pp "=";
      space pp ();
      instr Instruction pp i;
      string pp "]";
      space pp ())

let print_attributes pp attributes = List.iter (print_attribute pp) attributes

let print_attr_prefix pp attributes_list content_fn =
  hvbox pp (fun () ->
      print_attributes pp attributes_list;
      content_fn ())

let modulefield pp field =
  match field with
  | Type t -> rectype pp t
  | Func { name; typ; sign; body = label, body; attributes = a } ->
      print_attr_prefix pp a (fun () ->
          hvbox pp (fun () ->
              box pp (fun () ->
                  fundecl ~tag:false pp (name, typ, sign);
                  space pp ();
                  block_label pp label;
                  string pp "{");
              block_contents pp body;
              string pp "}"))
  | Global { name; mut; typ; def; attributes = a } ->
      print_attr_prefix pp a (fun () ->
          box pp ~indent:2 (fun () ->
              string pp (if mut then "let" else "const");
              space pp ();
              string pp name.desc;
              Option.iter
                (fun t ->
                  string pp ":";
                  space pp ();
                  valtype pp t)
                typ;
              space pp ();
              string pp "=";
              space pp ();
              instr Instruction pp def;
              string pp ";"))
  | Fundecl { name; typ; sign; attributes = a } ->
      print_attr_prefix pp a (fun () ->
          box pp (fun () -> fundecl ~tag:false pp (name, typ, sign)))
  | Tag { name; typ; sign; attributes = a } ->
      print_attr_prefix pp a (fun () ->
          box pp (fun () -> fundecl ~tag:true pp (name, typ, sign)))
  | GlobalDecl { name; mut; typ; attributes = a } ->
      print_attr_prefix pp a (fun () ->
          box pp ~indent:2 (fun () ->
              string pp (if mut then "let" else "const");
              space pp ();
              string pp name.desc;
              string pp ":";
              space pp ();
              valtype pp typ))

let module_ pp l = hvbox pp (fun () -> list ~sep:space modulefield pp l)
let instr i = instr Instruction i
