open Ast

let heaptype f (t : heaptype) =
  match t with
  | Func -> Format.fprintf f "func"
  | NoFunc -> Format.fprintf f "nofunc"
  | Extern -> Format.fprintf f "extern"
  | NoExtern -> Format.fprintf f "noextern"
  | Any -> Format.fprintf f "any"
  | Eq -> Format.fprintf f "eq"
  | I31 -> Format.fprintf f "i31"
  | Struct -> Format.fprintf f "struct"
  | Array -> Format.fprintf f "array"
  | None_ -> Format.fprintf f "none"
  | Type s -> Format.fprintf f "%s" s

let reftype f { nullable; typ } =
  if nullable then Format.fprintf f "&?%a" heaptype typ
  else Format.fprintf f "&%a" heaptype typ

let rec valtype f t =
  match t with
  | I32 -> Format.fprintf f "i32"
  | I64 -> Format.fprintf f "i64"
  | F32 -> Format.fprintf f "f32"
  | F64 -> Format.fprintf f "f64"
  | V128 -> Format.fprintf f "v128"
  | Ref t -> reftype f t
  | Tuple l -> tuple f l

and tuple f l =
  match l with
  | [ t ] -> valtype f t
  | _ ->
      Format.fprintf f "@[<1>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           valtype)
        l

let functype f { params; results } =
  if results = [||] then
    Format.fprintf f "@[<2>fn@ %a@]" tuple (Array.to_list params)
  else
    Format.fprintf f "@[<2>fn@ %a@ ->@ %a@]" tuple (Array.to_list params) tuple
      (Array.to_list results)

let packedtype f t =
  match t with I8 -> Format.fprintf f "i8" | I16 -> Format.fprintf f "i16"

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
  | Array t -> Format.fprintf f "@]@ @[<1>[%a]@]" fieldtype t

let subtype f (nm, { typ; supertype; final }) =
  Format.fprintf f "@[<hv>@[type@ %s%s" nm (if final then "" else " open");
  (match supertype with
  | Some supertype -> Format.fprintf f "@ :@ %s" supertype
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

let binop f op =
  Format.fprintf f "%s"
    (match op with
    | Add -> "+"
    | Sub -> "-"
    | Gt Signed -> ">s"
    | Gt Unsigned -> ">u"
    | Lt Signed -> "<s"
    | Lt Unsigned -> "<s"
    | Or -> "|"
    | And -> "&"
    | Ne -> "!="
    | Eq -> "==")

(* precedence, labels *)
let rec instr f i =
  match i.descr with
  | Block (label, l) -> block f label "" l
  | Loop (label, l) -> block f label "loop " l
  | If (_label, i, l1, l2) -> (
      Format.fprintf f "@[@[<hv>@[if@ %a@ {@]%a" instr i block_contents l1;
      match l2 with
      | Some l2 ->
          Format.fprintf f "@[<hv>@[}@ else@ {@]%a}@]@]@]" block_contents l2
      | None -> Format.fprintf f "}@]@]")
  | Unreachable -> Format.fprintf f "unreachable"
  | Nop -> Format.fprintf f "nop"
  | Get x -> Format.fprintf f "%s" x
  | Set (x, i) -> Format.fprintf f "@[<2>%s@ :=@ %a@]" x instr i
  | Tee (x, i) -> Format.fprintf f "@[<2>%s@ =@ %a@]" x instr i
  | Call (i, l) ->
      Format.fprintf f "@[<2>%a@[(%a)@]@]" instr i
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           instr)
        l
  | Struct (nm, l) ->
      Format.fprintf f "@[<hv>@[";
      Option.iter (fun nm -> Format.fprintf f "%s@ " nm) nm;
      Format.fprintf f "{@]@;<1 2>%a@ }@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@;<1 2>")
           (fun f (nm, i) -> Format.fprintf f "@[<2>%s:@ %a@]" nm instr i))
        l
  | String s -> Format.fprintf f "\"%s\"" s (* Escape *)
  | Int s -> Format.fprintf f "%s" s
  | Cast (i, t) -> Format.fprintf f "@[<2>%a@ as@ %a@]" instr i reftype t
  | Test (i, t) -> Format.fprintf f "@[<2>%a@ is@ %a@]" instr i reftype t
  | StructGet (i, s) -> Format.fprintf f "%a.%s" instr i s
  | StructSet (i, s, i') ->
      Format.fprintf f "@[<2>%a.%s@ =@ %a@]" instr i s instr i'
  | BinOp (op, i, i') ->
      Format.fprintf f "@[<2>%a@ %a@ %a@]" instr i binop op instr i'
  | Local (x, t, i) ->
      Format.fprintf f "@[let@ %s" x;
      Option.iter (fun t -> Format.fprintf f "@ :@ %a" valtype t) t;
      Option.iter (fun i -> Format.fprintf f "@ =@ %a" instr i) i;
      Format.fprintf f "@]"
  | Br (label, i) ->
      Format.fprintf f "@[br@ %s" label;
      Option.iter (fun i -> Format.fprintf f "@ %a" instr i) i;
      Format.fprintf f "@]"
  | Br_if (label, i) ->
      Format.fprintf f "@[br_if@ %s" label;
      Format.fprintf f "@ =@ %a" instr i;
      Format.fprintf f "@]"
  | Return i ->
      Format.fprintf f "@[return";
      Option.iter (fun i -> Format.fprintf f "@ %a" instr i) i;
      Format.fprintf f "@]"
  | Sequence l ->
      Format.fprintf f "@[<1>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           instr)
        l
  | Null -> Format.fprintf f "null"
  | _ -> Format.fprintf f "/* instr */"

(*
  | Br_table of string list * instr
  | Br_on_null of string * instr
  | Br_on_non_null of string * instr
  | Br_on_cast of string * reftype * instr
  | Br_on_cast_fail of string * reftype * instr
*)
and block f _label kind l =
  Format.fprintf f "@[<hv>@[%s{@]%a}@]" kind block_contents l

and deliminated_instr f i =
  match i.descr with
  | Block _ | Loop _ | If _ -> Format.fprintf f "%a" instr i
  | Unreachable | Nop | Get _ | Set _ | Tee _ | Call _ | Struct _ | String _
  | Int _ | Cast _ | Test _ | StructGet _ | StructSet _ | BinOp _ | Local _
  | Br _ | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _ | Br_on_cast _
  | Br_on_cast_fail _ | Return _ | Sequence _ | Null ->
      Format.fprintf f "@[%a;@]" instr i

and block_instrs f l =
  match l with
  | [] -> ()
  | [ i ] -> instr f i
  | i :: rem ->
      Format.fprintf f "%a@;<1 2>%a" deliminated_instr i block_instrs rem

and block_contents f l =
  match l with [] -> () | _ -> Format.fprintf f "@;<1 2>%a@ " block_instrs l

let fundecl f (name, typ, sign) =
  Format.fprintf f "fn@ %s" name;
  Option.iter (fun typ -> Format.fprintf f "@ : %s@ " typ) typ;
  Option.iter
    (fun { named_params; results } ->
      Format.fprintf f "@[<1>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           (fun f (id, t) ->
             match id with
             | None -> valtype f t
             | Some id -> Format.fprintf f "@[<2>%s:@ %a@]" id valtype t))
        named_params;
      if results <> [] then Format.fprintf f "@ ->@ %a" tuple results)
    sign

let attributes f attributes =
  List.iter
    (fun (name, i) -> Format.fprintf f "@[<2>#[%s@ =@ %a]@]@ " name instr i)
    attributes

let modulefield f field =
  match field with
  | Type t -> rectype f t
  | Func { name; typ; sign; body = _lab, body; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[<hv>@[%a@ {@]%a}@]@]" attributes a fundecl
        (name, typ, sign) block_contents body
  | Global { name; typ; def; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[<2>let@ %s" attributes a name;
      Option.iter (fun t -> Format.fprintf f "@ :@ %a" globaltype t) typ;
      Format.fprintf f "@ =@ %a@]@]" instr def
  | Fundecl { name; typ; sign; attributes = a } ->
      Format.fprintf f "@[<hv>%a@[%a@]]" attributes a fundecl (name, typ, sign)

let module_ f l =
  Format.fprintf f "@[<hv>%a@]@."
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@ ")
       modulefield)
    l
