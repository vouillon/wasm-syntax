open Ast.Text

type sexp =
  | Atom of string
  | Atom' of { len : int; s : string }
  | List of sexp list
  | Block of sexp list
  | VerticalBlock of sexp list
  | StructuredBlock of structure list

and structure = Delimiter of sexp | Contents of sexp list

let rec format_sexp f s =
  match s with
  | Atom s -> Format.pp_print_string f s
  | Atom' { len; s } -> Format.pp_print_as f len s
  | List l ->
      Format.fprintf f "(@[<1>%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f "@ ")
           format_sexp)
        l
  | Block l ->
      Format.fprintf f "@[%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f "@ ")
           format_sexp)
        l
  | VerticalBlock l ->
      Format.fprintf f "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f "@ ")
           format_sexp)
        l
  | StructuredBlock l ->
      Format.fprintf f "@[<hv>";
      List.iteri
        (fun i s ->
          match s with
          | Delimiter d ->
              if i > 0 then Format.fprintf f "@ ";
              Format.fprintf f "%a" format_sexp d
          | Contents l ->
              if l <> [] then
                Format.fprintf f "@;<1 2>@[<hv>%a@]"
                  (Format.pp_print_list
                     ~pp_sep:(fun f () -> Format.fprintf f "@ ")
                     format_sexp)
                  l)
        l;
      Format.fprintf f "@]"

let option f x = match x with None -> [] | Some x -> f x
let id x = Atom (Printf.sprintf "$%s" x)
let opt_id = option (fun i -> [ id i ])

let index x =
  match x with Num i -> Atom (Printf.sprintf "%ld" i) | Id s -> id s

let heaptype (ty : heaptype) =
  match ty with
  | Func -> Atom "func"
  | NoFunc -> Atom "nofunc"
  | Extern -> Atom "extern"
  | NoExtern -> Atom "noextern"
  | Any -> Atom "any"
  | Eq -> Atom "eq"
  | I31 -> Atom "i31"
  | Struct -> Atom "struct"
  | Array -> Atom "array"
  | None_ -> Atom "none"
  | Type t -> index t

let reftype { nullable; typ } =
  match (nullable, typ) with
  | true, Func -> Atom "funcref"
  | true, NoFunc -> Atom "nullfuncref"
  | true, Extern -> Atom "externref"
  | true, NoExtern -> Atom "nullexternref"
  | true, Any -> Atom "anyref"
  | true, Eq -> Atom "eqref"
  | true, I31 -> Atom "i31ref"
  | true, Struct -> Atom "structref"
  | true, Array -> Atom "arrayref"
  | true, None_ -> Atom "nullref"
  | _ ->
      let r = [ heaptype typ ] in
      List (Atom "ref" :: (if nullable then Atom "null" :: r else r))

let rec valtype (t : valtype) =
  match t with
  | I32 -> Atom "i32"
  | I64 -> Atom "i64"
  | F32 -> Atom "f32"
  | F64 -> Atom "f64"
  | V128 -> Atom "v128"
  | Ref ty -> reftype ty
  | Tuple l -> List (Atom "tuple" :: List.map valtype l)

let packedtype t = match t with I8 -> Atom "i8" | I16 -> Atom "i16"

let list ?(always = false) name f l =
  if (not always) && l = [] then [] else [ List (Atom name :: f l) ]

let valtype_list name tl = list name (fun tl -> List.map valtype tl) tl

let functype { params; results } =
  valtype_list "param" (Array.to_list params)
  @ valtype_list "result" (Array.to_list results)

let storagetype typ =
  match typ with Value typ -> valtype typ | Packed typ -> packedtype typ

let mut_type f { mut; typ } = if mut then List [ Atom "mut"; f typ ] else f typ
let fieldtype typ = mut_type (fun t -> storagetype t) typ
let globaltype typ = mut_type (fun t -> valtype t) typ

let comptype (typ : comptype) =
  match typ with
  | Func ty -> List (Atom "func" :: functype ty)
  | Struct l ->
      List
        (Atom "struct"
        :: List.map
             (fun (nm, f) ->
               List (Atom "field" :: (opt_id nm @ [ fieldtype f ])))
             (Array.to_list l))
  | Array ty -> List [ Atom "array"; fieldtype ty ]

let typeuse idx = [ List [ Atom "type"; index idx ] ]
let typeuse' (idx, typ) = option typeuse idx @ option functype typ

let blocktype =
  option @@ fun t ->
  match t with Valtype t -> [ valtype t ] | Typeuse t -> typeuse' t

let utf8_length s =
  let segmenter = Uuseg.create `Grapheme_cluster in
  let has_grapheme = ref false in
  let flush_segment acc =
    let acc = if !has_grapheme then acc + 1 else acc in
    has_grapheme := false;
    acc
  in
  let rec add acc v =
    match Uuseg.add segmenter v with
    | `Uchar _ ->
        has_grapheme := true;
        add acc `Await
    | `Boundary -> add (flush_segment acc) `Await
    | `Await | `End -> acc
  in
  let rec loop acc i len =
    if i >= len then flush_segment (add acc `End)
    else
      let dec = String.get_utf_8_uchar s i in
      let acc = add acc (`Uchar (Uchar.utf_decode_uchar dec)) in
      loop acc (i + Uchar.utf_decode_length dec) len
  in
  loop 0 0 (String.length s)

let escape_string s =
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= ' ' && c <> '\x7f' && c <> '"' && c <> '\\' then Buffer.add_char b c
    else
      match c with
      | '\t' -> Buffer.add_string b "\\t"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | _ -> Printf.bprintf b "\\%02x" (Char.code c)
  done;
  let s = Buffer.contents b in
  (utf8_length s, s)

let quoted_string s =
  let i, s = escape_string s in
  Atom' { len = i + 2; s = "\"" ^ s ^ "\"" }

let exports l =
  List.map (fun name -> List [ Atom "export"; quoted_string name ]) l

let type_prefix op nm =
  (match op with
  | I32 _ -> "i32."
  | I64 _ -> "i64."
  | F32 _ -> "f32."
  | F64 _ -> "f64.")
  ^ nm

let signage op (s : signage) =
  op ^ match s with Signed -> "_s" | Unsigned -> "_u"

let size sz =
  match sz with `F32 -> "f32" | `F64 -> "f64" | `I32 -> "i32" | `I64 -> "i64"

let int_un_op width op =
  match op with
  | Clz -> "clz"
  | Ctz -> "ctz"
  | Popcnt -> "popcnt"
  | Eqz -> "eqz"
  | Trunc (sz, s) -> signage "trunc_" s ^ size sz
  | TruncSat (sz, s) -> signage "trunc_sat_" s ^ size sz
  | Reinterpret -> "reinterpret_f" ^ width
  | ExtendS sz -> (
      match sz with
      | `_8 -> "extend8_s"
      | `_16 -> "extend16_s"
      | `_32 -> "extend32_s")

let int_bin_op _ (op : int_bin_op) =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div s -> signage "div" s
  | Rem s -> signage "rem" s
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Shl -> "shl"
  | Shr s -> signage "shr" s
  | Rotl -> "rotl"
  | Rotr -> "rotr"
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt s -> signage "lt" s
  | Gt s -> signage "gt" s
  | Le s -> signage "le" s
  | Ge s -> signage "ge" s

let float_un_op sz op =
  match op with
  | Neg -> "neg"
  | Abs -> "abs"
  | Ceil -> "ceil"
  | Floor -> "floor"
  | Trunc -> "trunc"
  | Nearest -> "nearest"
  | Sqrt -> "sqrt"
  | Convert (`I32, s) -> signage "convert_i32" s
  | Convert (`I64, s) -> signage "convert_i64" s
  | Reinterpret -> "reinterpret_i" ^ sz

let float_bin_op _ op =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Min -> "min"
  | Max -> "max"
  | CopySign -> "copysign"
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "lt"
  | Gt -> "gt"
  | Le -> "le"
  | Ge -> "ge"

let select i32 i64 f32 f64 op =
  match op with
  | I32 x -> i32 "32" x
  | I64 x -> i64 "64" x
  | F32 x -> f32 "32" x
  | F64 x -> f64 "64" x

let memarg align' { offset; align } =
  (if offset = 0l then [] else [ Atom (Printf.sprintf "offset=%ld" offset) ])
  @ if align = align' then [] else [ Atom (Printf.sprintf "align=%ld" align) ]

let integer i = Atom (Int32.to_string i)

let rec instr i =
  match i with
  | ExternConvertAny -> Atom "extern.convert_any"
  | AnyConvertExtern -> Atom "any.convert_extern"
  | Const op ->
      Block
        [
          Atom (type_prefix op "const");
          Atom
            (select
               (fun _ i -> i)
               (fun _ i -> i)
               (fun _ i -> i)
               (fun _ i -> i)
               op);
        ]
  | UnOp op ->
      Atom
        (type_prefix op (select int_un_op int_un_op float_un_op float_un_op op))
  | BinOp op ->
      Atom
        (type_prefix op
           (select int_bin_op int_bin_op float_bin_op float_bin_op op))
  | I32WrapI64 -> Atom "i32.wrap_i64"
  | I64ExtendI32 s -> Atom (signage "i64.extend_i32" s)
  | F32DemoteF64 -> Atom "f32.demote_f64"
  | F64PromoteF32 -> Atom "f64.promote_f32"
  | I32Load8 (s, m) -> Block (Atom (signage "i32load8" s) :: memarg 1l m)
  | LocalGet i -> Block [ Atom "local.get"; index i ]
  | LocalTee i -> Block [ Atom "local.tee"; index i ]
  | GlobalGet i -> Block [ Atom "global.get"; index i ]
  | CallIndirect (id, typ) ->
      Block
        (Atom "call_indirect"
        :: ((if id = Num 0l then [] else [ index id ]) @ typeuse' typ))
  | ReturnCallIndirect (id, typ) ->
      Block
        (Atom "return_call_indirect"
        :: ((if id = Num 0l then [] else [ index id ]) @ typeuse' typ))
  | Call f -> Block [ Atom "call"; index f ]
  | Select t -> Block (Atom "select" :: option (fun t -> [ valtype t ]) t)
  | Pop ty -> Block [ Atom "pop"; valtype ty ]
  | RefFunc i -> Block [ Atom "ref.func"; index i ]
  | RefIsNull -> Block [ Atom "ref.is_null" ]
  | RefAsNonNull -> Block [ Atom "ref.as_non_null" ]
  | CallRef t -> Block [ Atom "call_ref"; index t ]
  | RefI31 -> Atom "ref.i31"
  | I31Get s -> Atom (signage "i31.get" s)
  | ArrayNew t -> Block [ Atom "array.new"; index t ]
  | ArrayNewDefault t -> Block [ Atom "array.new_default"; index t ]
  | ArrayNewFixed (t, i) -> Block [ Atom "array.new_fixed"; index t; integer i ]
  | ArrayNewElem (i, i') -> Block [ Atom "array.new_fixed"; index i; index i' ]
  | ArrayNewData (typ, data) ->
      Block [ Atom "array.new_data"; index typ; index data ]
  | ArrayInitData (i, i') -> Block [ Atom "array.init_data"; index i; index i' ]
  | ArrayInitElem (i, i') -> Block [ Atom "array.init_elem"; index i; index i' ]
  | ArrayGet (None, typ) -> Block [ Atom "array.get"; index typ ]
  | ArrayGet (Some s, typ) -> Block [ Atom (signage "array.get" s); index typ ]
  | ArrayLen -> Atom "array.len"
  | ArrayCopy (i, i') -> Block [ Atom "array.copy"; index i; index i' ]
  | ArrayFill i -> Block [ Atom "array.fill"; index i ]
  | StructNew typ -> Block [ Atom "struct.new"; index typ ]
  | StructNewDefault typ -> Block [ Atom "struct.new_default"; index typ ]
  | StructGet (None, typ, f) -> Block [ Atom "struct.get"; index typ; index f ]
  | StructGet (Some s, typ, f) ->
      Block [ Atom (signage "struct.get" s); index typ; index f ]
  | RefCast ty -> Block [ Atom "ref.cast"; reftype ty ]
  | RefTest ty -> Block [ Atom "ref.test"; reftype ty ]
  | RefEq -> Atom "ref.eq"
  | RefNull ty -> Block [ Atom "ref.null"; heaptype ty ]
  | If { label; typ; if_block; else_block } ->
      StructuredBlock
        (Delimiter (Block (Atom "if" :: (opt_id label @ blocktype typ)))
        :: Contents (List.map instr if_block)
        ::
        (if else_block = [] then [ Delimiter (Atom "end") ]
         else
           [
             Delimiter (Atom "else");
             Contents (List.map instr else_block);
             Delimiter (Atom "end");
           ]))
  | Drop -> Atom "drop"
  | I32Store8 m -> Block (Atom "i32store8" :: memarg 1l m)
  | LocalSet i -> Block [ Atom "local.set"; index i ]
  | GlobalSet i -> Block [ Atom "global.set"; index i ]
  | Block { label; typ; block } ->
      StructuredBlock
        [
          Delimiter (Block (Atom "block" :: (opt_id label @ blocktype typ)));
          Contents (List.map instr block);
          Delimiter (Atom "end");
          (*ZZZ Comment?*)
        ]
  | Loop { label; typ; block } ->
      StructuredBlock
        [
          Delimiter (Block (Atom "loop" :: (opt_id label @ blocktype typ)));
          Contents (List.map instr block);
          Delimiter (Atom "end");
          (*ZZZ Comment?*)
        ]
  | Try _ -> Atom "unreachable" (*ZZZ*)
  | Br_table (l, i) ->
      Block (Atom "br_table" :: List.map (fun i -> index i) (l @ [ i ]))
  | Br i -> Block [ Atom "br"; index i ]
  | Br_if i -> Block [ Atom "br_if"; index i ]
  | Br_on_null i -> Block [ Atom "br_on_null"; index i ]
  | Br_on_non_null i -> Block [ Atom "br_on_non_null"; index i ]
  | Br_on_cast (i, ty, ty') ->
      Block [ Atom "br_on_cast"; index i; reftype ty; reftype ty' ]
  | Br_on_cast_fail (i, ty, ty') ->
      Block [ Atom "br_on_cast_fail"; index i; reftype ty; reftype ty' ]
  | Return -> Atom "return"
  | Throw tag -> Block [ Atom "throw"; index tag ]
  | Nop -> Atom "nop"
  | Unreachable -> Atom "unreachable"
  | ArraySet typ -> Block [ Atom "array.set"; index typ ]
  | StructSet (typ, i) -> Block [ Atom "struct.set"; index typ; index i ]
  | ReturnCall f -> Block [ Atom "return_call"; index f ]
  | ReturnCallRef typ -> Block [ Atom "return_call_ref"; index typ ]
  | TupleMake i -> Block [ Atom "tuple.make"; integer i ]
  | TupleExtract (i, j) -> Block [ Atom "tuple.make"; integer i; integer j ]
  | Folded (If { label; typ; if_block; else_block }, l) ->
      List
        (Atom "if"
        :: (opt_id label @ blocktype typ @ List.map instr l
           @ list ~always:true "then" (List.map instr) if_block
           @ list "else" (List.map instr) else_block))
  | Folded (Block { label; typ; block }, l) ->
      assert (l = []);
      List
        (Atom "block" :: (opt_id label @ blocktype typ @ List.map instr block))
      (*ZZZ Comment?*)
  | Folded (Loop { label; typ; block }, l) ->
      assert (l = []);
      List (Atom "loop" :: (opt_id label @ blocktype typ @ List.map instr block))
      (*ZZZ Comment?*)
  | Folded (i, l) -> List (instr i :: List.map instr l)

let instrs l = VerticalBlock (List.map instr l)

let subtype (id, { typ; supertype; final }) =
  if final && Option.is_none supertype then
    List (Atom "type" :: (opt_id id @ [ comptype typ ]))
  else
    List
      (Atom "type"
      :: (opt_id id
         @ [
             List
               (Atom "sub"
               :: ((if final then [ Atom "final" ] else [])
                  @ option (fun i -> [ index i ]) supertype
                  @ [ comptype typ ]));
           ]))

let fundecl (idx, typ) =
  option typeuse idx
  @ option
      (fun (params, results) ->
        (if List.for_all (fun (i, _) -> i = None) params then
           list "param" (fun tl -> List.map valtype tl) (List.map snd params)
         else
           List.map
             (fun (i, t) -> List (Atom "param" :: (opt_id i @ [ valtype t ])))
             params)
        @ valtype_list "result" results)
      typ

let modulefield f =
  match f with
  | Types [| t |] -> subtype t
  | Types l -> List (Atom "rec" :: List.map subtype (Array.to_list l))
  | Func { id; typ; locals; instrs = i; exports = e } ->
      List
        (Atom "func"
        :: (opt_id id @ exports e @ fundecl typ
           @ List.map
               (fun (i, t) -> List (Atom "local" :: (opt_id i @ [ valtype t ])))
               locals
           @ [ instrs i ]))
  | Import { module_; name; id; desc; exports = e } ->
      List
        (Atom "import" :: quoted_string module_ :: quoted_string name
        :: (exports e
           @ [
               List
                 (match desc with
                 | Func typ -> Atom "func" :: (opt_id id @ fundecl typ)
                 | Global ty -> Atom "global" :: (opt_id id @ [ globaltype ty ])
                 | Tag typ -> Atom "tag" :: (opt_id id @ fundecl typ)
                 | Memory _ -> [ Atom "memory" ] (*ZZZ*));
             ]))
  | Global { id; typ; init; exports = e } ->
      List
        (Atom "global"
        :: (opt_id id @ exports e @ [ globaltype typ; instrs init ]))
  | Tag { id; typ; exports = e } ->
      List (Atom "tag" :: (opt_id id @ exports e @ fundecl typ))
  | Data { id; init; mode } ->
      List
        (Atom "data"
        :: (opt_id id
           @ (match mode with
             | Passive -> []
             | Active (i, e) -> (
                 (if i = Num 0l then [] else [ List [ Atom "memory"; index i ] ])
                 @
                 match e with
                 | [ i ] -> [ instr i ]
                 | _ -> [ List [ Atom "offset"; instrs e ] ]))
           @ [ quoted_string init ]))
  | Start idx -> List [ Atom "start"; index idx ]
  | _ -> List [ Atom "other" ]
(*ZZZ*)

let module_ f (id, fields) =
  Format.fprintf f "%a@." format_sexp
    (List [ Atom "module"; Block (opt_id id @ List.map modulefield fields) ])
