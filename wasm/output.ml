open Ast.Text

type sexp = Atom of string | List of sexp list | Block of sexp list

let rec format_sexp f s =
  match s with
  | Atom s -> Format.pp_print_string f s
  | List l ->
      Format.fprintf f "@[<2>(%a)@]"
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

let id x = Atom (Printf.sprintf "$%s" x)

let index x =
  Atom (match x with Num i -> Printf.sprintf "%ld" i | Id s -> id s)

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
  | Tuple l -> List (List.map valtype l)

let packedtype t = match t with I8 -> Atom "i8" | I16 -> Atom "i16"

let list ?(always = false) name f l =
  if (not always) && l = [] then [] else [ List (Atom name :: f l) ]

let valtype_list name tl =
  list name (fun tl -> List.map valtype tl) (Array.to_list tl)

let functype { params; results } =
  valtype_list "param" params @ valtype_list "result" results

let storagetype typ =
  match typ with Value typ -> valtype typ | Packed typ -> packedtype typ

let mut_type f { mut; typ } = if mut then List [ Atom "mut"; f typ ] else f typ
let fieldtype typ = mut_type (fun t -> storagetype t) typ
let globaltype typ = mut_type (fun t -> valtype t) typ
let option f x = match x with None -> [] | Some x -> f x

let comptype (typ : comptype) =
  match typ with
  | Func ty -> List (Atom "func" :: functype ty)
  | Struct l ->
      List
        (Atom "struct"
        :: List.map
             (fun (nm, f) ->
               List
                 (Atom "field"
                 :: (option (fun i -> [ id i ]) nm @ [ fieldtype f ])))
             (Array.to_list l))
  | Array ty -> List [ Atom "array"; fieldtype ty ]

let typeuse (idx, typ) =
  option (fun i -> [ List [ Atom "type"; index i ] ]) idx @ option functype typ

let blocktype =
  option @@ fun t ->
  match t with Valtype t -> [ valtype t ] | Typeuse t -> typeuse t

let quoted_name name = Atom ("\"" ^ name ^ "\"")
let export = option @@ fun name -> [ List [ Atom "export"; quoted_name name ] ]

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

let label = option @@ fun l -> [ Atom l ]

let memarg align' { offset; align } =
  (if offset = 0l then [] else [ Atom (Printf.sprintf "offset=%ld" offset) ])
  @ if align = align' then [] else [ Atom (Printf.sprintf "align=%ld" align) ]

let rec instr i =
  match i with
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
  (*
  | CallIndirect (id, typeuse) ->
      [
        Block
          (Atom "call_indirect" :: func_type st typ)
        );
      ]
*)
  | Call f -> Block [ Atom "call"; index f ]
  | Pop ty -> Block [ Atom "pop"; valtype ty ]
  | RefFunc i -> Block [ Atom "ref.func"; index i ]
  | RefIsNull -> Block [ Atom "ref.is_null" ]
  | RefAsNonNull -> Block [ Atom "ref.as_non_null" ]
  | CallRef t -> Block [ Atom "call_ref"; index t ]
  | RefI31 -> Atom "ref.i31"
  | I31Get s -> Atom (signage "i31.get" s)
  | ArrayNew t -> Block [ Atom "array.new"; index t ]
  | ArrayNewFixed (t, i) ->
      Block [ Atom "array.new_fixed"; index t; Atom (Int32.to_string i) ]
  | ArrayNewData (typ, data) ->
      Block [ Atom "array.new_data"; index typ; index data ]
  | ArrayGet (None, typ) -> Block [ Atom "array.get"; index typ ]
  | ArrayGet (Some s, typ) -> Block [ Atom (signage "array.get" s); index typ ]
  | ArrayLen -> Atom "array.len"
  | StructNew typ -> Block [ Atom "struct.new"; index typ ]
  | StructGet (None, typ, f) -> Block [ Atom "struct.get"; index typ; index f ]
  | StructGet (Some s, typ, f) ->
      Block [ Atom (signage "struct.get" s); index typ; index f ]
  | RefCast ty -> Block [ Atom "ref.cast"; reftype ty ]
  | RefTest ty -> Block [ Atom "ref.test"; reftype ty ]
  | RefEq -> Atom "ref.eq"
  | RefNull ty -> Block [ Atom "ref.null"; heaptype ty ]
  | Br_on_cast (i, ty, ty') ->
      Block [ Atom "br_on_cast"; index i; reftype ty; reftype ty' ]
  | Br_on_cast (i, ty, ty') ->
      Block [ Atom "br_on_cast_fail"; index i; reftype ty; reftype ty' ]
  | If { label = l; typ; if_block; else_block } ->
      Block
        (Atom "if"
        :: (label l @ blocktype typ
           @ (Atom "then" :: List.map instr if_block)
           @ (Atom "else" :: List.map instr else_block)
           @ [ Atom "end" ]))
  | Drop -> Atom "drop"
  | I32Store8 m -> Block (Atom "i32store8" :: memarg 1l m)
  | LocalSet i -> Block [ Atom "local.set"; index i ]
  | GlobalSet i -> Block [ Atom "global.set"; index i ]
  | Loop { label = l; typ; block } ->
      Block
        (Atom "loop"
        :: (label l @ blocktype typ @ List.map instr block @ [ Atom "end" ]))
  | Block { label = l; typ; block } ->
      Block
        (Atom "block"
        :: (label l @ blocktype typ @ List.map instr block @ [ Atom "end" ]))
  (* | Try (ty, body, catches, catch_all) ->
       [
         List
           (Atom "try"
           :: (block_type st ty
              @ List (Atom "do" :: instructions body)
                :: (List.map
                      ~f:(fun (tag, l) ->
                        List
                          (Atom "catch" :: index st.tag_names tag
                         :: instructions l))
                      catches
                   @
                   match catch_all with
                   | None -> []
                   | Some l -> [ List (Atom "catch_all" :: instructions l) ])));
       ]
  *)
  | Br_table (l, i) ->
      Block (Atom "br_table" :: List.map (fun i -> index i) (l @ [ i ]))
  | Br i -> Block [ Atom "br"; index i ]
  | Br_if i -> Block [ Atom "br_if"; index i ]
  | Return -> Atom "return"
  | Throw tag -> Block [ Atom "throw"; index tag ]
  | Nop -> Atom "nop"
  | Unreachable -> Atom "unreachable"
  | ArraySet typ -> Block [ Atom "array.set"; index typ ]
  | StructSet (typ, i) -> Block [ Atom "struct.set"; index typ; index i ]
  (*
  | ReturnCallIndirect (typ, e, l) ->
      [
        List
          ((Atom "return_call_indirect" :: func_type st typ)
          @ List.concat (List.map ~f:expression (l @ [ e ])));
      ]
*)
  | ReturnCall f -> Block [ Atom "return_call"; index f ]
  | ReturnCallRef typ -> Block [ Atom "return_call_ref"; index typ ]
  | Folded (i, l) -> List (instr i :: List.map instr l)

let funct ctx st name exported_name typ param_names locals body =
  List
    ((Atom "func" :: index st.func_names name :: export exported_name)
    @ func_type st ~param_names typ
    @ List.map
        ~f:(fun (i, t) ->
          List [ Atom "local"; index st.local_names i; value_type st t ])
        locals
    @ instructions ctx st body)

let import st f =
  match f with
  | Function _ | Global _ | Data _ | Tag _ | Type _ -> []
  | Import { import_module; import_name; name; desc } ->
      [
        List
          [
            Atom "import";
            quoted_name import_module;
            quoted_name import_name;
            List
              (match desc with
              | Fun typ ->
                  Atom "func" :: index st.func_names name :: func_type st typ
              | Global ty ->
                  [ Atom "global"; symbol st (V name); global_type st ty ]
              | Tag ty ->
                  [
                    Atom "tag";
                    index st.tag_names name;
                    List [ Atom "param"; value_type st ty ];
                  ]);
          ];
      ]

let escape_string s =
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if Poly.(c >= ' ' && c <= '~' && c <> '"' && c <> '\\') then
      Buffer.add_char b c
    else Printf.bprintf b "\\%02x" (Char.code c)
  done;
  Buffer.contents b

let data_contents ctx contents =
  let b = Buffer.create 16 in
  List.iter
    ~f:(fun d ->
      match d with
      | DataI8 c -> Buffer.add_uint8 b c
      | DataI32 i -> Buffer.add_int32_le b i
      | DataI64 i -> Buffer.add_int64_le b i
      | DataBytes s -> Buffer.add_string b s
      | DataSym (symb, ofs) ->
          Buffer.add_int32_le b (Int32.of_int (lookup_symbol ctx symb + ofs))
      | DataSpace n -> Buffer.add_string b (String.make n '\000'))
    contents;
  escape_string (Buffer.contents b)

let type_field st { name; typ; supertype; final } =
  if final && Option.is_none supertype then
    List [ Atom "type"; index st.type_names name; str_type st typ ]
  else
    List
      [
        Atom "type";
        index st.type_names name;
        List
          (Atom "sub"
          :: ((if final then [ Atom "final" ] else [])
             @ (match supertype with
               | Some supertype -> [ index st.type_names supertype ]
               | None -> [])
             @ [ str_type st typ ]));
      ]

let field ctx st f =
  match f with
  | Function { name; exported_name; typ; param_names; locals; body } ->
      [ funct ctx st name exported_name typ param_names locals body ]
  | Global { name; exported_name; typ; init } ->
      [
        List
          (Atom "global" :: symbol st name
          :: (export exported_name
             @ (global_type st typ :: expression ctx st init)));
      ]
  | Tag { name; typ } ->
      [
        List
          [
            Atom "tag";
            index st.tag_names name;
            List [ Atom "param"; value_type st typ ];
          ];
      ]
  | Import _ -> []
  | Data { name; active; contents; _ } ->
      [
        List
          (Atom "data" :: index st.data_names name
          :: ((if active then
                 expression ctx st
                   (Const (I32 (Int32.of_int (lookup_symbol ctx (V name)))))
               else [])
             @ [ Atom ("\"" ^ data_contents ctx contents ^ "\"") ]));
      ]
  | Type [ t ] -> [ type_field st t ]
  | Type l -> [ List (Atom "rec" :: List.map ~f:(type_field st) l) ]

let data_size contents =
  List.fold_left
    ~f:(fun sz d ->
      sz
      +
      match d with
      | DataI8 _ -> 1
      | DataI32 _ -> 4
      | DataI64 _ -> 8
      | DataBytes s -> String.length s
      | DataSym _ -> 4
      | DataSpace n -> n)
    ~init:0 contents

let data_offsets fields =
  List.fold_left
    ~f:(fun (i, addresses) f ->
      match f with
      | Data { name; contents; active = true; _ } ->
          (i + data_size contents, Code.Var.Map.add name i addresses)
      | Function _ | Global _ | Tag _ | Import _
      | Data { active = false; _ }
      | Type _ ->
          (i, addresses))
    ~init:(0, Code.Var.Map.empty) fields

let f ~debug ch fields =
  let st = build_name_tables fields in
  let heap_base, addresses = data_offsets fields in
  let ctx =
    {
      addresses;
      functions = Code.Var.Map.empty;
      function_refs = Code.Var.Set.empty;
      function_count = 0;
      debug;
    }
  in
  let other_fields =
    List.concat (List.map ~f:(fun f -> field ctx st f) fields)
  in
  let funct_table =
    let functions =
      List.map ~f:fst
        (List.sort
           ~cmp:(fun (_, i) (_, j) -> compare i j)
           (Code.Var.Map.bindings ctx.functions))
    in
    if List.is_empty functions then []
    else
      [
        List
          [
            Atom "table";
            Atom "funcref";
            List (Atom "elem" :: List.map ~f:(index st.func_names) functions);
          ];
      ]
  in
  let funct_decl =
    let functions =
      Code.Var.Set.elements
        (Code.Var.Set.filter
           (fun f -> not (Code.Var.Map.mem f ctx.functions))
           ctx.function_refs)
    in
    if List.is_empty functions then []
    else
      [
        List
          (Atom "elem" :: Atom "declare" :: Atom "func"
          :: List.map ~f:(index st.func_names) functions);
      ]
  in
  Format.fprintf
    (Format.formatter_of_out_channel ch)
    "%a@." format_sexp
    (List
       (Atom "module"
       :: (List.concat (List.map ~f:(fun i -> import st i) fields)
          @ (if Code.Var.Map.is_empty addresses then []
             else
               [
                 List
                   [
                     Atom "memory";
                     Atom (string_of_int ((heap_base + 0xffff) / 0x10000));
                   ];
               ])
          @ funct_table @ funct_decl @ other_fields)))
