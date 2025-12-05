open Ast.Binary

let header = "\000asm\001\000\000\000"

let check_header file contents =
  if
    String.length contents < 8
    || not (String.equal header (String.sub contents 0 8))
  then failwith (file ^ " is not a Wasm binary file (bad magic)")

type ch = { buf : string; mutable pos : int; limit : int }

let pos_in ch = ch.pos
let seek_in ch pos = ch.pos <- pos

let input_byte ch =
  let pos = ch.pos in
  ch.pos <- pos + 1;
  Char.code ch.buf.[pos]

let peek_byte ch = Char.code ch.buf.[ch.pos]

let really_input_string ch len =
  let pos = ch.pos in
  ch.pos <- pos + len;
  String.sub ch.buf pos len

let rec uint ?(n = 5) ch =
  let i = input_byte ch in
  if n = 1 then assert (i < 16);
  if i < 128 then i else i - 128 + (uint ~n:(n - 1) ch lsl 7)

let rec uint64_rec ?(n = 10) ch =
  let i = input_byte ch in
  if n = 1 then assert (i < 2);
  if i < 128 then Int64.of_int i
  else
    Int64.add
      (Int64.sub (Int64.of_int i) 128L)
      (Int64.shift_left (uint64_rec ~n:(n - 1) ch) 7)

let uint64 ch = Utils.Uint64.of_int64 (uint64_rec ch)

let rec sint ?(n = 5) ch =
  let i = input_byte ch in
  if n = 1 then assert (i < 8 || (i > 120 && i < 128));
  if i < 64 then i
  else if i < 128 then i - 128
  else i - 128 + (sint ~n:(n - 1) ch lsl 7)

let rec sint32 ?(n = 5) ch =
  let i = Int32.of_int (input_byte ch) in
  if n = 1 then assert (Int32.compare i 128l < 0);
  if Int32.compare i 64l < 0 then i
  else if Int32.compare i 128l < 0 then Int32.sub i 128l
  else Int32.add (Int32.sub i 128l) (Int32.shift_left (sint32 ~n:(n - 1) ch) 7)

let rec sint64 ?(n = 10) ch =
  let i = Int64.of_int (input_byte ch) in
  if n = 1 then assert (Int64.compare i 128L < 0);
  if Int64.compare i 64L < 0 then i
  else if Int64.compare i 128L < 0 then Int64.sub i 128L
  else Int64.add (Int64.sub i 128L) (Int64.shift_left (sint64 ~n:(n - 1) ch) 7)

let float32 ch =
  let b1 = input_byte ch in
  let b2 = input_byte ch in
  let b3 = input_byte ch in
  let b4 = input_byte ch in
  let i = Int32.of_int b1 in
  let i = Int32.logor i (Int32.shift_left (Int32.of_int b2) 8) in
  let i = Int32.logor i (Int32.shift_left (Int32.of_int b3) 16) in
  let i = Int32.logor i (Int32.shift_left (Int32.of_int b4) 24) in
  Int32.float_of_bits i

let float64 ch =
  let b1 = Int64.of_int (input_byte ch) in
  let b2 = Int64.of_int (input_byte ch) in
  let b3 = Int64.of_int (input_byte ch) in
  let b4 = Int64.of_int (input_byte ch) in
  let b5 = Int64.of_int (input_byte ch) in
  let b6 = Int64.of_int (input_byte ch) in
  let b7 = Int64.of_int (input_byte ch) in
  let b8 = Int64.of_int (input_byte ch) in
  let i = b1 in
  let i = Int64.logor i (Int64.shift_left b2 8) in
  let i = Int64.logor i (Int64.shift_left b3 16) in
  let i = Int64.logor i (Int64.shift_left b4 24) in
  let i = Int64.logor i (Int64.shift_left b5 32) in
  let i = Int64.logor i (Int64.shift_left b6 40) in
  let i = Int64.logor i (Int64.shift_left b7 48) in
  Int64.logor i (Int64.shift_left b8 56) |> Int64.float_of_bits

let repeat n f ch = Array.init n (fun _ -> f ch)
let vec f ch = repeat (uint ch) f ch
let v128 ch = really_input_string ch 16
let name ch = really_input_string ch (uint ch)

type section = { id : int; pos : int; size : int }

let next_section ch =
  if pos_in ch = ch.limit then None
  else
    let id = input_byte ch in
    let size = uint ch in
    Some { id; pos = pos_in ch; size }

let skip_section ch { pos; size; _ } = seek_in ch (pos + size)

let heaptype ch =
  let i = sint ch in
  match i + 128 with
  | 0X73 -> NoFunc
  | 0x72 -> NoExtern
  | 0x71 -> None_
  | 0x70 -> Func
  | 0x6F -> Extern
  | 0x6E -> Any
  | 0x6D -> Eq
  | 0x6C -> I31
  | 0x6B -> Struct
  | 0x6A -> Array
  | _ ->
      if i < 0 then failwith (Printf.sprintf "Unknown heaptype %x@." i);
      Type i

let nullable typ = { nullable = true; typ }
let ref_eq = { nullable = false; typ = Eq }
let ref_i31 = { nullable = false; typ = I31 }

let reftype i ch =
  match i with
  | 0X73 -> nullable NoFunc
  | 0x72 -> nullable NoExtern
  | 0x71 -> nullable None_
  | 0x70 -> nullable Func
  | 0x6F -> nullable Extern
  | 0x6E -> nullable Any
  | 0x6D -> nullable Eq
  | 0x6C -> nullable I31
  | 0x6B -> nullable Struct
  | 0x6A -> nullable Array
  | 0x63 -> nullable (heaptype ch)
  | 0x64 -> { nullable = false; typ = heaptype ch }
  | _ -> failwith (Printf.sprintf "Unknown reftype %x@." i)

let reftype_first_byte ch = reftype (input_byte ch) ch
let ref_i31 = Ref ref_i31
let ref_eq = Ref ref_eq

let valtype i ch =
  match i with
  | 0x7B -> V128
  | 0x7C -> F64
  | 0x7D -> F32
  | 0x7E -> I64
  | 0x7F -> I32
  | 0x64 -> (
      match peek_byte ch with
      | 0x6C ->
          ignore (input_byte ch);
          ref_i31
      | 0x6D ->
          ignore (input_byte ch);
          ref_eq
      | _ -> Ref { nullable = false; typ = heaptype ch })
  | _ -> Ref (reftype i ch)

let valtype_first_byte ch = valtype (uint ch) ch

let blocktype ch =
  let c = peek_byte ch in
  if c = 0x40 then (
    ignore (input_byte ch);
    None)
  else if c > 0x40 && c < 0x80 then Some (Valtype (valtype_first_byte ch))
  else Some (Typeuse (sint ch))

let storagetype ch =
  let i = uint ch in
  match i with
  | 0x78 -> Packed I8
  | 0x77 -> Packed I16
  | _ -> Value (valtype i ch)

let fieldtype ch =
  let typ = storagetype ch in
  let mut = input_byte ch <> 0 in
  { mut; typ }

let comptype i ch =
  match i with
  | 0x5E -> Array (fieldtype ch)
  | 0x5F -> Struct (vec fieldtype ch)
  | 0x60 ->
      let params = vec valtype_first_byte ch in
      let results = vec valtype_first_byte ch in
      Func { params; results }
  | c -> failwith (Printf.sprintf "Unknown comptype %d" c)

let supertype ch =
  match input_byte ch with
  | 0 -> None
  | 1 ->
      let t = uint ch in
      Some t
  | _ -> assert false

let subtype i ch =
  match i with
  | 0x50 ->
      let supertype = supertype ch in
      { final = false; supertype; typ = comptype (input_byte ch) ch }
  | 0x4F ->
      let supertype = supertype ch in
      { final = true; supertype; typ = comptype (input_byte ch) ch }
  | _ -> { final = true; supertype = None; typ = comptype i ch }

let rectype ch =
  match input_byte ch with
  | 0x4E -> vec (subtype (input_byte ch)) ch
  | i -> [| subtype i ch |]

let type_section ch =
  let n = uint ch in
  repeat n rectype ch

let limits ch =
  let kind = input_byte ch in
  assert (kind < 8);
  let address_type = if kind land 4 = 0 then `I32 else `I64 in
  let mi = uint64 ch in
  let ma = if kind land 1 = 0 then None else Some (uint64 ch) in
  { mi; ma; address_type }

let memtype ch = limits ch

let tabletype ch =
  let reftype = reftype_first_byte ch in
  let limits = limits ch in
  { limits; reftype }

let typeidx ch = uint ch

let globaltype ch =
  let typ = valtype_first_byte ch in
  let mut = input_byte ch in
  assert (mut < 2);
  { mut = mut <> 0; typ }

let import ch =
  let module_ = name ch in
  let name = name ch in
  let d = uint ch in
  if d > 4 then failwith (Printf.sprintf "Unknown import %x@." d);
  let map i = i in
  let importdesc =
    match d with
    | 0 -> Func (map (uint ch))
    | 1 -> Table (tabletype ch)
    | 2 -> Memory (memtype ch)
    | 3 -> Global (globaltype ch)
    | 4 ->
        let b = uint ch in
        assert (b = 0);
        Tag (map (uint ch))
    | _ -> assert false
  in
  { module_; name; desc = importdesc }

let exportable_kind d : exportable =
  match d with
  | 0 -> Func
  | 1 -> Table
  | 2 -> Memory
  | 3 -> Global
  | 4 -> Tag
  | _ -> assert false

let export ch =
  let export_name = name ch in
  let d = uint ch in
  if d > 4 then failwith (Printf.sprintf "Unknown export %x@." d);
  let idx = uint ch in
  let kind = exportable_kind d in
  { name = export_name; kind; index = idx }

let memarg ch =
  let a = uint ch in
  let o = uint64 ch in
  { align = Utils.Uint64.of_int a; offset = o }

let rec instructions ch acc =
  if pos_in ch = ch.limit then List.rev acc
  else
    match peek_byte ch with
    | 0x0B | 0x05 -> List.rev acc
    | _ -> instructions ch (instruction ch :: acc)

and instruction ch =
  let op = input_byte ch in
  let desc =
    match op with
    | 0x00 -> Unreachable
    | 0x01 -> Nop
    | 0x02 ->
        let typ = blocktype ch in
        let block = instructions ch [] in
        assert (input_byte ch = 0x0B);
        Block { label = (); typ; block }
    | 0x03 ->
        let typ = blocktype ch in
        let block = instructions ch [] in
        assert (input_byte ch = 0x0B);
        Loop { label = (); typ; block }
    | 0x04 ->
        let typ = blocktype ch in
        let if_block = instructions ch [] in
        let else_block =
          if input_byte ch = 0x05 then (
            let b = instructions ch [] in
            assert (input_byte ch = 0x0B);
            b)
          else []
        in
        If { label = (); typ; if_block; else_block }
    | 0x0C -> Br (uint ch)
    | 0x0D -> Br_if (uint ch)
    | 0x0E ->
        let targets = vec uint ch in
        let default = uint ch in
        Br_table (Array.to_list targets, default)
    | 0x0F -> Return
    | 0x10 -> Call (uint ch)
    | 0x11 ->
        let y = uint ch in
        let x = uint ch in
        CallIndirect (x, y)
    | 0x1A -> Drop
    | 0x1B -> Select None
    | 0x1C -> Select (Some (Array.to_list (vec valtype_first_byte ch)))
    | 0x20 -> LocalGet (uint ch)
    | 0x21 -> LocalSet (uint ch)
    | 0x22 -> LocalTee (uint ch)
    | 0x23 -> GlobalGet (uint ch)
    | 0x24 -> GlobalSet (uint ch)
    | 0x25 -> TableGet (uint ch)
    | 0x26 -> TableSet (uint ch)
    | 0x28 -> Load (0, memarg ch, NumI32)
    | 0x29 -> Load (0, memarg ch, NumI64)
    | 0x2A -> Load (0, memarg ch, NumF32)
    | 0x2B -> Load (0, memarg ch, NumF64)
    | 0x2C -> LoadS (0, memarg ch, `I32, `I8, Signed)
    | 0x2D -> LoadS (0, memarg ch, `I32, `I8, Unsigned)
    | 0x2E -> LoadS (0, memarg ch, `I32, `I16, Signed)
    | 0x2F -> LoadS (0, memarg ch, `I32, `I16, Unsigned)
    | 0x30 -> LoadS (0, memarg ch, `I64, `I8, Signed)
    | 0x31 -> LoadS (0, memarg ch, `I64, `I8, Unsigned)
    | 0x32 -> LoadS (0, memarg ch, `I64, `I16, Signed)
    | 0x33 -> LoadS (0, memarg ch, `I64, `I16, Unsigned)
    | 0x34 -> LoadS (0, memarg ch, `I64, `I32, Signed)
    | 0x35 -> LoadS (0, memarg ch, `I64, `I32, Unsigned)
    | 0x36 -> Store (0, memarg ch, NumI32)
    | 0x37 -> Store (0, memarg ch, NumI64)
    | 0x38 -> Store (0, memarg ch, NumF32)
    | 0x39 -> Store (0, memarg ch, NumF64)
    | 0x3A -> StoreS (0, memarg ch, `I32, `I8)
    | 0x3B -> StoreS (0, memarg ch, `I32, `I16)
    | 0x3C -> StoreS (0, memarg ch, `I64, `I8)
    | 0x3D -> StoreS (0, memarg ch, `I64, `I16)
    | 0x3E -> StoreS (0, memarg ch, `I64, `I32)
    | 0x3F ->
        assert (input_byte ch = 0);
        MemorySize 0
    | 0x40 ->
        assert (input_byte ch = 0);
        MemoryGrow 0
    | 0x41 -> Const (I32 (sint32 ch))
    | 0x42 -> Const (I64 (sint64 ch))
    | 0x43 -> Const (F32 (float32 ch))
    | 0x44 -> Const (F64 (float64 ch))
    | 0x45 -> UnOp (I32 Eqz)
    | 0x46 -> BinOp (I32 Eq)
    | 0x47 -> BinOp (I32 Ne)
    | 0x48 -> BinOp (I32 (Lt Signed))
    | 0x49 -> BinOp (I32 (Lt Unsigned))
    | 0x4A -> BinOp (I32 (Gt Signed))
    | 0x4B -> BinOp (I32 (Gt Unsigned))
    | 0x4C -> BinOp (I32 (Le Signed))
    | 0x4D -> BinOp (I32 (Le Unsigned))
    | 0x4E -> BinOp (I32 (Ge Signed))
    | 0x4F -> BinOp (I32 (Ge Unsigned))
    | 0x50 -> UnOp (I64 Eqz)
    | 0x51 -> BinOp (I64 Eq)
    | 0x52 -> BinOp (I64 Ne)
    | 0x53 -> BinOp (I64 (Lt Signed))
    | 0x54 -> BinOp (I64 (Lt Unsigned))
    | 0x55 -> BinOp (I64 (Gt Signed))
    | 0x56 -> BinOp (I64 (Gt Unsigned))
    | 0x57 -> BinOp (I64 (Le Signed))
    | 0x58 -> BinOp (I64 (Le Unsigned))
    | 0x59 -> BinOp (I64 (Ge Signed))
    | 0x5A -> BinOp (I64 (Ge Unsigned))
    | 0x5B -> BinOp (F32 Eq)
    | 0x5C -> BinOp (F32 Ne)
    | 0x5D -> BinOp (F32 Lt)
    | 0x5E -> BinOp (F32 Gt)
    | 0x5F -> BinOp (F32 Le)
    | 0x60 -> BinOp (F32 Ge)
    | 0x61 -> BinOp (F64 Eq)
    | 0x62 -> BinOp (F64 Ne)
    | 0x63 -> BinOp (F64 Lt)
    | 0x64 -> BinOp (F64 Gt)
    | 0x65 -> BinOp (F64 Le)
    | 0x66 -> BinOp (F64 Ge)
    | 0x67 -> UnOp (I32 Clz)
    | 0x68 -> UnOp (I32 Ctz)
    | 0x69 -> UnOp (I32 Popcnt)
    | 0x6A -> BinOp (I32 Add)
    | 0x6B -> BinOp (I32 Sub)
    | 0x6C -> BinOp (I32 Mul)
    | 0x6D -> BinOp (I32 (Div Signed))
    | 0x6E -> BinOp (I32 (Div Unsigned))
    | 0x6F -> BinOp (I32 (Rem Signed))
    | 0x70 -> BinOp (I32 (Rem Unsigned))
    | 0x71 -> BinOp (I32 And)
    | 0x72 -> BinOp (I32 Or)
    | 0x73 -> BinOp (I32 Xor)
    | 0x74 -> BinOp (I32 Shl)
    | 0x75 -> BinOp (I32 (Shr Signed))
    | 0x76 -> BinOp (I32 (Shr Unsigned))
    | 0x77 -> BinOp (I32 Rotl)
    | 0x78 -> BinOp (I32 Rotr)
    | 0x79 -> UnOp (I64 Clz)
    | 0x7A -> UnOp (I64 Ctz)
    | 0x7B -> UnOp (I64 Popcnt)
    | 0x7C -> BinOp (I64 Add)
    | 0x7D -> BinOp (I64 Sub)
    | 0x7E -> BinOp (I64 Mul)
    | 0x7F -> BinOp (I64 (Div Signed))
    | 0x80 -> BinOp (I64 (Div Unsigned))
    | 0x81 -> BinOp (I64 (Rem Signed))
    | 0x82 -> BinOp (I64 (Rem Unsigned))
    | 0x83 -> BinOp (I64 And)
    | 0x84 -> BinOp (I64 Or)
    | 0x85 -> BinOp (I64 Xor)
    | 0x86 -> BinOp (I64 Shl)
    | 0x87 -> BinOp (I64 (Shr Signed))
    | 0x88 -> BinOp (I64 (Shr Unsigned))
    | 0x89 -> BinOp (I64 Rotl)
    | 0x8A -> BinOp (I64 Rotr)
    | 0x8B -> UnOp (F32 Abs)
    | 0x8C -> UnOp (F32 Neg)
    | 0x8D -> UnOp (F32 Ceil)
    | 0x8E -> UnOp (F32 Floor)
    | 0x8F -> UnOp (F32 Trunc)
    | 0x90 -> UnOp (F32 Nearest)
    | 0x91 -> UnOp (F32 Sqrt)
    | 0x92 -> BinOp (F32 Add)
    | 0x93 -> BinOp (F32 Sub)
    | 0x94 -> BinOp (F32 Mul)
    | 0x95 -> BinOp (F32 Div)
    | 0x96 -> BinOp (F32 Min)
    | 0x97 -> BinOp (F32 Max)
    | 0x98 -> BinOp (F32 CopySign)
    | 0x99 -> UnOp (F64 Abs)
    | 0x9A -> UnOp (F64 Neg)
    | 0x9B -> UnOp (F64 Ceil)
    | 0x9C -> UnOp (F64 Floor)
    | 0x9D -> UnOp (F64 Trunc)
    | 0x9E -> UnOp (F64 Nearest)
    | 0x9F -> UnOp (F64 Sqrt)
    | 0xA0 -> BinOp (F64 Add)
    | 0xA1 -> BinOp (F64 Sub)
    | 0xA2 -> BinOp (F64 Mul)
    | 0xA3 -> BinOp (F64 Div)
    | 0xA4 -> BinOp (F64 Min)
    | 0xA5 -> BinOp (F64 Max)
    | 0xA6 -> BinOp (F64 CopySign)
    | 0xA7 -> I32WrapI64
    | 0xA8 -> UnOp (I32 (Trunc (`F32, Signed)))
    | 0xA9 -> UnOp (I32 (Trunc (`F32, Unsigned)))
    | 0xAA -> UnOp (I32 (Trunc (`F64, Signed)))
    | 0xAB -> UnOp (I32 (Trunc (`F64, Unsigned)))
    | 0xAC -> I64ExtendI32 Signed
    | 0xAD -> I64ExtendI32 Unsigned
    | 0xAE -> UnOp (I64 (Trunc (`F32, Signed)))
    | 0xAF -> UnOp (I64 (Trunc (`F32, Unsigned)))
    | 0xB0 -> UnOp (I64 (Trunc (`F64, Signed)))
    | 0xB1 -> UnOp (I64 (Trunc (`F64, Unsigned)))
    | 0xB2 -> UnOp (F32 (Convert (`I32, Signed)))
    | 0xB3 -> UnOp (F32 (Convert (`I32, Unsigned)))
    | 0xB4 -> UnOp (F32 (Convert (`I64, Signed)))
    | 0xB5 -> UnOp (F32 (Convert (`I64, Unsigned)))
    | 0xB6 -> F32DemoteF64
    | 0xB7 -> UnOp (F64 (Convert (`I32, Signed)))
    | 0xB8 -> UnOp (F64 (Convert (`I32, Unsigned)))
    | 0xB9 -> UnOp (F64 (Convert (`I64, Signed)))
    | 0xBA -> UnOp (F64 (Convert (`I64, Unsigned)))
    | 0xBB -> F64PromoteF32
    | 0xBC -> UnOp (I32 Reinterpret)
    | 0xBD -> UnOp (I64 Reinterpret)
    | 0xBE -> UnOp (F32 Reinterpret)
    | 0xBF -> UnOp (F64 Reinterpret)
    | 0xC0 -> UnOp (I32 (ExtendS `_8))
    | 0xC1 -> UnOp (I32 (ExtendS `_16))
    | 0xC2 -> UnOp (I64 (ExtendS `_8))
    | 0xC3 -> UnOp (I64 (ExtendS `_16))
    | 0xC4 -> UnOp (I64 (ExtendS `_32))
    | 0xD0 -> RefNull (heaptype ch)
    | 0xD1 -> RefIsNull
    | 0xD2 -> RefFunc (uint ch)
    | 0xFB -> (
        match uint ch with
        | 0 -> StructNew (uint ch)
        | 1 -> StructNewDefault (uint ch)
        | 2 ->
            let i = uint ch in
            StructGet (None, i, uint ch)
        | 3 ->
            let i = uint ch in
            StructGet (Some Signed, i, uint ch)
        | 4 -> let i = uint ch in

               StructGet (Some Unsigned, i, uint ch)
        | 5 ->
            let i = uint ch in
            StructSet (i, uint ch)
        | 6 -> ArrayNew (uint ch)
        | 7 -> ArrayNewDefault (uint ch)
        | 8 ->
            let i = uint ch in
            ArrayNewFixed (i, Utils.Uint32.of_int (uint ch))
        | 9 ->
            let i = uint ch in
            ArrayNewData (i, uint ch)
        | 10 ->
            let i = uint ch in
            ArrayNewElem (i, uint ch)
        | 11 -> ArrayGet (None, uint ch)
        | 12 -> ArrayGet (Some Signed, uint ch)
        | 13 -> ArrayGet (Some Unsigned, uint ch)
        | 14 -> ArraySet (uint ch)
        | 15 -> ArrayLen
        | 16 -> ArrayFill (uint ch)
        | 17 ->
            let i = uint ch in
            ArrayCopy (i, uint ch)
        | 18 ->
            let i = uint ch in
            ArrayInitData (i, uint ch)
        | 19 ->
            let i = uint ch in
            ArrayInitElem (i, uint ch)
        | 20 -> RefTest (nullable (heaptype ch))
        | 21 -> RefTest { nullable = false; typ = heaptype ch }
        | 22 -> RefCast (nullable (heaptype ch))
        | 23 -> RefCast { nullable = false; typ = heaptype ch }
        | 24 ->
            let flags = input_byte ch in
            let label = uint ch in
            let ht1 = heaptype ch in
            let ht2 = heaptype ch in
            let rt1 = { nullable = flags land 1 <> 0; typ = ht1 } in
            let rt2 = { nullable = flags land 2 <> 0; typ = ht2 } in
            Br_on_cast (label, rt1, rt2)
        | 25 ->
            let flags = input_byte ch in
            let label = uint ch in
            let ht1 = heaptype ch in
            let ht2 = heaptype ch in
            let rt1 = { nullable = flags land 1 <> 0; typ = ht1 } in
            let rt2 = { nullable = flags land 2 <> 0; typ = ht2 } in
            Br_on_cast_fail (label, rt1, rt2)
        | 26 -> AnyConvertExtern
        | 27 -> ExternConvertAny
        | 28 -> RefI31
        | 29 -> I31Get Signed
        | 30 -> I31Get Unsigned
        | c -> failwith (Printf.sprintf "Unknown GC op %d" c))
    | 0xFC -> (
        match uint ch with
        | 0 -> UnOp (I32 (TruncSat (`F32, Signed)))
        | 1 -> UnOp (I32 (TruncSat (`F32, Unsigned)))
        | 2 -> UnOp (I32 (TruncSat (`F64, Signed)))
        | 3 -> UnOp (I32 (TruncSat (`F64, Unsigned)))
        | 4 -> UnOp (I64 (TruncSat (`F32, Signed)))
        | 5 -> UnOp (I64 (TruncSat (`F32, Unsigned)))
        | 6 -> UnOp (I64 (TruncSat (`F64, Signed)))
        | 7 -> UnOp (I64 (TruncSat (`F64, Unsigned)))
        | 8 ->
            let i = uint ch in
            let m = uint ch in
            MemoryInit (i, m)
        | 9 -> DataDrop (uint ch)
        | 10 ->
            let m_dst = uint ch in
            let m_src = uint ch in
            MemoryCopy (m_dst, m_src)
        | 11 ->
            let m = uint ch in
            MemoryFill m
        | 12 ->
            let i = uint ch in
            TableInit (i, uint ch)
        | 13 -> ElemDrop (uint ch)
        | 14 ->
            let i = uint ch in
            TableCopy (i, uint ch)
        | 15 -> TableGrow (uint ch)
        | 16 -> TableSize (uint ch)
        | 17 -> TableFill (uint ch)
        | c -> failwith (Printf.sprintf "Unknown 0xFC op %d" c))
    | 0x05 -> failwith "Unexpected Else instruction"
    | 0x06 -> failwith "Unknown 0x06 instruction"
    | 0x07 | 0x08 | 0x09 | 0x0A -> failwith "Unknown opcode"
    | 0x0B -> failwith "Unexpected End instruction"
    | 0xFD -> (
        match uint ch with
        | 0 -> VecLoad (0, Load128, memarg ch)
        | 1 -> VecLoad (0, Load8x8S, memarg ch)
        | 2 -> VecLoad (0, Load8x8U, memarg ch)
        | 3 -> VecLoad (0, Load16x4S, memarg ch)
        | 4 -> VecLoad (0, Load16x4U, memarg ch)
        | 5 -> VecLoad (0, Load32x2S, memarg ch)
        | 6 -> VecLoad (0, Load32x2U, memarg ch)
        | 7 -> VecLoadSplat (0, `I8, memarg ch)
        | 8 -> VecLoadSplat (0, `I16, memarg ch)
        | 9 -> VecLoadSplat (0, `I32, memarg ch)
        | 10 -> VecLoadSplat (0, `I64, memarg ch)
        | 11 -> VecStore (0, memarg ch)
        | 12 -> VecConst (v128 ch)
        | 13 -> VecShuffle (Shuffle, v128 ch)
        | 14 -> VecBinOp VecSwizzle
        | 15 -> VecSplat (Splat I8x16)
        | 16 -> VecSplat (Splat I16x8)
        | 17 -> VecSplat (Splat I32x4)
        | 18 -> VecSplat (Splat I64x2)
        | 19 -> VecSplat (Splat F32x4)
        | 20 -> VecSplat (Splat F64x2)
        | 21 -> VecExtract (I8x16, Some Signed, Int32.of_int (uint ch))
        | 22 -> VecExtract (I8x16, Some Unsigned, Int32.of_int (uint ch))
        | 23 -> VecReplace (I8x16, Int32.of_int (uint ch))
        | 24 -> VecExtract (I16x8, Some Signed, Int32.of_int (uint ch))
        | 25 -> VecExtract (I16x8, Some Unsigned, Int32.of_int (uint ch))
        | 26 -> VecReplace (I16x8, Int32.of_int (uint ch))
        | 27 -> VecExtract (I32x4, None, Int32.of_int (uint ch))
        | 28 -> VecReplace (I32x4, Int32.of_int (uint ch))
        | 29 -> VecExtract (I64x2, None, Int32.of_int (uint ch))
        | 30 -> VecReplace (I64x2, Int32.of_int (uint ch))
        | 31 -> VecExtract (F32x4, None, Int32.of_int (uint ch))
        | 32 -> VecReplace (F32x4, Int32.of_int (uint ch))
        | 33 -> VecExtract (F64x2, None, Int32.of_int (uint ch))
        | 34 -> VecReplace (F64x2, Int32.of_int (uint ch))
        | 35 -> VecBinOp (VecEq I8x16)
        | 36 -> VecBinOp (VecNe I8x16)
        | 77 -> VecUnOp VecNot
        | 78 -> VecBinOp VecAnd
        | 79 -> VecBinOp VecAndNot
        | 80 -> VecBinOp VecOr
        | 81 -> VecBinOp VecXor

        | 84 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecLoadLane (0, `I8, m, Int32.of_int l)
        | 85 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecLoadLane (0, `I16, m, Int32.of_int l)
        | 86 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecLoadLane (0, `I32, m, Int32.of_int l)
        | 87 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecLoadLane (0, `I64, m, Int32.of_int l)
        | 88 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecStoreLane (0, `I8, m, Int32.of_int l)
        | 89 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecStoreLane (0, `I16, m, Int32.of_int l)
        | 90 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecStoreLane (0, `I32, m, Int32.of_int l)
        | 91 ->
            let m = memarg ch in
            let l = input_byte ch in
            VecStoreLane (0, `I64, m, Int32.of_int l)
        | 92 -> VecLoadSplat (0, `I8, memarg ch)
        | 93 -> VecLoadSplat (0, `I16, memarg ch)
        | 94 -> VecLoadSplat (0, `I32, memarg ch)
        | 95 -> VecLoadSplat (0, `I64, memarg ch)
        | 96 -> VecUnOp (VecAbs I8x16)
        | 97 -> VecUnOp (VecNeg I8x16)
        | 98 -> VecTest (AnyTrue I8x16)
        | 99 -> VecTest (AllTrue I8x16)
        | 100 -> VecBitmask (Bitmask I8x16)
        | 101 -> VecBinOp (VecNarrow (Signed, I8x16))
        | 102 -> VecBinOp (VecNarrow (Unsigned, I8x16))
        | 107 -> VecShift (Shl I8x16)
        | 108 -> VecShift (Shr (Signed, I8x16))
        | 109 -> VecShift (Shr (Unsigned, I8x16))
        | 110 -> VecBinOp (VecAdd I8x16)
        | 111 -> VecBinOp (VecAddSat (Signed, I8x16))
        | 112 -> VecBinOp (VecAddSat (Unsigned, I8x16))
        | 113 -> VecBinOp (VecSub I8x16)
        | 114 -> VecBinOp (VecSubSat (Signed, I8x16))
        | 115 -> VecBinOp (VecSubSat (Unsigned, I8x16))
        | 118 -> VecBinOp (VecMin (Some Signed, I8x16))
        | 119 -> VecBinOp (VecMin (Some Unsigned, I8x16))
        | 120 -> VecBinOp (VecMax (Some Signed, I8x16))
        | 121 -> VecBinOp (VecMax (Some Unsigned, I8x16))
        | 123 -> VecBinOp (VecAvgr (Unsigned, I8x16))
        | 124 -> VecUnOp (VecPopcnt I8x16)
        | 128 -> VecUnOp (VecAbs I16x8)
        | 129 -> VecUnOp (VecNeg I16x8)
        | 130 -> VecTest (AnyTrue I16x8)
        | 131 -> VecTest (AllTrue I16x8)
        | 132 -> VecBitmask (Bitmask I16x8)
        | 133 -> VecBinOp (VecNarrow (Signed, I16x8))
        | 134 -> VecBinOp (VecNarrow (Unsigned, I16x8))
        | 135 -> VecUnOp (VecExtend (`Low, `_8, Signed, I16x8))
        | 136 -> VecUnOp (VecExtend (`High, `_8, Signed, I16x8))
        | 137 -> VecUnOp (VecExtend (`Low, `_8, Unsigned, I16x8))
        | 138 -> VecUnOp (VecExtend (`High, `_8, Unsigned, I16x8))
        | 139 -> VecShift (Shl I16x8)
        | 140 -> VecShift (Shr (Signed, I16x8))
        | 141 -> VecShift (Shr (Unsigned, I16x8))
        | 142 -> VecBinOp (VecAdd I16x8)
        | 143 -> VecBinOp (VecAddSat (Signed, I16x8))
        | 144 -> VecBinOp (VecAddSat (Unsigned, I16x8))
        | 145 -> VecBinOp (VecSub I16x8)
        | 146 -> VecBinOp (VecSubSat (Signed, I16x8))
        | 147 -> VecBinOp (VecSubSat (Unsigned, I16x8))
        | 149 -> VecBinOp (VecMul I16x8)
        | 150 -> VecBinOp (VecMin (Some Signed, I16x8))
        | 151 -> VecBinOp (VecMin (Some Unsigned, I16x8))
        | 152 -> VecBinOp (VecMax (Some Signed, I16x8))
        | 153 -> VecBinOp (VecMax (Some Unsigned, I16x8))
        | 154 -> VecBinOp (VecAvgr (Unsigned, I16x8))
        | 155 -> VecBinOp (VecQ15MulrSat I16x8)
        | 156 -> VecBinOp (VecExtMulLow (Signed, I16x8))
        | 157 -> VecBinOp (VecExtMulHigh (Signed, I16x8))
        | 158 -> VecBinOp (VecExtMulLow (Unsigned, I16x8))
        | 159 -> VecBinOp (VecExtMulHigh (Unsigned, I16x8))
        | 160 -> VecUnOp (VecAbs I32x4)
        | 161 -> VecUnOp (VecNeg I32x4)
        | 162 -> VecTest (AnyTrue I32x4)
        | 163 -> VecTest (AllTrue I32x4)
        | 164 -> VecBitmask (Bitmask I32x4)
        | 167 -> VecUnOp (VecExtend (`Low, `_16, Signed, I32x4))
        | 168 -> VecUnOp (VecExtend (`High, `_16, Signed, I32x4))
        | 169 -> VecUnOp (VecExtend (`Low, `_16, Unsigned, I32x4))
        | 170 -> VecUnOp (VecExtend (`High, `_16, Unsigned, I32x4))
        | 171 -> VecShift (Shl I32x4)
        | 172 -> VecShift (Shr (Signed, I32x4))
        | 173 -> VecShift (Shr (Unsigned, I32x4))
        | 174 -> VecBinOp (VecAdd I32x4)
        | 177 -> VecBinOp (VecSub I32x4)
        | 181 -> VecBinOp (VecMul I32x4)
        | 182 -> VecBinOp (VecMin (Some Signed, I32x4))
        | 183 -> VecBinOp (VecMin (Some Unsigned, I32x4))
        | 184 -> VecBinOp (VecMax (Some Signed, I32x4))
        | 185 -> VecBinOp (VecMax (Some Unsigned, I32x4))
        | 186 -> VecBinOp (VecDot I32x4)
        | 187 -> VecUnOp (VecExtAddPairwise (Signed, I32x4))
        | 188 -> VecUnOp (VecExtAddPairwise (Unsigned, I32x4))
        | 192 -> VecUnOp (VecAbs I64x2)
        | 193 -> VecUnOp (VecNeg I64x2)
        | 194 -> VecTest (AnyTrue I64x2)
        | 195 -> VecTest (AllTrue I64x2)
        | 196 -> VecBitmask (Bitmask I64x2)
        | 199 -> VecUnOp (VecExtend (`Low, `_32, Signed, I64x2))
        | 200 -> VecUnOp (VecExtend (`High, `_32, Signed, I64x2))
        | 201 -> VecUnOp (VecExtend (`Low, `_32, Unsigned, I64x2))
        | 202 -> VecUnOp (VecExtend (`High, `_32, Unsigned, I64x2))
        | 203 -> VecShift (Shl I64x2)
        | 204 -> VecShift (Shr (Signed, I64x2))
        | 205 -> VecShift (Shr (Unsigned, I64x2))
        | 206 -> VecBinOp (VecAdd I64x2)
        | 207 -> VecBinOp (VecSub I64x2)
        | 208 -> VecBinOp (VecMul I64x2)
        | 224 -> VecUnOp (VecAbs F32x4)
        | 225 -> VecUnOp (VecNeg F32x4)
        | 227 -> VecUnOp (VecSqrt F32x4)
        | 228 -> VecBinOp (VecAdd F32x4)
        | 229 -> VecBinOp (VecSub F32x4)
        | 230 -> VecBinOp (VecMul F32x4)
        | 231 -> VecBinOp (VecDiv F32x4)
        | 232 -> VecBinOp (VecMin (None, F32x4))
        | 233 -> VecBinOp (VecMax (None, F32x4))
        | 234 -> VecBinOp (VecPMin F32x4)
        | 235 -> VecBinOp (VecPMax F32x4)
        | 236 -> VecUnOp (VecCeil F32x4)
        | 237 -> VecUnOp (VecFloor F32x4)
        | 238 -> VecUnOp (VecTrunc F32x4)
        | 239 -> VecUnOp (VecNearest F32x4)
        | 240 -> VecUnOp (VecAbs F64x2)
        | 241 -> VecUnOp (VecNeg F64x2)
        | 243 -> VecUnOp (VecSqrt F64x2)
        | 244 -> VecBinOp (VecAdd F64x2)
        | 245 -> VecBinOp (VecSub F64x2)
        | 246 -> VecBinOp (VecMul F64x2)
        | 247 -> VecBinOp (VecDiv F64x2)
        | 248 -> VecBinOp (VecMin (None, F64x2))
        | 249 -> VecBinOp (VecMax (None, F64x2))
        | 250 -> VecBinOp (VecPMin F64x2)
        | 251 -> VecBinOp (VecPMax F64x2)
        | 252 -> VecUnOp (VecCeil F64x2)
        | 253 -> VecUnOp (VecFloor F64x2)
        | 254 -> VecUnOp (VecTrunc F64x2)
        | 255 -> VecUnOp (VecNearest F64x2)
        (* Relaxed SIMD *)
        | 0x100 -> VecBinOp VecRelaxedSwizzle
        | 0x101 -> VecUnOp (VecRelaxedTrunc (`F32, Signed, I32x4))
        | 0x102 -> VecUnOp (VecRelaxedTrunc (`F32, Unsigned, I32x4))
        | 0x103 -> VecUnOp (VecRelaxedTruncZero (`F64, Signed, I32x4))
        | 0x104 -> VecUnOp (VecRelaxedTruncZero (`F64, Unsigned, I32x4))
        | 0x105 -> VecTernOp (VecRelaxedMAdd F32x4)
        | 0x106 -> VecTernOp (VecRelaxedNMAdd F32x4)
        | 0x107 -> VecTernOp (VecRelaxedMAdd F64x2)
        | 0x108 -> VecTernOp (VecRelaxedNMAdd F64x2)
        | 0x109 -> VecTernOp (VecRelaxedLaneSelect I8x16)
        | 0x10a -> VecTernOp (VecRelaxedLaneSelect I16x8)
        | 0x10b -> VecTernOp (VecRelaxedLaneSelect I32x4)
        | 0x10c -> VecTernOp (VecRelaxedLaneSelect I64x2)
        | 0x10d -> VecBinOp (VecRelaxedMin F32x4)
        | 0x10e -> VecBinOp (VecRelaxedMax F32x4)
        | 0x10f -> VecBinOp (VecRelaxedMin F64x2)
        | 0x110 -> VecBinOp (VecRelaxedMax F64x2)
        | 0x111 -> VecBinOp (VecRelaxedQ15Mulr (Signed, I16x8))
        | 0x112 -> VecBinOp (VecRelaxedDot I16x8)
        | 0x113 -> VecTernOp (VecRelaxedDotAdd I32x4)
        | c -> failwith (Printf.sprintf "Unknown SIMD opcode 0x%02X" c))
    | c -> failwith (Printf.sprintf "Unknown opcode 0x%02X" c)
  in
  Ast.no_loc desc

let expr ch =
  let instrs = instructions ch [] in
  if input_byte ch <> 0x0B then failwith "expr must end with 0x0B";
  instrs

let elem ch =
  let mode_byte = uint ch in
  let typ =
    match mode_byte with
    | 0x00 | 0x04 -> { nullable = false; typ = Func } (* funcref *)
    | _ -> reftype_first_byte ch
  in
  let mode =
    match mode_byte with
    | 0x00 ->
        (* Active, table 0, funcref *)
        let offset_expr = expr ch in
        Active (0, offset_expr)
    | 0x01 -> Passive
    | 0x02 ->
        (* Active, explicit tableidx *)
        let table_idx = uint ch in
        let offset_expr = expr ch in
        Active (table_idx, offset_expr)
    | 0x03 -> Declare
    | 0x04 ->
        (* Active, legacy vector of funcidx *)
        let offset_expr = expr ch in
        Active (0, offset_expr)
    | _ -> failwith (Printf.sprintf "Unknown elem mode 0x%02X" mode_byte)
  in
  let init = Array.to_list (vec expr ch) in
  { typ; init; mode }

let table ch =
  let next_byte = peek_byte ch in
  if next_byte = 0x40 then (
    (* Case 2: 0x40 0x00 tabletype expr *)
    let marker = input_byte ch in
    let attribute = input_byte ch in
    assert (marker = 0x40);
    assert (attribute = 0x00);
    let typ = tabletype ch in
    let expr = expr ch in
    { typ; expr = Some expr })
  else
    (* Case 1: tabletype *)
    let typ = tabletype ch in
    { typ; expr = None }

let code ch =
  let size = uint ch in
  let start_pos = pos_in ch in
  let locals =
    let n = uint ch in
    let vec_locals ch =
      let n = uint ch in
      let t = valtype_first_byte ch in
      List.init n (fun _ -> t)
    in
    List.flatten (Array.to_list (repeat n vec_locals ch))
  in
  let instrs = expr ch in
  assert (pos_in ch = start_pos + size);
  { locals; instrs }

let data ch =
  let mode_byte = uint ch in
  let (mode : Ast.location Ast.Binary.datamode) =
    match mode_byte with
    | 0x00 ->
        (* Active, memory 0 *)
        let offset_expr = expr ch in
        Active (0, offset_expr)
    | 0x01 -> Passive
    | 0x02 ->
        (* Active, explicit memory index *)
        let mem_idx = uint ch in
        let offset_expr = expr ch in
        Active (mem_idx, offset_expr)
    | _ -> failwith (Printf.sprintf "Unknown data mode 0x%02X" mode_byte)
  in
  let init_len = uint ch in
  let init_str = really_input_string ch init_len in
  { Ast.Binary.init = init_str; mode }

let tag ch =
  let b = input_byte ch in
  assert (b = 0);
  typeidx ch

let empty_names =
  {
    module_ = None;
    functions = IntMap.empty;
    locals = IntMap.empty;
    labels = IntMap.empty;
    types = IntMap.empty;
    fields = IntMap.empty;
    tags = IntMap.empty;
    globals = IntMap.empty;
    tables = IntMap.empty;
    memories = IntMap.empty;
    data = IntMap.empty;
    elem = IntMap.empty;
  }

let name_map' f ch =
  Array.fold_left
    (fun acc (idx, n) -> IntMap.add idx n acc)
    IntMap.empty (vec f ch)

let name_assoc ch =
  let i = uint ch in
  (i, name ch)

let name_map ch = name_map' name_assoc ch

let indirect_name_map ch =
  name_map'
    (fun ch ->
      let i = uint ch in
      (i, name_map ch))
    ch

let module_ buf =
  check_header "input" buf;
  let ch = { buf; pos = 8; limit = String.length buf } in
  ch.pos <- 8;
  (* Reset position after index scan *)
  let rec loop m =
    match next_section ch with
    | None -> m
    | Some sect -> (
        match sect.id with
        | 1 ->
            (* Type section *)
            loop { m with types = Array.to_list (type_section ch) }
        | 2 ->
            (* Import section *)
            loop { m with imports = Array.to_list (vec import ch) }
        | 3 ->
            (* Function section *)
            loop { m with functions = Array.to_list (vec typeidx ch) }
        | 4 ->
            (* Table section *)
            let tables = Array.to_list (vec table ch) in
            loop { m with tables }
        | 5 ->
            (* Memory section *)
            loop { m with memories = Array.to_list (vec limits ch) }
        | 6 ->
            (* Global section *)
            let globals =
              Array.to_list
                (vec
                   (fun ch ->
                     let typ = globaltype ch in
                     { typ; init = expr ch })
                   ch)
            in
            loop { m with globals }
        | 7 ->
            (* Export section *)
            loop { m with exports = Array.to_list (vec export ch) }
        | 8 ->
            (* Start section *)
            loop { m with start = Some (uint ch) }
        | 9 ->
            (* Element section *)
            loop { m with elem = Array.to_list (vec elem ch) }
        | 10 ->
            (* Code section *)
            loop { m with code = Array.to_list (vec code ch) }
        | 11 ->
            (* Data section *)
            loop { m with data = Array.to_list (vec data ch) }
        | 12 ->
            (* DataCount section *)
            let _ = uint ch in
            loop m
        | 13 ->
            (* Tag section *)
            let tags = Array.to_list (vec tag ch) in
            loop { m with Ast.Binary.tags }
        | 0 -> (
            (* Custom section *)
            let custom_name = name ch in
            let start_pos = pos_in ch in
            match custom_name with
            | "name" ->
                let rec parse_name_subsections current_names =
                  if pos_in ch = start_pos + sect.size then current_names
                  else
                    let subsection_id = uint ch in
                    let subsection_size = uint ch in
                    let subsection_start_pos = pos_in ch in
                    let updated_names =
                      match subsection_id with
                      | 0 ->
                          (* Module name *)
                          let module_name = name ch in
                          {
                            current_names with
                            Ast.Binary.module_ = Some module_name;
                          }
                      | 1 ->
                          (* Function names *)
                          { current_names with functions = name_map ch }
                      | 2 ->
                          (* Local names *)
                          { current_names with locals = indirect_name_map ch }
                      | 3 ->
                          (* Label names *)
                          { current_names with labels = indirect_name_map ch }
                      | 4 ->
                          (* Type names *)
                          { current_names with types = name_map ch }
                      | 5 ->
                          (* Table names *)
                          { current_names with tables = name_map ch }
                      | 6 ->
                          (* Memory names *)
                          { current_names with memories = name_map ch }
                      | 7 ->
                          (* Global names *)
                          { current_names with globals = name_map ch }
                      | 8 ->
                          (* Elem names *)
                          { current_names with elem = name_map ch }
                      | 9 ->
                          (* Data names *)
                          { current_names with data = name_map ch }
                      | 10 ->
                          (* Field names *)
                          { current_names with fields = indirect_name_map ch }
                      | 11 ->
                          (* Tag names *)
                          { current_names with tags = name_map ch }
                      | _ -> current_names (* Skip unknown subsections *)
                    in
                    seek_in ch (subsection_start_pos + subsection_size);
                    parse_name_subsections updated_names
                in
                let names = parse_name_subsections m.names in
                loop { m with Ast.Binary.names }
            | _ ->
                (* Skip other custom sections *)
                skip_section ch sect;
                loop m)
        | _ ->
            skip_section ch sect;
            loop m)
  in
  loop
    {
      Ast.Binary.types = [];
      imports = [];
      functions = [];
      tables = [];
      memories = [];
      tags = [];
      globals = [];
      exports = [];
      start = None;
      elem = [];
      code = [];
      data = [];
      names = empty_names;
    }
