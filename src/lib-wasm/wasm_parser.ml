let exportable_kind d : Ast.exportable =
  match d with
  | 0 -> Func
  | 1 -> Table
  | 2 -> Memory
  | 3 -> Global
  | 4 -> Tag
  | _ -> assert false

module Read = struct
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
    else
      Int32.add (Int32.sub i 128l) (Int32.shift_left (sint32 ~n:(n - 1) ch) 7)

  let rec sint64 ?(n = 10) ch =
    let i = Int64.of_int (input_byte ch) in
    if n = 1 then assert (Int64.compare i 128L < 0);
    if Int64.compare i 64L < 0 then i
    else if Int64.compare i 128L < 0 then Int64.sub i 128L
    else
      Int64.add (Int64.sub i 128L) (Int64.shift_left (sint64 ~n:(n - 1) ch) 7)

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
  let name ch = really_input_string ch (uint ch)

  type section = { id : int; pos : int; size : int }

  let next_section ch =
    if pos_in ch = ch.limit then None
    else
      let id = input_byte ch in
      let size = uint ch in
      Some { id; pos = pos_in ch; size }

  let skip_section ch { pos; size; _ } = seek_in ch (pos + size)

  type t = { ch : ch }

  let heaptype ch : Ast.Binary.heaptype =
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
        Ast.Binary.Type i

  let nullable typ : Ast.Binary.reftype = { nullable = true; typ }
  let ref_eq : Ast.Binary.reftype = { nullable = false; typ = Eq }
  let ref_i31 : Ast.Binary.reftype = { nullable = false; typ = I31 }

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
  let ref_i31 = Ast.Binary.Ref ref_i31
  let ref_eq = Ast.Binary.Ref ref_eq

  let valtype i ch : Ast.Binary.valtype =
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
    else if c > 0x40 && c < 0x80 then
      Some (Ast.Binary.Valtype (valtype_first_byte ch))
    else Some (Ast.Binary.Typeuse (sint ch))

  let storagetype ch : Ast.Binary.storagetype =
    let i = uint ch in
    match i with
    | 0x78 -> Packed I8
    | 0x77 -> Packed I16
    | _ -> Value (valtype i ch)

  let fieldtype ch : Ast.Binary.fieldtype =
    let typ = storagetype ch in
    let mut = input_byte ch <> 0 in
    { mut; typ }

  let comptype i ch : Ast.Binary.comptype =
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

  let subtype i ch : Ast.Binary.subtype =
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
    | 0x4E -> vec (fun ch -> subtype (input_byte ch) ch) ch
    | i -> [| subtype i ch |]

  let type_section ch =
    let n = uint ch in
    repeat n rectype ch

  let limits ch : Ast.Binary.limits =
    let kind = input_byte ch in
    assert (kind < 8);
    let address_type = if kind land 4 = 0 then `I32 else `I64 in
    let mi = uint64 ch in
    let ma = if kind land 1 = 0 then None else Some (uint64 ch) in
    { mi; ma; address_type }

  let memtype ch = limits ch

  let tabletype ch : Ast.Binary.tabletype =
    let reftype = reftype_first_byte ch in
    let limits = limits ch in
    { limits; reftype }

  let tabletype ch = tabletype ch
  let typeidx _ ch = uint ch

  let globaltype ch : Ast.Binary.globaltype =
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
    let importdesc : Ast.Binary.importdesc =
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
    let entry = { Ast.Binary.module_; name; desc = importdesc } in
    entry

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
    { Ast.Binary.align = Utils.Uint64.of_int a; offset = o }

  let expr ch =
    let int32 () = sint32 ch in
    let int64 () = sint64 ch in
    let float32 () = float32 ch in
    let float64 () = float64 ch in
    let heaptype () = heaptype ch in
    let reftype () = reftype_first_byte ch in
    let valtype () = valtype_first_byte ch in
    let blocktype () = blocktype ch in
    let memarg () = memarg ch in
    let rec instructions acc =
      if pos_in ch = ch.limit then List.rev acc
      else
        match peek_byte ch with
        | 0x0B | 0x05 -> List.rev acc
        | _ ->
            let i = instruction () in
            instructions (i :: acc)
    and instruction () =
      let op = input_byte ch in
      let desc =
        match op with
        | 0x00 -> Unreachable
        | 0x01 -> Nop
        | 0x02 ->
            let typ = blocktype () in
            let block = instructions [] in
            assert (input_byte ch = 0x0B);
            Block { label = (); typ; block }
        | 0x03 ->
            let typ = blocktype () in
            let block = instructions [] in
            assert (input_byte ch = 0x0B);
            Loop { label = (); typ; block }
        | 0x04 ->
            let typ = blocktype () in
            let if_block = instructions [] in
            let else_block =
              if input_byte ch = 0x05 then (
                let b = instructions [] in
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
        | 0x1C -> Select (Some (Array.to_list (vec (fun _ -> valtype ()) ch)))
        | 0x20 -> LocalGet (uint ch)
        | 0x21 -> LocalSet (uint ch)
        | 0x22 -> LocalTee (uint ch)
        | 0x23 -> GlobalGet (uint ch)
        | 0x24 -> GlobalSet (uint ch)
        | 0x25 -> TableGet (uint ch)
        | 0x26 -> TableSet (uint ch)
        | 0x28 -> Load (0, memarg (), I32 ())
        | 0x29 -> Load (0, memarg (), I64 ())
        | 0x2A -> Load (0, memarg (), F32 ())
        | 0x2B -> Load (0, memarg (), F64 ())
        | 0x2C -> LoadS (0, memarg (), `I32, `I8, Signed)
        | 0x2D -> LoadS (0, memarg (), `I32, `I8, Unsigned)
        | 0x2E -> LoadS (0, memarg (), `I32, `I16, Signed)
        | 0x2F -> LoadS (0, memarg (), `I32, `I16, Unsigned)
        | 0x30 -> LoadS (0, memarg (), `I64, `I8, Signed)
        | 0x31 -> LoadS (0, memarg (), `I64, `I8, Unsigned)
        | 0x32 -> LoadS (0, memarg (), `I64, `I16, Signed)
        | 0x33 -> LoadS (0, memarg (), `I64, `I16, Unsigned)
        | 0x34 -> LoadS (0, memarg (), `I64, `I32, Signed)
        | 0x35 -> LoadS (0, memarg (), `I64, `I32, Unsigned)
        | 0x36 -> Store (0, memarg (), I32 ())
        | 0x37 -> Store (0, memarg (), I64 ())
        | 0x38 -> Store (0, memarg (), F32 ())
        | 0x39 -> Store (0, memarg (), F64 ())
        | 0x3A -> StoreS (0, memarg (), `I32, `I8)
        | 0x3B -> StoreS (0, memarg (), `I32, `I16)
        | 0x3C -> StoreS (0, memarg (), `I64, `I8)
        | 0x3D -> StoreS (0, memarg (), `I64, `I16)
        | 0x3E -> StoreS (0, memarg (), `I64, `I32)
        | 0x3F ->
            assert (input_byte ch = 0);
            MemorySize 0
        | 0x40 ->
            assert (input_byte ch = 0);
            MemoryGrow 0
        | 0x41 -> Const (I32 (int32 ()))
        | 0x42 -> Const (I64 (int64 ()))
        | 0x43 -> Const (F32 (float32 ()))
        | 0x44 -> Const (F64 (float64 ()))
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
        | 0xD0 -> RefNull (heaptype ())
        | 0xD1 -> RefIsNull
        | 0xD2 -> RefFunc (uint ch)
        | 0xFB -> (
            match uint ch with
            | 0 -> StructNew (uint ch)
            | 1 -> StructNewDefault (uint ch)
            | 2 -> StructGet (None, uint ch, uint ch)
            | 3 -> StructGet (Some Signed, uint ch, uint ch)
            | 4 -> StructGet (Some Unsigned, uint ch, uint ch)
            | 5 -> StructSet (uint ch, uint ch)
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
            | 20 -> RefTest (nullable (heaptype ()))
            | 21 -> RefTest { nullable = false; typ = heaptype () }
            | 22 -> RefCast (nullable (heaptype ()))
            | 23 -> RefCast { nullable = false; typ = heaptype () }
            | 24 ->
                let a = uint ch in
                let b = reftype () in
                Br_on_cast (a, b, reftype ())
            | 25 ->
                let a = uint ch in
                let b = reftype () in
                Br_on_cast_fail (a, b, reftype ())
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
                let _ = assert (input_byte ch = 0) in
                MemoryInit (i, uint ch)
            | 9 -> DataDrop (uint ch)
            | 10 ->
                assert (input_byte ch = 0);
                assert (input_byte ch = 0);
                MemoryCopy (0, 0)
            | 11 ->
                assert (input_byte ch = 0);
                MemoryFill 0
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
        | c -> failwith (Printf.sprintf "Unknown opcode 0x%02X" c)
      in
      Ast.no_loc desc
    in
    instructions []

  let empty_names =
    let open Ast.Binary in
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

  let parse_module buf =
    check_header "input" buf;
    let ch = { buf; pos = 8; limit = String.length buf } in
    let st = { ch } in
    st.ch.pos <- 8;
    (* Reset position after index scan *)
    let rec loop m =
      match next_section st.ch with
      | None -> m
      | Some sect -> (
          match sect.id with
          | 1 ->
              (* Type section *)
              let types = Array.to_list (type_section st.ch) in
              loop { m with Ast.Binary.types }
          | 2 ->
              (* Import section *)
              let imports = Array.to_list (vec import st.ch) in
              loop { m with Ast.Binary.imports }
          | 3 ->
              (* Function section *)
              let functions = Array.to_list (vec (typeidx st) st.ch) in
              loop { m with Ast.Binary.functions }
          | 4 ->
              (* Table section *)
              let tables =
                Array.to_list
                  (vec
                     (fun ch ->
                       let next_byte = peek_byte ch in
                       if next_byte = 0x40 then (
                         (* Case 2: 0x40 0x00 tabletype expr *)
                         let marker = input_byte ch in
                         let attribute = input_byte ch in
                         assert (marker = 0x40);
                         assert (attribute = 0x00);
                         let typ = tabletype ch in
                         let expr = expr ch in
                         { Ast.Binary.typ; expr = Some expr })
                       else
                         (* Case 1: tabletype *)
                         let typ = tabletype ch in
                         { Ast.Binary.typ; expr = None })
                     st.ch)
              in
              loop { m with Ast.Binary.tables }
          | 5 ->
              (* Memory section *)
              let memories = Array.to_list (vec limits st.ch) in
              loop { m with Ast.Binary.memories }
          | 6 ->
              (* Global section *)
              let globals =
                Array.to_list
                  (vec
                     (fun ch ->
                       { Ast.Binary.typ = globaltype ch; init = expr ch })
                     st.ch)
              in
              loop { m with Ast.Binary.globals }
          | 7 ->
              (* Export section *)
              let exports = Array.to_list (vec export st.ch) in
              loop { m with Ast.Binary.exports }
          | 8 ->
              (* Start section *)
              let start = Some (uint st.ch) in
              loop { m with Ast.Binary.start }
          | 9 ->
              (* Element section *)
              let parse_elem ch =
                let mode_byte = uint ch in
                let typ =
                  match mode_byte with
                  | 0x00 | 0x04 ->
                      { nullable = false; typ = Func } (* funcref *)
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
                  | _ ->
                      failwith
                        (Printf.sprintf "Unknown elem mode 0x%02X" mode_byte)
                in
                let init = Array.to_list (vec expr ch) in
                { Ast.Binary.typ; init; mode }
              in
              let elem = Array.to_list (vec parse_elem st.ch) in
              loop { m with Ast.Binary.elem }
          | 10 ->
              (* Code section *)
              let parse_code ch =
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
                { Ast.Binary.locals; instrs }
              in
              let code = Array.to_list (vec parse_code st.ch) in
              loop { m with Ast.Binary.code }
          | 11 ->
              (* Data section *)
              let parse_data ch =
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
                  | _ ->
                      failwith
                        (Printf.sprintf "Unknown data mode 0x%02X" mode_byte)
                in
                let init_len = uint ch in
                let init_str = really_input_string ch init_len in
                { Ast.Binary.init = init_str; mode }
              in
              let data = Array.to_list (vec parse_data st.ch) in
              loop { m with Ast.Binary.data }
          | 12 ->
              (* DataCount section *)
              let _ = uint st.ch in
              loop m
          | 13 ->
              (* Tag section *)
              let parse_tag ch =
                let b = input_byte ch in
                assert (b = 0);
                typeidx st ch
              in
              let tags = Array.to_list (vec parse_tag st.ch) in
              loop { m with Ast.Binary.tags }
          | 0 -> (
              (* Custom section *)
              let custom_name = name st.ch in
              let start_pos = pos_in st.ch in
              match custom_name with
              | "name" ->
                  let rec parse_name_subsections current_names =
                    if pos_in st.ch = start_pos + sect.size then current_names
                    else
                      let subsection_id = uint st.ch in
                      let subsection_size = uint st.ch in
                      let subsection_start_pos = pos_in st.ch in
                      let updated_names =
                        match subsection_id with
                        | 0 ->
                            (* Module name *)
                            let module_name = name st.ch in
                            {
                              current_names with
                              Ast.Binary.module_ = Some module_name;
                            }
                        | 1 ->
                            (* Function names *)
                            let func_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              functions =
                                List.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty
                                  (Array.to_list func_names);
                            }
                        | 2 ->
                            (* Local names *)
                            let local_func_names_arr =
                              vec
                                (fun ch ->
                                  ( uint ch,
                                    vec (fun ch -> (uint ch, name ch)) ch ))
                                st.ch
                            in
                            let locals_map =
                              Array.fold_left
                                (fun acc (func_idx, local_names_arr) ->
                                  let inner_map =
                                    Array.fold_left
                                      (fun acc (idx, n) ->
                                        Ast.Binary.IntMap.add idx n acc)
                                      Ast.Binary.IntMap.empty local_names_arr
                                  in
                                  Ast.Binary.IntMap.add func_idx inner_map acc)
                                Ast.Binary.IntMap.empty local_func_names_arr
                            in
                            { current_names with locals = locals_map }
                        | 3 ->
                            (* Label names *)
                            let label_func_names_arr =
                              vec
                                (fun ch ->
                                  ( uint ch,
                                    vec (fun ch -> (uint ch, name ch)) ch ))
                                st.ch
                            in
                            let labels_map =
                              Array.fold_left
                                (fun acc (func_idx, label_names_arr) ->
                                  let inner_map =
                                    Array.fold_left
                                      (fun acc (idx, n) ->
                                        Ast.Binary.IntMap.add idx n acc)
                                      Ast.Binary.IntMap.empty label_names_arr
                                  in
                                  Ast.Binary.IntMap.add func_idx inner_map acc)
                                Ast.Binary.IntMap.empty label_func_names_arr
                            in
                            { current_names with labels = labels_map }
                        | 4 ->
                            (* Type names *)
                            let type_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              types =
                                Array.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty type_names;
                            }
                        | 5 ->
                            (* Table names *)
                            let table_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              tables =
                                Array.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty table_names;
                            }
                        | 6 ->
                            (* Memory names *)
                            let mem_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              memories =
                                Array.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty mem_names;
                            }
                        | 7 ->
                            (* Global names *)
                            let global_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              globals =
                                Array.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty global_names;
                            }
                        | 8 ->
                            (* Elem names *)
                            let elem_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              elem =
                                Array.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty elem_names;
                            }
                        | 9 ->
                            (* Data names *)
                            let data_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              data =
                                Array.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty data_names;
                            }
                        | 10 ->
                            (* Field names *)
                            let type_field_names_arr =
                              vec
                                (fun ch ->
                                  ( uint ch,
                                    vec (fun ch -> (uint ch, name ch)) ch ))
                                st.ch
                            in
                            let fields_map =
                              Array.fold_left
                                (fun acc (type_idx, field_names_arr) ->
                                  let inner_map =
                                    Array.fold_left
                                      (fun acc (idx, n) ->
                                        Ast.Binary.IntMap.add idx n acc)
                                      Ast.Binary.IntMap.empty field_names_arr
                                  in
                                  Ast.Binary.IntMap.add type_idx inner_map acc)
                                Ast.Binary.IntMap.empty type_field_names_arr
                            in
                            { current_names with fields = fields_map }
                        | 11 ->
                            (* Tag names *)
                            let tag_names =
                              vec (fun ch -> (uint ch, name ch)) st.ch
                            in
                            {
                              current_names with
                              tags =
                                Array.fold_left
                                  (fun acc (idx, n) ->
                                    Ast.Binary.IntMap.add idx n acc)
                                  Ast.Binary.IntMap.empty tag_names;
                            }
                        | _ -> current_names (* Skip unknown subsections *)
                      in
                      seek_in st.ch (subsection_start_pos + subsection_size);
                      parse_name_subsections updated_names
                  in
                  let names = parse_name_subsections m.names in
                  loop { m with Ast.Binary.names }
              | _ ->
                  (* Skip other custom sections *)
                  skip_section st.ch sect;
                  loop m)
          | _ ->
              skip_section st.ch sect;
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
end

let module_ = Read.parse_module
