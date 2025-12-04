open Ast
open Ast.Binary

module Encoder = struct
  let byte b i = Buffer.add_char b (Char.chr i)
  let string b s = Buffer.add_string b s

  let rec uint b i =
    if i < 128 then byte b i
    else (
      byte b (128 + (i land 127));
      uint b (i lsr 7))

  let rec sint b i =
    if i >= -64 && i < 64 then byte b (i land 127)
    else (
      byte b (128 + (i land 127));
      sint b (i asr 7))

  let sint32 b i =
    let rec aux i =
      if Int32.compare i (-64l) >= 0 && Int32.compare i 64l < 0 then
        byte b (Int32.to_int i land 0x7f)
      else (
        byte b (128 + (Int32.to_int i land 127));
        aux (Int32.shift_right i 7))
    in
    aux i

  let sint64 b i =
    let rec aux i =
      if Int64.compare i (-64L) >= 0 && Int64.compare i 64L < 0 then
        byte b (Int64.to_int i land 0x7f)
      else (
        byte b (128 + (Int64.to_int i land 127));
        aux (Int64.shift_right i 7))
    in
    aux i

  let f32 b f =
    let i = Int32.bits_of_float f in
    byte b (Int32.to_int i land 0xff);
    byte b (Int32.to_int (Int32.shift_right i 8) land 0xff);
    byte b (Int32.to_int (Int32.shift_right i 16) land 0xff);
    byte b (Int32.to_int (Int32.shift_right i 24) land 0xff)

  let f64 b f =
    let i = Int64.bits_of_float f in
    byte b (Int64.to_int i land 0xff);
    byte b (Int64.to_int (Int64.shift_right i 8) land 0xff);
    byte b (Int64.to_int (Int64.shift_right i 16) land 0xff);
    byte b (Int64.to_int (Int64.shift_right i 24) land 0xff);
    byte b (Int64.to_int (Int64.shift_right i 32) land 0xff);
    byte b (Int64.to_int (Int64.shift_right i 40) land 0xff);
    byte b (Int64.to_int (Int64.shift_right i 48) land 0xff);
    byte b (Int64.to_int (Int64.shift_right i 56) land 0xff)

  let name b n =
    uint b (String.length n);
    string b n

  let vec f b l =
    uint b (List.length l);
    List.iter (f b) l

  let heaptype b (t : heaptype) =
    match t with
    | Func -> byte b 0x70
    | Extern -> byte b 0x6F
    | Any -> byte b 0x6E
    | Eq -> byte b 0x6D
    | I31 -> byte b 0x6C
    | Struct -> byte b 0x6B
    | Array -> byte b 0x6A
    | None_ -> byte b 0x71
    | NoFunc -> byte b 0x73
    | NoExtern -> byte b 0x72
    | Exn -> byte b 0x69
    | NoExn -> byte b 0x74
    | Type idx -> sint b idx

  let reftype b (t : reftype) =
    if t.nullable then byte b 0x63 else byte b 0x64;
    heaptype b t.typ

  let valtype b (t : valtype) =
    match t with
    | I32 -> byte b 0x7F
    | I64 -> byte b 0x7E
    | F32 -> byte b 0x7D
    | F64 -> byte b 0x7C
    | V128 -> byte b 0x7B
    | Ref r -> reftype b r
    | Tuple _ -> failwith "Tuples not supported"

  let mut b m = byte b (if m then 0x01 else 0x00)

  let storagetype b s =
    match s with
    | Value v -> valtype b v
    | Packed I8 -> byte b 0x78
    | Packed I16 -> byte b 0x77

  let fieldtype b (f : fieldtype) =
    storagetype b f.typ;
    mut b f.mut

  let limits b (l : limits) =
    match l.ma with
    | None ->
        byte b 0x00;
        uint b (Utils.Uint64.to_int l.mi)
    | Some m ->
        byte b 0x01;
        uint b (Utils.Uint64.to_int l.mi);
        uint b (Utils.Uint64.to_int m)

  let globaltype b (t : globaltype) =
    valtype b t.typ;
    mut b t.mut

  let tabletype b (t : tabletype) =
    reftype b t.reftype;
    limits b t.limits

  let functype b (t : functype) =
    byte b 0x60;
    vec valtype b (Array.to_list t.params);
    vec valtype b (Array.to_list t.results)

  let comptype b (t : comptype) =
    match t with
    | Func f -> functype b f
    | Struct fields ->
        byte b 0x5F;
        vec fieldtype b (Array.to_list fields)
    | Array field ->
        byte b 0x5E;
        fieldtype b field

  let subtype b (t : subtype) =
    if t.final && t.supertype = None then comptype b t.typ
    else (
      byte b (if t.final then 0x4F else 0x50);
      vec uint b (match t.supertype with Some i -> [ i ] | None -> []);
      comptype b t.typ)

  let memarg b (m : memarg) =
    uint b (Utils.Uint64.to_int m.align);
    uint b (Utils.Uint64.to_int m.offset)

  let blocktype b (t : blocktype) =
    match t with Valtype v -> valtype b v | Typeuse i -> sint b i

  let rec instr ~source_map_t b (i : Ast.location instr) =
    let generated_offset = Buffer.length b in
    if
      i.info.Utils.Ast.loc_start.Lexing.pos_fname <> ""
      && i.info.Utils.Ast.loc_start.Lexing.pos_lnum <> -1
      && i.info.Utils.Ast.loc_start.Lexing.pos_cnum <> -1
    then
      Source_map.add_mapping source_map_t ~generated_offset
        ~original_location:i.info;

    match i.desc with
    | Unreachable -> byte b 0x00
    | Nop -> byte b 0x01
    | Block { typ; block; _ } ->
        byte b 0x02;
        (match typ with Some t -> blocktype b t | None -> byte b 0x40);
        List.iter (instr ~source_map_t b) block;
        byte b 0x0B
    | Loop { typ; block; _ } ->
        byte b 0x03;
        (match typ with Some t -> blocktype b t | None -> byte b 0x40);
        List.iter (instr ~source_map_t b) block;
        byte b 0x0B
    | If { typ; if_block; else_block; _ } ->
        byte b 0x04;
        (match typ with Some t -> blocktype b t | None -> byte b 0x40);
        List.iter (instr ~source_map_t b) if_block;
        if else_block <> [] then (
          byte b 0x05;
          List.iter (instr ~source_map_t b) else_block);
        byte b 0x0B
    | Br i ->
        byte b 0x0C;
        uint b i
    | Br_if i ->
        byte b 0x0D;
        uint b i
    | Br_table (ls, d) ->
        byte b 0x0E;
        vec uint b ls;
        uint b d
    | Return -> byte b 0x0F
    | Call i ->
        byte b 0x10;
        uint b i
    | CallIndirect (table, type_idx) ->
        byte b 0x11;
        uint b type_idx;
        uint b table
    | Drop -> byte b 0x1A
    | Select None -> byte b 0x1B
    | Select (Some types) ->
        byte b 0x1C;
        vec valtype b types
    | LocalGet i ->
        byte b 0x20;
        uint b i
    | LocalSet i ->
        byte b 0x21;
        uint b i
    | LocalTee i ->
        byte b 0x22;
        uint b i
    | GlobalGet i ->
        byte b 0x23;
        uint b i
    | GlobalSet i ->
        byte b 0x24;
        uint b i
    | Load (_mem_idx, m, op) ->
        (match op with
        | I32 () -> byte b 0x28
        | I64 () -> byte b 0x29
        | F32 () -> byte b 0x2A
        | F64 () -> byte b 0x2B);
        memarg b m
    | LoadS (_mem_idx, m, typ, sz, s) ->
        (match (typ, sz, s) with
        | `I32, `I8, Signed -> byte b 0x2C
        | `I32, `I8, Unsigned -> byte b 0x2D
        | `I32, `I16, Signed -> byte b 0x2E
        | `I32, `I16, Unsigned -> byte b 0x2F
        | `I64, `I8, Signed -> byte b 0x30
        | `I64, `I8, Unsigned -> byte b 0x31
        | `I64, `I16, Signed -> byte b 0x32
        | `I64, `I16, Unsigned -> byte b 0x33
        | `I64, `I32, Signed -> byte b 0x34
        | `I64, `I32, Unsigned -> byte b 0x35
        | _ -> failwith "Invalid LoadS combination");
        memarg b m
    | Store (_mem_idx, m, op) ->
        (match op with
        | I32 () -> byte b 0x36
        | I64 () -> byte b 0x37
        | F32 () -> byte b 0x38
        | F64 () -> byte b 0x39);
        memarg b m
    | StoreS (_mem_idx, m, typ, sz) ->
        (match (typ, sz) with
        | `I32, `I8 -> byte b 0x3A
        | `I32, `I16 -> byte b 0x3B
        | `I64, `I8 -> byte b 0x3C
        | `I64, `I16 -> byte b 0x3D
        | `I64, `I32 -> byte b 0x3E
        | _ -> failwith "Invalid StoreS combination");
        memarg b m
    | MemorySize i ->
        byte b 0x3F;
        byte b (if i = 0 then 0x00 else i)
    | MemoryGrow i ->
        byte b 0x40;
        byte b (if i = 0 then 0x00 else i)
    | Const (I32 i) ->
        byte b 0x41;
        sint32 b i
    | Const (I64 i) ->
        byte b 0x42;
        sint64 b i
    | Const (F32 f) ->
        byte b 0x43;
        f32 b f
    | Const (F64 f) ->
        byte b 0x44;
        f64 b f
    | UnOp op -> (
        match op with
        | I32 Clz -> byte b 0x67
        | I32 Ctz -> byte b 0x68
        | I32 Popcnt -> byte b 0x69
        | I64 Clz -> byte b 0x79
        | I64 Ctz -> byte b 0x7A
        | I64 Popcnt -> byte b 0x7B
        | F32 Abs -> byte b 0x8B
        | F32 Neg -> byte b 0x8C
        | F32 Ceil -> byte b 0x8D
        | F32 Floor -> byte b 0x8E
        | F32 Trunc -> byte b 0x8F
        | F32 Nearest -> byte b 0x90
        | F32 Sqrt -> byte b 0x91
        | F64 Abs -> byte b 0x99
        | F64 Neg -> byte b 0x9A
        | F64 Ceil -> byte b 0x9B
        | F64 Floor -> byte b 0x9C
        | F64 Trunc -> byte b 0x9D
        | F64 Nearest -> byte b 0x9E
        | F64 Sqrt -> byte b 0x9F
        | I32 Eqz -> byte b 0x45
        | I64 Eqz -> byte b 0x50
        | I32 (ExtendS `_8) -> byte b 0xC0
        | I32 (ExtendS `_16) -> byte b 0xC1
        | I64 (ExtendS `_8) -> byte b 0xC2
        | I64 (ExtendS `_16) -> byte b 0xC3
        | I64 (ExtendS `_32) -> byte b 0xC4
        | I32 (Trunc (`F32, Signed)) -> byte b 0xA8
        | I32 (Trunc (`F32, Unsigned)) -> byte b 0xA9
        | I32 (Trunc (`F64, Signed)) -> byte b 0xAA
        | I32 (Trunc (`F64, Unsigned)) -> byte b 0xAB
        | I64 (Trunc (`F32, Signed)) -> byte b 0xAE
        | I64 (Trunc (`F32, Unsigned)) -> byte b 0xAF
        | I64 (Trunc (`F64, Signed)) -> byte b 0xB0
        | I64 (Trunc (`F64, Unsigned)) -> byte b 0xB1
        | I32 (TruncSat (`F32, Signed)) ->
            byte b 0xFC;
            byte b 0x00
        | I32 (TruncSat (`F32, Unsigned)) ->
            byte b 0xFC;
            byte b 0x01
        | I32 (TruncSat (`F64, Signed)) ->
            byte b 0xFC;
            byte b 0x02
        | I32 (TruncSat (`F64, Unsigned)) ->
            byte b 0xFC;
            byte b 0x03
        | I64 (TruncSat (`F32, Signed)) ->
            byte b 0xFC;
            byte b 0x04
        | I64 (TruncSat (`F32, Unsigned)) ->
            byte b 0xFC;
            byte b 0x05
        | I64 (TruncSat (`F64, Signed)) ->
            byte b 0xFC;
            byte b 0x06
        | I64 (TruncSat (`F64, Unsigned)) ->
            byte b 0xFC;
            byte b 0x07
        | F32 (Convert (`I32, Signed)) -> byte b 0xB2
        | F32 (Convert (`I32, Unsigned)) -> byte b 0xB3
        | F32 (Convert (`I64, Signed)) -> byte b 0xB4
        | F32 (Convert (`I64, Unsigned)) -> byte b 0xB5
        | F64 (Convert (`I32, Signed)) -> byte b 0xB7
        | F64 (Convert (`I32, Unsigned)) -> byte b 0xB8
        | F64 (Convert (`I64, Signed)) -> byte b 0xB9
        | F64 (Convert (`I64, Unsigned)) -> byte b 0xBA
        | I32 Reinterpret -> byte b 0xBC
        | I64 Reinterpret -> byte b 0xBD
        | F32 Reinterpret -> byte b 0xBE
        | F64 Reinterpret -> byte b 0xBF
        | _ -> failwith "Unimplemented UnOp")
    | BinOp op -> (
        match op with
        | I32 Add -> byte b 0x6A
        | I32 Sub -> byte b 0x6B
        | I32 Mul -> byte b 0x6C
        | I32 (Div Signed) -> byte b 0x6D
        | I32 (Div Unsigned) -> byte b 0x6E
        | I32 (Rem Signed) -> byte b 0x6F
        | I32 (Rem Unsigned) -> byte b 0x70
        | I32 And -> byte b 0x71
        | I32 Or -> byte b 0x72
        | I32 Xor -> byte b 0x73
        | I32 Shl -> byte b 0x74
        | I32 (Shr Signed) -> byte b 0x75
        | I32 (Shr Unsigned) -> byte b 0x76
        | I32 Rotl -> byte b 0x77
        | I32 Rotr -> byte b 0x78
        | I64 Add -> byte b 0x7C
        | I64 Sub -> byte b 0x7D
        | I64 Mul -> byte b 0x7E
        | I64 (Div Signed) -> byte b 0x7F
        | I64 (Div Unsigned) -> byte b 0x80
        | I64 (Rem Signed) -> byte b 0x81
        | I64 (Rem Unsigned) -> byte b 0x82
        | I64 And -> byte b 0x83
        | I64 Or -> byte b 0x84
        | I64 Xor -> byte b 0x85
        | I64 Shl -> byte b 0x86
        | I64 (Shr Signed) -> byte b 0x87
        | I64 (Shr Unsigned) -> byte b 0x88
        | I64 Rotl -> byte b 0x89
        | I64 Rotr -> byte b 0x8A
        | F32 Add -> byte b 0x92
        | F32 Sub -> byte b 0x93
        | F32 Mul -> byte b 0x94
        | F32 Div -> byte b 0x95
        | F32 Min -> byte b 0x96
        | F32 Max -> byte b 0x97
        | F32 CopySign -> byte b 0x98
        | F64 Add -> byte b 0xA0
        | F64 Sub -> byte b 0xA1
        | F64 Mul -> byte b 0xA2
        | F64 Div -> byte b 0xA3
        | F64 Min -> byte b 0xA4
        | F64 Max -> byte b 0xA5
        | F64 CopySign -> byte b 0xA6
        | I32 Eq -> byte b 0x46
        | I32 Ne -> byte b 0x47
        | I32 (Lt Signed) -> byte b 0x48
        | I32 (Lt Unsigned) -> byte b 0x49
        | I32 (Gt Signed) -> byte b 0x4A
        | I32 (Gt Unsigned) -> byte b 0x4B
        | I32 (Le Signed) -> byte b 0x4C
        | I32 (Le Unsigned) -> byte b 0x4D
        | I32 (Ge Signed) -> byte b 0x4E
        | I32 (Ge Unsigned) -> byte b 0x4F
        | I64 Eq -> byte b 0x51
        | I64 Ne -> byte b 0x52
        | I64 (Lt Signed) -> byte b 0x53
        | I64 (Lt Unsigned) -> byte b 0x54
        | I64 (Gt Signed) -> byte b 0x55
        | I64 (Gt Unsigned) -> byte b 0x56
        | I64 (Le Signed) -> byte b 0x57
        | I64 (Le Unsigned) -> byte b 0x58
        | I64 (Ge Signed) -> byte b 0x59
        | I64 (Ge Unsigned) -> byte b 0x5A
        | F32 Eq -> byte b 0x5B
        | F32 Ne -> byte b 0x5C
        | F32 Lt -> byte b 0x5D
        | F32 Gt -> byte b 0x5E
        | F32 Le -> byte b 0x5F
        | F32 Ge -> byte b 0x60
        | F64 Eq -> byte b 0x61
        | F64 Ne -> byte b 0x62
        | F64 Lt -> byte b 0x63
        | F64 Gt -> byte b 0x64
        | F64 Le -> byte b 0x65
        | F64 Ge -> byte b 0x66)
    | I32WrapI64 -> byte b 0xA7
    | I64ExtendI32 Signed -> byte b 0xAC
    | I64ExtendI32 Unsigned -> byte b 0xAD
    | F32DemoteF64 -> byte b 0xB6
    | F64PromoteF32 -> byte b 0xBB
    | ExternConvertAny ->
        byte b 0xFB;
        byte b 0x1B
    | AnyConvertExtern ->
        byte b 0xFB;
        byte b 0x1A
    | RefNull t ->
        byte b 0xD0;
        heaptype b t
    | RefIsNull -> byte b 0xD1
    | RefFunc i ->
        byte b 0xD2;
        uint b i
    | RefEq -> byte b 0xD3
    | RefAsNonNull -> byte b 0xD4
    | RefTest t ->
        byte b 0xFB;
        byte b (if t.nullable then 0x15 else 0x14);
        heaptype b t.typ
    | RefCast t ->
        byte b 0xFB;
        byte b (if t.nullable then 0x17 else 0x16);
        heaptype b t.typ
    | StructNew i ->
        byte b 0xFB;
        byte b 0x00;
        uint b i
    | StructNewDefault i ->
        byte b 0xFB;
        byte b 0x01;
        uint b i
    | StructGet (s, type_idx, field_idx) ->
        byte b 0xFB;
        byte b
          (match s with
          | None -> 0x02
          | Some Signed -> 0x03
          | Some Unsigned -> 0x04);
        uint b type_idx;
        uint b field_idx
    | StructSet (type_idx, field_idx) ->
        byte b 0xFB;
        byte b 0x05;
        uint b type_idx;
        uint b field_idx
    | ArrayNew i ->
        byte b 0xFB;
        byte b 0x06;
        uint b i
    | ArrayNewDefault i ->
        byte b 0xFB;
        byte b 0x07;
        uint b i
    | ArrayNewFixed (i, len) ->
        byte b 0xFB;
        byte b 0x08;
        uint b i;
        uint b (Utils.Uint32.to_int len)
    | ArrayNewData (type_idx, data_idx) ->
        byte b 0xFB;
        byte b 0x09;
        uint b type_idx;
        uint b data_idx
    | ArrayNewElem (type_idx, elem_idx) ->
        byte b 0xFB;
        byte b 0x0A;
        uint b type_idx;
        uint b elem_idx
    | ArrayGet (s, type_idx) ->
        byte b 0xFB;
        byte b
          (match s with
          | None -> 0x0B
          | Some Signed -> 0x0C
          | Some Unsigned -> 0x0D);
        uint b type_idx
    | ArraySet type_idx ->
        byte b 0xFB;
        byte b 0x0E;
        uint b type_idx
    | ArrayLen ->
        byte b 0xFB;
        byte b 0x0F
    | ArrayFill type_idx ->
        byte b 0xFB;
        byte b 0x10;
        uint b type_idx
    | ArrayCopy (type_idx_dst, type_idx_src) ->
        byte b 0xFB;
        byte b 0x11;
        uint b type_idx_dst;
        uint b type_idx_src
    | ArrayInitData (type_idx, data_idx) ->
        byte b 0xFB;
        byte b 0x12;
        uint b type_idx;
        uint b data_idx
    | ArrayInitElem (type_idx, elem_idx) ->
        byte b 0xFB;
        byte b 0x13;
        uint b type_idx;
        uint b elem_idx
    | RefI31 ->
        byte b 0xFB;
        byte b 0x1C
    | I31Get Signed ->
        byte b 0xFB;
        byte b 0x1D
    | I31Get Unsigned ->
        byte b 0xFB;
        byte b 0x1E
    | _ -> failwith "Instruction not implemented in Wasm binary output"
end

let output_section ch id encoder data =
  let b = Buffer.create 1024 in
  encoder b data;
  Out_channel.output_byte ch id;
  let len = Buffer.length b in
  let rec output_uint i =
    if i < 128 then Out_channel.output_byte ch i
    else (
      Out_channel.output_byte ch (128 + (i land 127));
      output_uint (i lsr 7))
  in
  output_uint len;
  Buffer.output_buffer ch b

let module_ ?(color = Utils.Colors.Auto) ?(out_channel = stdout)
    ?opt_source_map_file (m : Ast.location module_) =
  let _ = color in
  Out_channel.output_string out_channel "\x00\x61\x73\x6D\x01\x00\x00\x00";

  let source_map_t = Source_map.create () in

  (* 1. Type Section *)
  if m.types <> [] then
    output_section out_channel 1
      (Encoder.vec (fun b t -> Encoder.vec Encoder.subtype b (Array.to_list t)))
      m.types;

  (* 2. Import Section *)
  if m.imports <> [] then
    output_section out_channel 2
      (Encoder.vec (fun b (i : import) ->
           Encoder.name b i.module_;
           Encoder.name b i.name;
           match i.desc with
           | Func i ->
               Encoder.byte b 0x00;
               Encoder.sint b i
           | Table t ->
               Encoder.byte b 0x01;
               Encoder.tabletype b t
           | Memory l ->
               Encoder.byte b 0x02;
               Encoder.limits b l
           | Global g ->
               Encoder.byte b 0x03;
               Encoder.globaltype b g
           | Tag t ->
               Encoder.byte b 0x04;
               Encoder.byte b 0x00;
               Encoder.sint b t))
      m.imports;

  (* 3. Function Section *)
  if m.functions <> [] then
    output_section out_channel 3 (Encoder.vec Encoder.sint) m.functions;

  (* 4. Table Section *)
  if m.tables <> [] then
    output_section out_channel 4
      (Encoder.vec (fun b (t : Ast.location table) -> Encoder.limits b t.typ))
      m.tables;

  (* 5. Memory Section *)
  if m.memories <> [] then
    output_section out_channel 5 (Encoder.vec Encoder.limits) m.memories;

  (* 6. Tag Section *)
  if m.tags <> [] then
    output_section out_channel 13
      (Encoder.vec (fun b i ->
           Encoder.byte b 0x00;
           Encoder.sint b i))
      m.tags;

  (* 7. Global Section *)
  if m.globals <> [] then
    output_section out_channel 6
      (Encoder.vec (fun b (g : Ast.location global) ->
           Encoder.globaltype b g.typ;
           List.iter (Encoder.instr ~source_map_t b) g.init;
           Encoder.byte b 0x0B))
      m.globals;

  (* 8. Export Section *)
  if m.exports <> [] then
    output_section out_channel 7
      (Encoder.vec (fun b (e : export) ->
           Encoder.name b e.name;
           (match e.kind with
           | Func -> Encoder.byte b 0x00
           | Table -> Encoder.byte b 0x01
           | Memory -> Encoder.byte b 0x02
           | Global -> Encoder.byte b 0x03
           | Tag -> Encoder.byte b 0x04);
           Encoder.sint b e.index))
      m.exports;

  (* 9. Start Section *)
  (match m.start with
  | Some i -> output_section out_channel 8 Encoder.sint i
  | None -> ());

  (* 10. Element Section *)
  if m.elem <> [] then
    output_section out_channel 9
      (Encoder.vec (fun b (e : Ast.location elem) ->
           match e.mode with
           | Active (table, offset) ->
               Encoder.byte b 0x06;
               Encoder.uint b table;
               List.iter (Encoder.instr ~source_map_t b) offset;
               Encoder.byte b 0x0B;
               Encoder.reftype b e.typ;
               Encoder.vec
                 (fun b ex ->
                   List.iter (Encoder.instr ~source_map_t b) ex;
                   Encoder.byte b 0x0B)
                 b e.init
           | Passive ->
               Encoder.byte b 0x05;
               Encoder.reftype b e.typ;
               Encoder.vec
                 (fun b ex ->
                   List.iter (Encoder.instr ~source_map_t b) ex;
                   Encoder.byte b 0x0B)
                 b e.init
           | Declare ->
               Encoder.byte b 0x07;
               Encoder.reftype b e.typ;
               Encoder.vec
                 (fun b ex ->
                   List.iter (Encoder.instr ~source_map_t b) ex;
                   Encoder.byte b 0x0B)
                 b e.init))
      m.elem;

  (* 12. Data Count Section *)
  if m.data <> [] then
    output_section out_channel 12 Encoder.uint (List.length m.data);

  (* 11. Code Section *)
  if m.code <> [] then
    output_section out_channel 10
      (Encoder.vec (fun b (c : Ast.location code) ->
           let b_code = Buffer.create 128 in
           Encoder.vec
             (fun b t ->
               Encoder.uint b 1;
               Encoder.valtype b t)
             b_code c.locals;
           List.iter (Encoder.instr ~source_map_t b_code) c.instrs;
           Encoder.byte b_code 0x0B;
           Encoder.uint b (Buffer.length b_code);
           Buffer.add_buffer b b_code))
      m.code;

  (* 12. Data Section *)
  if m.data <> [] then
    output_section out_channel 11
      (Encoder.vec (fun b (d : Ast.location data) ->
           match d.mode with
           | Passive ->
               Encoder.byte b 0x01;
               Encoder.name b d.init
           | Active (mem, offset) ->
               if mem = 0 then (
                 Encoder.byte b 0x00;
                 List.iter (Encoder.instr ~source_map_t b) offset;
                 Encoder.byte b 0x0B;
                 Encoder.name b d.init)
               else (
                 Encoder.byte b 0x02;
                 Encoder.uint b mem;
                 List.iter (Encoder.instr ~source_map_t b) offset;
                 Encoder.byte b 0x0B;
                 Encoder.name b d.init)))
      m.data;

  (* Custom Name Section *)
  let output_name_subsection id name_list b =
    if not (IntMap.is_empty name_list) then (
      Encoder.byte b id;
      let b_sub = Buffer.create 128 in
      Encoder.vec
        (fun b (idx, name) ->
          Encoder.uint b idx;
          Encoder.name b name)
        b_sub
        (IntMap.bindings name_list);
      Encoder.uint b (Buffer.length b_sub);
      Buffer.add_buffer b b_sub)
  in

  let output_indirect_name_subsection id name_list b =
    if not (IntMap.is_empty name_list) then (
      Encoder.byte b id;
      let b_sub = Buffer.create 128 in
      Encoder.vec
        (fun b (outer_idx, inner_map) ->
          Encoder.uint b outer_idx;
          Encoder.vec
            (fun b (inner_idx, name) ->
              Encoder.uint b inner_idx;
              Encoder.name b name)
            b
            (IntMap.bindings inner_map))
        b_sub
        (IntMap.bindings name_list);
      Encoder.uint b (Buffer.length b_sub);
      Buffer.add_buffer b b_sub)
  in

  let b_names = Buffer.create 1024 in
  (match m.names.module_ with
  | Some name ->
      Encoder.byte b_names 0x00;
      (* Module name subsection ID *)
      let b_sub = Buffer.create 64 in
      Encoder.name b_sub name;
      Encoder.uint b_names (Buffer.length b_sub);
      Buffer.add_buffer b_names b_sub
  | None -> ());

  output_name_subsection 0x01 m.names.functions b_names;
  (* Function names *)
  output_indirect_name_subsection 0x02 m.names.locals b_names;
  (* Local names *)
  output_indirect_name_subsection 0x03 m.names.labels b_names;
  (* Label names *)
  output_name_subsection 0x04 m.names.types b_names;
  (* Type names *)
  output_name_subsection 0x05 m.names.tables b_names;
  (* Table names *)
  output_name_subsection 0x06 m.names.memories b_names;
  (* Memory names *)
  output_name_subsection 0x07 m.names.globals b_names;
  (* Global names *)
  output_name_subsection 0x08 m.names.elem b_names;
  (* Elem names *)
  output_name_subsection 0x09 m.names.data b_names;

  (* Data names *)
  if Buffer.length b_names > 0 then (
    let b_custom_section_content = Buffer.create (Buffer.length b_names + 10) in
    Encoder.name b_custom_section_content "name";
    Buffer.add_buffer b_custom_section_content b_names;

    Out_channel.output_byte out_channel 0;
    (* Custom section ID (0) *)
    let len = Buffer.length b_custom_section_content in
    let rec output_uint i =
      if i < 128 then Out_channel.output_byte out_channel i
      else (
        Out_channel.output_byte out_channel (128 + (i land 127));
        output_uint (i lsr 7))
    in
    output_uint len;
    Buffer.output_buffer out_channel b_custom_section_content);

  (* Generate source map file *)
  match opt_source_map_file with
  | Some map_file_name ->
      let json_content =
        Source_map.to_json source_map_t ~file_name:map_file_name
      in
      Out_channel.with_open_text map_file_name (fun oc ->
          Out_channel.output_string oc json_content)
  | None -> ()
