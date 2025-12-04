type ('desc, 'info) annotated = ('desc, 'info) Utils.Ast.annotated = {
  desc : 'desc;
  info : 'info;
}

type location = Utils.Ast.location = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
}

let no_loc = Utils.Ast.no_loc

module Uint32 = Utils.Uint32
module Uint64 = Utils.Uint64

(* Types *)

type packedtype = I8 | I16
type 'typ muttype = { mut : bool; typ : 'typ }

type limits = {
  mi : Uint64.t;
  ma : Uint64.t option;
  address_type : [ `I32 | `I64 ];
}

module Make_types (X : sig
  type idx
  type 'a annotated_array
end) =
struct
  type heaptype =
    | Func
    | NoFunc
    | Exn
    | NoExn
    | Extern
    | NoExtern
    | Any
    | Eq
    | I31
    | Struct
    | Array
    | None_
    | Type of X.idx

  type reftype = { nullable : bool; typ : heaptype }

  type valtype =
    | I32
    | I64
    | F32
    | F64
    | V128
    | Ref of reftype
    | Tuple of valtype list (* Tuples are not nested *)

  type functype = { params : valtype array; results : valtype array }
  type nonrec packedtype = packedtype = I8 | I16
  type storagetype = Value of valtype | Packed of packedtype
  type nonrec 'typ muttype = 'typ muttype = { mut : bool; typ : 'typ }
  type fieldtype = storagetype muttype

  type comptype =
    | Func of functype
    | Struct of fieldtype X.annotated_array
    | Array of fieldtype

  type subtype = { typ : comptype; supertype : X.idx option; final : bool }
  type rectype = subtype X.annotated_array

  type nonrec limits = limits = {
    mi : Uint64.t;
    ma : Uint64.t option;
    address_type : [ `I32 | `I64 ];
  }

  type globaltype = valtype muttype
  type tabletype = { limits : limits; reftype : reftype }
end

(* Instructions *)

type signage = Signed | Unsigned

type int_un_op =
  | Clz
  | Ctz
  | Popcnt
  | Eqz
  | Trunc of [ `F32 | `F64 ] * signage
  | TruncSat of [ `F32 | `F64 ] * signage
  | Reinterpret
  | ExtendS of [ `_8 | `_16 | `_32 ]

type int_bin_op =
  | Add
  | Sub
  | Mul
  | Div of signage
  | Rem of signage
  | And
  | Or
  | Xor
  | Shl
  | Shr of signage
  | Rotl
  | Rotr
  | Eq
  | Ne
  | Lt of signage
  | Gt of signage
  | Le of signage
  | Ge of signage

type float_un_op =
  | Neg
  | Abs
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Sqrt
  | Convert of [ `I32 | `I64 ] * signage
  | Reinterpret

type float_bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | CopySign
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

type ('i32, 'i64, 'f32, 'f64) op =
  | I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

type memarg = {
  offset : Uint64.t;
  align : Uint64.t (* The wasm test suite contains large align values *);
}

module Make_instructions (X : sig
  type idx
  type typeuse
  type label
  type heaptype
  type reftype
  type valtype
  type int32_t
  type int64_t
  type float_t
end) =
struct
  type nonrec ('i32, 'i64, 'f32, 'f64) op = ('i32, 'i64, 'f32, 'f64) op =
    | I32 of 'i32
    | I64 of 'i64
    | F32 of 'f32
    | F64 of 'f64

  type nonrec signage = signage = Signed | Unsigned

  type nonrec int_un_op = int_un_op =
    | Clz
    | Ctz
    | Popcnt
    | Eqz
    | Trunc of [ `F32 | `F64 ] * signage
    | TruncSat of [ `F32 | `F64 ] * signage
    | Reinterpret
    | ExtendS of [ `_8 | `_16 | `_32 ]

  type nonrec int_bin_op = int_bin_op =
    | Add
    | Sub
    | Mul
    | Div of signage
    | Rem of signage
    | And
    | Or
    | Xor
    | Shl
    | Shr of signage
    | Rotl
    | Rotr
    | Eq
    | Ne
    | Lt of signage
    | Gt of signage
    | Le of signage
    | Ge of signage

  type nonrec float_un_op = float_un_op =
    | Neg
    | Abs
    | Ceil
    | Floor
    | Trunc
    | Nearest
    | Sqrt
    | Convert of [ `I32 | `I64 ] * signage
    | Reinterpret

  type nonrec float_bin_op = float_bin_op =
    | Add
    | Sub
    | Mul
    | Div
    | Min
    | Max
    | CopySign
    | Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge

  type blocktype = Typeuse of X.typeuse | Valtype of X.valtype

  type nonrec memarg = memarg = {
    offset : Uint64.t;
    align : Uint64.t (* The wasm test suite contains large align values *);
  }

  type catch =
    | Catch of X.idx * X.idx
    | CatchRef of X.idx * X.idx
    | CatchAll of X.idx
    | CatchAllRef of X.idx

  type 'info instr_desc =
    | Block of {
        label : X.label;
        typ : blocktype option;
        block : 'info instr list;
      }
    | Loop of {
        label : X.label;
        typ : blocktype option;
        block : 'info instr list;
      }
    | If of {
        label : X.label;
        typ : blocktype option;
        if_block : 'info instr list;
        else_block : 'info instr list;
      }
    | TryTable of {
        label : X.label;
        typ : blocktype option;
        catches : catch list;
        block : 'info instr list;
      }
    | Try of {
        label : X.label;
        typ : blocktype option;
        block : 'info instr list;
        catches : (X.idx * 'info instr list) list;
        catch_all : 'info instr list option;
      }
    | Unreachable
    | Nop
    | Throw of X.idx
    | ThrowRef
    | Br of X.idx
    | Br_if of X.idx
    | Br_table of X.idx list * X.idx
    | Br_on_null of X.idx
    | Br_on_non_null of X.idx
    | Br_on_cast of X.idx * X.reftype * X.reftype
    | Br_on_cast_fail of X.idx * X.reftype * X.reftype
    | Return
    | Call of X.idx
    | CallRef of X.idx
    | CallIndirect of X.idx * X.typeuse
    | ReturnCall of X.idx
    | ReturnCallRef of X.idx
    | ReturnCallIndirect of X.idx * X.typeuse
    | Drop
    | Select of X.valtype list option
    | LocalGet of X.idx
    | LocalSet of X.idx
    | LocalTee of X.idx
    | GlobalGet of X.idx
    | GlobalSet of X.idx
    | Load of X.idx * memarg * (unit, unit, unit, unit) op
    | LoadS of
        X.idx * memarg * [ `I32 | `I64 ] * [ `I8 | `I16 | `I32 ] * signage
    | Store of X.idx * memarg * (unit, unit, unit, unit) op
    | StoreS of X.idx * memarg * [ `I32 | `I64 ] * [ `I8 | `I16 | `I32 ]
    | MemorySize of X.idx
    | MemoryGrow of X.idx
    | MemoryFill of X.idx
    | MemoryCopy of X.idx * X.idx
    | MemoryInit of X.idx * X.idx
    | DataDrop of X.idx
    | TableGet of X.idx
    | TableSet of X.idx
    | TableSize of X.idx
    | TableGrow of X.idx
    | TableFill of X.idx
    | TableCopy of X.idx * X.idx
    | TableInit of X.idx * X.idx
    | ElemDrop of X.idx
    | RefNull of X.heaptype
    | RefFunc of X.idx
    | RefIsNull
    | RefAsNonNull
    | RefEq
    | RefTest of X.reftype
    | RefCast of X.reftype
    | StructNew of X.idx
    | StructNewDefault of X.idx
    | StructGet of signage option * X.idx * X.idx
    | StructSet of X.idx * X.idx
    | ArrayNew of X.idx
    | ArrayNewDefault of X.idx
    | ArrayNewFixed of X.idx * Uint32.t
    | ArrayNewData of X.idx * X.idx
    | ArrayNewElem of X.idx * X.idx
    | ArrayGet of signage option * X.idx
    | ArraySet of X.idx
    | ArrayLen
    | ArrayFill of X.idx
    | ArrayCopy of X.idx * X.idx
    | ArrayInitData of X.idx * X.idx
    | ArrayInitElem of X.idx * X.idx
    | RefI31
    | I31Get of signage
    | Const of (X.int32_t, X.int64_t, X.float_t, X.float_t) op
    | UnOp of (int_un_op, int_un_op, float_un_op, float_un_op) op
    | BinOp of (int_bin_op, int_bin_op, float_bin_op, float_bin_op) op
    | I32WrapI64
    | I64ExtendI32 of signage
    | F32DemoteF64
    | F64PromoteF32
    | ExternConvertAny
    | AnyConvertExtern
    | Folded of 'info instr * 'info instr list
    (* Binaryen extensions *)
    | Pop of X.valtype
    | TupleMake of Uint32.t
    | TupleExtract of Uint32.t * Uint32.t

  and 'info instr = ('info instr_desc, 'info) annotated

  type 'info expr = 'info instr list
end

(* Modules *)

type exportable = Func | Memory | Table | Tag | Global

module Text = struct
  type id = string
  type idx_desc = Num of Uint32.t | Id of id
  type idx = (idx_desc, location) annotated

  module X = struct
    type nonrec idx = idx
    type 'a annotated_array = (id option * 'a) array
    type label = id option
  end

  module Types = Make_types (X)
  include Types

  type typeuse_no_bindings = idx option * functype option
  type typeuse = idx option * ((id option * valtype) list * valtype list) option

  include Make_instructions (struct
    include X
    include Types

    type nonrec typeuse = typeuse_no_bindings
    type int32_t = string
    type int64_t = string
    type float_t = string
  end)

  type importdesc =
    | Func of typeuse
    | Memory of limits
    | Table of tabletype
    | Global of globaltype
    | Tag of typeuse

  type nonrec exportable = exportable = Func | Memory | Table | Tag | Global
  type 'info datamode = Passive | Active of idx * 'info expr
  type 'info elemmode = Passive | Active of idx * 'info expr | Declare

  type 'info tableinit =
    | Init_default
    | Init_expr of 'info expr
    | Init_segment of 'info expr list

  type 'info modulefield =
    | Types of rectype
    | Import of {
        module_ : string;
        name : string;
        id : id option;
        desc : importdesc;
        exports : string list;
      }
    | Func of {
        id : id option;
        typ : typeuse;
        locals : (id option * valtype) list;
        instrs : 'info instr list;
        exports : string list;
      }
    | Memory of {
        id : id option;
        limits : limits;
        init : string option;
        exports : string list;
      }
    | Table of {
        id : id option;
        typ : tabletype;
        init : 'info tableinit;
        exports : string list;
      }
    | Tag of { id : id option; typ : typeuse; exports : string list }
    | Global of {
        id : id option;
        typ : globaltype;
        init : 'info expr;
        exports : string list;
      }
    | Export of { name : string; kind : exportable; index : idx }
    | Start of idx
    | Elem of {
        id : id option;
        typ : reftype;
        init : 'info expr list;
        mode : 'info elemmode;
      }
    | Data of { id : id option; init : string; mode : 'info datamode }

  type 'info module_ = string option * 'info modulefield list
end

module Binary = struct
  type id = unit
  type idx = int

  module X = struct
    type nonrec idx = idx
    type 'a annotated_array = 'a array
    type label = unit
  end

  module Types = Make_types (X)
  include Types

  type typeuse = idx

  include Make_instructions (struct
    include X
    include Types

    type nonrec typeuse = typeuse
    type int32_t = Int32.t
    type int64_t = Int64.t
    type float_t = float
  end)

  type nonrec exportable = exportable = Func | Memory | Table | Tag | Global
  type 'info datamode = Passive | Active of idx * 'info expr
  type 'info elemmode = Passive | Active of idx * 'info expr | Declare

  type importdesc =
    | Func of typeuse
    | Memory of limits
    | Table of tabletype
    | Global of globaltype
    | Tag of typeuse

  type import = { module_ : string; name : string; desc : importdesc }
  type 'info table = { typ : limits; expr : 'info expr option }

  type 'info memory = {
    id : id option;
    limits : limits;
    init : string option;
    exports : string list;
  }

  type tag = { typ : typeuse; exports : string list }
  type 'info global = { typ : globaltype; init : 'info expr }
  type export = { name : string; kind : exportable; index : idx }

  type 'info elem = {
    typ : reftype;
    init : 'info expr list;
    mode : 'info elemmode;
  }

  type 'info code = { locals : valtype list; instrs : 'info instr list }
  type 'info data = { init : string; mode : 'info datamode }

  module IntMap = Map.Make (Int)

  type name_map = string IntMap.t
  type indirect_name_map = string IntMap.t IntMap.t

  type names = {
    module_ : string option;
    functions : name_map;
    locals : indirect_name_map;
    labels : indirect_name_map;
    types : name_map;
    fields : indirect_name_map;
    tags : name_map;
    globals : name_map;
    tables : name_map;
    memories : name_map;
    data : name_map;
    elem : name_map;
  }

  type 'info module_ = {
    types : rectype list;
    imports : import list;
    functions : idx list;
    tables : 'info table list;
    memories : limits list;
    tags : idx list;
    globals : 'info global list;
    exports : export list;
    start : idx option;
    elem : 'info elem list;
    code : 'info code list;
    data : 'info data list;
    names : names;
  }
end
