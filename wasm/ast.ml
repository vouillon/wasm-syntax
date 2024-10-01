(* Types *)

type packedtype = I8 | I16
type 'typ muttype = { mut : bool; typ : 'typ }
type limits = { mi : Int32.t; ma : Int32.t option }

module Types (X : sig
  type idx
  type 'a annotated_array
end) =
struct
  type heaptype =
    | Func
    | NoFunc
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
    | Tuple of valtype list

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
  type nonrec limits = limits = { mi : Int32.t; ma : Int32.t option }
  type globaltype = valtype muttype
end

(* Instructions *)

type signage = Signed | Unsigned

module Instructions (X : sig
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
  type ('i32, 'i64, 'f32, 'f64) op =
    | I32 of 'i32
    | I64 of 'i64
    | F32 of 'f32
    | F64 of 'f64

  type nonrec signage = signage = Signed | Unsigned

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

  type blocktype = Idx of X.idx | ValType of X.valtype
  type memarg = { offset : Int32.t; align : Int32.t }

  type instr =
    | Block of { label : X.label; typ : blocktype option; block : instr list }
    | Loop of { label : X.label; typ : blocktype option; block : instr list }
    | If of {
        label : X.label;
        typ : blocktype option;
        if_block : instr list;
        else_block : instr list;
      }
    | Try of {
        label : X.label;
        typ : blocktype option;
        block : instr list;
        catches : (X.idx * instr list) list;
        catch_all : instr list option;
      }
    | Unreachable
    | Nop
    | Throw of X.idx
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
    | Select of X.valtype option
    | LocalGet of X.idx
    | LocalSet of X.idx
    | LocalTee of X.idx
    | GlobalGet of X.idx
    | GlobalSet of X.idx
    | I32Load8 of signage * memarg
    | I32Store8 of memarg
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
    | ArrayNewFixed of X.idx * Int32.t
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
    | Folded of instr * instr list
    (* Binaryen extensions *)
    | Pop of X.valtype
    | TupleMake of Int32.t
    | TupleExtract of Int32.t * Int32.t

  type expr = instr list
end

(* Modules *)

module Text = struct
  type id = string
  type idx = Num of Int32.t | Id of id

  module X = struct
    type nonrec idx = idx
    type 'a annotated_array = (id option * 'a) array
    type label = id option
  end

  module Types = Types (X)
  include Types

  type typeuse_no_bindings = idx option * functype option
  type typeuse = idx option * ((id option * valtype) list * valtype list) option

  include Instructions (struct
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
    | Global of globaltype
    | Tag of typeuse

  type exportable = Func | Memory | Tag | Global
  type datamode = Passive | Active of idx * expr

  type modulefield =
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
        instrs : instr list;
        exports : string list;
      }
    | Memory of {
        id : id option;
        limits : limits;
        init : string option;
        exports : string list;
      }
    | Tag of { id : id option; typ : typeuse; exports : string list }
    | Global of {
        id : id option;
        typ : globaltype;
        init : expr;
        exports : string list;
      }
    | Export of { name : string; kind : exportable; index : idx }
    | Start of idx
    | Elem of { id : id option; typ : reftype; init : expr list }
    | Data of { id : id option; init : string; mode : datamode }
end

module Binary = struct
  type id = unit
  type idx = int

  module X = struct
    type nonrec idx = idx
    type 'a annotated_array = 'a array
    type label = unit
  end

  module Types = Types (X)
  include Types

  type typeuse = idx

  include Instructions (struct
    include X
    include Types

    type nonrec typeuse = typeuse
    type int32_t = Int32.t
    type int64_t = Int32.t
    type float_t = float
  end)

  type importdesc =
    | Func of typeuse
    | Memory of limits
    | Global of globaltype
    | Tag of typeuse

  type exportable = Func | Memory | Tag | Global
  type datamode = Passive | Active of idx * expr

  type modulefield =
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
        instrs : instr list;
        exports : string list;
      }
    | Memory of {
        id : id option;
        limits : limits;
        init : string option;
        exports : string list;
      }
    | Tag of { id : id option; typ : typeuse; exports : string list }
    | Global of {
        id : id option;
        typ : globaltype;
        init : expr;
        exports : string list;
      }
    | Export of { name : string; kind : exportable; index : idx }
    | Start of idx
    | Elem of { id : id option; typ : reftype; init : expr list }
    | Data of { id : id option; init : string; mode : datamode }
end
