type id = string
type idx = Num of Int32.t | Id of id

(* Types *)
type 'idx heaptype =
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
  | Type of 'idx

type 'idx reftype = { nullable : bool; typ : 'idx heaptype }

type 'idx valtype =
  | I32
  | I64
  | F32
  | F64
  | V128
  | Ref of 'idx reftype
  | Tuple of 'idx valtype list

type 'idx functype = {
  params : 'idx valtype array;
  results : 'idx valtype array;
}

type packedtype = I8 | I16
type 'idx storagetype = Value of 'idx valtype | Packed of packedtype
type 'typ muttype = { mut : bool; typ : 'typ }
type 'idx fieldtype = 'idx storagetype muttype

type 'idx comptype =
  | Func of 'idx functype
  | Struct of (id option * 'idx fieldtype) array
  | Array of 'idx fieldtype

type 'idx subtype = {
  name : id option;
  typ : 'idx comptype;
  supertype : 'idx option;
  final : bool;
}

type 'idx rectype = 'idx subtype array
type limits = { mi : Int32.t; ma : Int32.t option }
type 'idx globaltype = 'idx valtype muttype
type typeuse = idx option * idx functype option

(* Instructions *)

type ('i32, 'i64, 'f32, 'f64) op =
  | I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

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

type 'idx blocktype = Idx of 'idx | ValType of 'idx valtype
type memarg = { offset : Int32.t; align : Int32.t }

type ('idx, 'typeuse) instr =
  | Block of {
      label : id option;
      typ : 'idx blocktype option;
      block : ('idx, 'typeuse) instr list;
    }
  | Loop of {
      label : id option;
      typ : 'idx blocktype option;
      block : ('idx, 'typeuse) instr list;
    }
  | If of {
      label : id option;
      typ : 'idx blocktype option;
      if_block : ('idx, 'typeuse) instr list;
      else_block : ('idx, 'typeuse) instr list;
    }
  | Try of {
      label : id option;
      typ : 'idx blocktype option;
      block : ('idx, 'typeuse) instr list;
      catches : (idx * ('idx, 'typeuse) instr list) list;
      catch_all : ('idx, 'typeuse) instr list option;
    }
  | Unreachable
  | Nop
  | Throw of 'idx
  | Br of 'idx
  | Br_if of 'idx
  | Br_table of 'idx list * 'idx
  | Br_on_null of 'idx
  | Br_on_non_null of 'idx
  | Br_on_cast of 'idx * 'idx reftype * 'idx reftype
  | Br_on_cast_fail of 'idx * 'idx reftype * 'idx reftype
  | Return
  | Call of 'idx
  | CallRef of 'idx
  | CallIndirect of 'idx * 'typeuse
  | ReturnCall of 'idx
  | ReturnCallRef of 'idx
  | ReturnCallIndirect of 'idx * 'typeuse
  | Drop
  | Select of 'idx valtype option
  | LocalGet of 'idx
  | LocalSet of 'idx
  | LocalTee of 'idx
  | GlobalGet of 'idx
  | GlobalSet of 'idx
  | I32Load8 of signage * memarg
  | I32Store8 of memarg
  | RefNull of 'idx heaptype
  | RefFunc of 'idx
  | RefIsNull
  | RefAsNonNull
  | RefEq
  | RefTest of 'idx reftype
  | RefCast of 'idx reftype
  | StructNew of 'idx
  | StructNewDefault of 'idx
  | StructGet of signage option * 'idx * 'idx
  | StructSet of 'idx * 'idx
  | ArrayNew of 'idx
  | ArrayNewDefault of 'idx
  | ArrayNewFixed of 'idx * Int32.t
  | ArrayNewData of 'idx * 'idx
  | ArrayNewElem of 'idx * 'idx
  | ArrayGet of signage option * 'idx
  | ArraySet of 'idx
  | ArrayLen
  | ArrayFill of 'idx
  | ArrayCopy of 'idx * 'idx
  | ArrayInitData of 'idx * 'idx
  | ArrayInitElem of 'idx * 'idx
  | RefI31
  | I31Get of signage
  | Const of (Int32.t, Int64.t, string, string) op
  | UnOp of (int_un_op, int_un_op, float_un_op, float_un_op) op
  | BinOp of (int_bin_op, int_bin_op, float_bin_op, float_bin_op) op
  | I32WrapI64
  | I64ExtendI32 of signage
  | F32DemoteF64
  | F64PromoteF32
  | ExternConvertAny
  | AnyConvertExtern
  | Folded of ('idx, 'typeuse) instr * ('idx, 'typeuse) instr list
  (* Binaryen extensions *)
  | Pop of 'idx valtype
  | TupleMake of Int32.t
  | TupleExtract of Int32.t * Int32.t

type ('idx, 'typeuse) expr = ('idx, 'typeuse) instr list

(* Modules *)

type ('idx, 'typeuse) importdesc =
  | Func of 'typeuse
  | Memory of limits
  | Global of 'idx globaltype
  | Tag of 'typeuse

type exportable = Func | Memory | Tag | Global

type ('idx, 'typeuse) datamode =
  | Passive
  | Active of 'idx * ('idx, 'typeuse) expr

type modulefield =
  | Types of idx subtype array
  | Import of {
      module_ : string;
      name : string;
      id : id option;
      desc : (idx, typeuse) importdesc;
      exports : string list;
    }
  | Func of {
      id : id option;
      typ : typeuse;
      locals : (id option * idx valtype) list;
      instrs : (idx, typeuse) instr list;
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
      typ : idx globaltype;
      init : (idx, typeuse) expr;
      exports : string list;
    }
  | Export of { name : string; kind : exportable; index : idx }
  | Start of idx
  | Elem of {
      id : id option;
      typ : idx reftype;
      init : (idx, typeuse) expr list;
    }
  | Data of { id : id option; init : string; mode : (idx, typeuse) datamode }
