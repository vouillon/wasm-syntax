type idx = Num of Int32.t | Id of string

(* Types *)
type heaptype =
  | Func_
  | NoFunc
  | Extern
  | NoExtern
  | Any
  | Eq
  | I31
  | Struct
  | Array
  | None_
  | Type of idx

type reftype = { nullable : bool; typ : heaptype }

type valtype =
  | I32
  | I64
  | F32
  | F64
  | V128
  | Ref of reftype
  | Tuple of valtype list

type functype = { params : valtype list; result : valtype list }
type packedtype = I8 | I16
type storagetype = Value of valtype | Packed of packedtype
type 'typ muttype = { mut : bool; typ : 'typ }
type fieldtype = storagetype muttype

type comptype =
  | Struct of (string option * fieldtype) list
  | Array of fieldtype
  | Func of functype

type subtype = {
  name : string option;
  typ : comptype;
  supertype : idx option;
  final : bool;
}

type globaltype = valtype muttype
type typeuse = idx option * functype option

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

type blocktype = Idx of idx | ValType of valtype

type instr =
  | Block of {
      label : string option;
      typ : blocktype option;
      block : instr list;
    }
  | Loop of {
      label : string option;
      typ : blocktype option;
      block : instr list;
    }
  | If of {
      label : string option;
      typ : blocktype option;
      if_block : instr list;
      else_block : instr list;
    }
  | Unreachable
  | Nop
  | Br of idx
  | Br_if of idx
  | Br_table of idx list * idx
  | Br_on_null of idx
  | Br_on_non_null of idx
  | Br_on_cast of idx * reftype * reftype
  | Br_on_cast_fail of idx * reftype * reftype
  | Return
  | Call of idx
  | CallRef of idx
  | CallIndirect of idx * typeuse
  | ReturnCall of idx
  | ReturnCallRef of idx
  | ReturnCallIndirect of idx * typeuse
  | Drop
  | Select of valtype option
  | LocalGet of idx
  | LocalSet of idx
  | LocalTee of idx
  | GlobalGet of idx
  | GlobalSet of idx
  | RefNull of heaptype
  | RefFunc of idx
  | RefIsNull
  | RefAsNonNull
  | RefEq
  | RefTest of reftype
  | RefCast of reftype
  | StructNew of idx
  | StructNewDefault of idx
  | StructGet of signage option * idx * idx
  | StructSet of idx * idx
  | ArrayNew of idx
  | ArrayNewDefault of idx
  | ArrayNewFixed of idx * Int32.t
  | ArrayNewData of idx * idx
  | ArrayNewElem of idx * idx
  | ArrayGet of signage option * idx
  | ArraySet of idx
  | ArrayLen
  | ArrayFill of idx
  | ArrayCopy of idx * idx
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
  | Folded of instr * instr list
  | TupleMake of Int32.t
  | TupleExtract of Int32.t * Int32.t

type expr = instr list

(* Modules *)

type importdesc =
  | Func of string option * typeuse
  | Global of string option * globaltype

type exportdesc = Func of idx | Global of idx
type datamode = Passive | Active of idx * expr

type modulefield =
  | Types of subtype list
  | Import of { module_ : string; name : string; desc : importdesc }
  | Global of string option * globaltype * expr
  | Func of {
      id : string option;
      typ : typeuse;
      locals : (string option * valtype) list;
      instrs : instr list;
    }
  | Export of string * exportdesc
  | Data of { id : string option; init : string; mode : datamode }
  | Start of idx
