type id = string
type idx = Num of Int32.t | Id of id

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
  | Struct of (id option * fieldtype) list
  | Array of fieldtype
  | Func of functype

type subtype = {
  name : id option;
  typ : comptype;
  supertype : idx option;
  final : bool;
}

type limits = { mi : Int32.t; ma : Int32.t option }
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
type memarg = { offset : Int32.t; align : Int32.t }

type instr =
  | Block of { label : id option; typ : blocktype option; block : instr list }
  | Loop of { label : id option; typ : blocktype option; block : instr list }
  | If of {
      label : id option;
      typ : blocktype option;
      if_block : instr list;
      else_block : instr list;
    }
  | Try of {
      label : id option;
      typ : blocktype option;
      block : instr list;
      catches : (idx * instr list) list;
      catch_all : instr list option;
    }
  | Unreachable
  | Nop
  | Throw of idx
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
  | I32Load8 of signage * memarg
  | I32Store8 of memarg
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
  | ArrayInitData of idx * idx
  | ArrayInitElem of idx * idx
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
  (* Binaryen extensions *)
  | Pop of valtype
  | TupleMake of Int32.t
  | TupleExtract of Int32.t * Int32.t

type expr = instr list

(* Modules *)

type importdesc =
  | Func of typeuse
  | Memory of limits
  | Global of globaltype
  | Tag of typeuse

type exportdesc = Func of idx | Memory of idx | Tag of idx | Global of idx
type datamode = Passive | Active of idx * expr

type modulefield =
  | Types of subtype list
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
  | Export of string * exportdesc
  | Start of idx
  | Elem of { id : id option; typ : reftype; init : expr list }
  | Data of { id : id option; init : string; mode : datamode }
