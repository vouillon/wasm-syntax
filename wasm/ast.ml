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
type valtype = I32 | I64 | F32 | F64 | V128 | Ref of reftype
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

type signage = Signed | Unsigned
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
  | Call of idx
  | CallRef of idx
  | CallIndirect of idx * typeuse
  | ReturnCall of idx
  | ReturnCallRef of idx
  | ReturnCallIndirect of idx * typeuse
  | LocalGet of idx
  | LocalSet of idx
  | LocalTee of idx
  | GlobalGet of idx
  | GlobalSet of idx
  | RefNull of heaptype
  | RefFunc of idx
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
  | RefI31
  | I31Get of signage
  | I32Const of Int32.t
  | I32Add
  | I32Sub
  | I32Mul
  | I32Div of signage
  | I32Rem of signage
  | I32And
  | I32Or
  | I32Xor
  | I32Shl
  | I32Shr of signage
  | I32Rotl
  | I32Rotr
  | I32Eqz
  | I32Eq
  | I32Ne
  | I32Lt of signage
  | I32Gt of signage
  | I32Le of signage
  | I32Ge of signage
  | Folded of instr * instr list
  | TupleMake of Int32.t

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
