type idx = string

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
  | Type of idx

type reftype = { nullable : bool; typ : heaptype }
type valtype = I32 | I64 | F32 | F64 | V128 | Ref of reftype
type functype = { params : valtype list; result : valtype list }
type packedtype = I8 | I16
type storagetype = Value of valtype | Packed of packedtype
type 'typ muttype = { mut : bool; typ : 'typ }
type fieldtype = storagetype muttype

type comptype =
  | Struct of (idx * fieldtype) list
  | Array of fieldtype
  | Func of functype

type subtype = {
  name : idx;
  typ : comptype;
  supertype : idx option;
  final : bool;
}

type binop = Plus | Minus | Gtu | Ltu

type instr =
  | Block of string option * instr list
  | Loop of string option * instr list
  | If of string option * instr list * instr list * instr list option
  | Unreachable
  | Nop
  | Get of idx
  | Set of idx * instr list
  | Call of idx * instr list list
  | RefFunc of idx
  | Struct of string * (string * instr list) list
  | String of string
  | Int of string
  | Cast of instr list * reftype
  | StructGet of instr list * string
  | StructSet of instr list * string * instr list
  | BinOp of binop * instr list * instr list
  | Local of string * valtype option * instr list option

type modulefield =
  | Type of subtype list
  | Fundecl of {
      name : string;
      typ : string option;
      params : (string option * valtype) list;
      result : valtype list;
    }
  | Func of {
      name : string;
      typ : string option;
      params : (string option * valtype) list;
      result : valtype list;
      body : string option * instr list;
    }
  | Global of { name : string; typ : valtype option; def : instr list }
