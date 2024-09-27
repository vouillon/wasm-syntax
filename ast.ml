type idx = string

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
  | Struct of (idx * fieldtype) list
  | Array of fieldtype
  | Func of functype

type subtype = {
  name : idx;
  typ : comptype;
  supertype : idx option;
  final : bool;
}

type binop = Plus | Minus | Gtu | Ltu | Or | And
type location = { loc_start : Lexing.position; loc_end : Lexing.position }
type 'a with_loc = { descr : 'a; loc : location }

type instr_descr =
  | Block of string option * instr list
  | Loop of string option * instr list
  | If of string option * instr * instr list * instr list option
  | Unreachable
  | Nop
  | Get of idx
  | Set of idx * instr
  | Call of idx * instr list
  | RefFunc of idx
  | Struct of string option * (string * instr) list
  | String of string
  | Int of string
  | Cast of instr * reftype
  | StructGet of instr * string
  | StructSet of instr * string * instr
  | BinOp of binop * instr * instr
  | Local of string * valtype option * instr option
  | Br of string * instr option
  | Br_if of string * instr
  | Br_table of string list * instr
  | Br_on_null of string * instr
  | Br_on_non_null of string * instr
  | Br_on_cast of string * reftype * instr
  | Br_on_cast_fail of string * reftype * instr
  | Return of instr option
  | Sequence of instr list

and instr = instr_descr with_loc

type funsig = {
  named_params : (string option * valtype) list;
  result : valtype list;
}

type modulefield =
  | Type of subtype list
  | Fundecl of { name : string; typ : string option; sign : funsig option }
  | Func of {
      name : string;
      typ : string option;
      sign : funsig option;
      body : string option * instr list;
    }
  | Global of { name : string; typ : valtype muttype option; def : instr }
