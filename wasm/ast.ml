type idx = Num of Int32.t | Id of string

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

type importdesc =
  | Func of string option * typeuse
  | Global of string option * globaltype

type instr =
  | StructNew of idx
  | ArrayNewFixed of idx * Int32.t
  | RefFunc of idx
  | RefNull of idx
  | I32Const of Int32.t
  | Folded of instr * instr list

type expr = instr list

type modulefield =
  | Types of subtype list
  | Import of { module_ : string; name : string; desc : importdesc }
  | Global of string option * globaltype * expr
