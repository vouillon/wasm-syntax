type idx = string

include Wasm.Ast.Types (struct
  type nonrec idx = idx
  type 'a annotated_array = (string * 'a) array
end)

type signage = Wasm.Ast.signage = Signed | Unsigned

type binop =
  | Add
  | Sub
  | Mul
  | Div of signage option
  | Rem of signage
  | And
  | Or
  | Xor
  | Shl
  | Shr of signage
  | Eq
  | Ne
  | Lt of signage option
  | Gt of signage option
  | Le of signage option
  | Ge of signage option

type location = { loc_start : Lexing.position; loc_end : Lexing.position }
type 'a with_loc = { descr : 'a; loc : location }

let no_loc descr =
  { descr; loc = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos } }

type label = string

type instr_descr =
  | Block of label option * instr list
  | Loop of label option * instr list
  | If of label option * instr * instr list * instr list option
  | Unreachable
  | Nop
  | Null
  | Get of idx
  | Set of idx * instr
  | Tee of idx * instr
  | Call of instr * instr list
  | String of idx option * string
  | Int of string
  | Float of string
  | Cast of instr * valtype
  | Test of instr * reftype
  | Struct of idx option * (string * instr) list
  | StructGet of instr * string
  | StructSet of instr * string * instr
  | Array of idx option * instr * instr
  | ArrayFixed of idx option * instr list
  | ArrayGet of instr * instr
  | ArraySet of instr * instr * instr
  | BinOp of binop * instr * instr
  | Let of (idx option * valtype option) list * instr option
  | Br of label * instr option
  | Br_if of label * instr
  | Br_table of label list * instr
  | Br_on_null of label * instr
  | Br_on_non_null of label * instr
  | Br_on_cast of label * reftype * instr
  | Br_on_cast_fail of label * reftype * instr
  | Return of instr option
  | Sequence of instr list

and instr = instr_descr with_loc

type funsig = {
  named_params : (string option * valtype) list;
  results : valtype list;
}

type attributes = (string * instr) list

type modulefield =
  | Type of rectype
  | Fundecl of {
      name : string;
      typ : string option;
      sign : funsig option;
      attributes : attributes;
    }
  | Func of {
      name : string;
      typ : string option;
      sign : funsig option;
      body : string option * instr list;
      attributes : attributes;
    }
  | Global of {
      name : string;
      typ : valtype muttype option;
      def : instr;
      attributes : attributes;
    }
