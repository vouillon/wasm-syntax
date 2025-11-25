type location = { loc_start : Lexing.position; loc_end : Lexing.position }
type 'a with_loc = { descr : 'a; loc : location }

let no_loc descr =
  { descr; loc = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos } }

type idx = string with_loc

include Wasm.Ast.Types (struct
  type nonrec idx = idx
  type 'a annotated_array = (idx * 'a) array
end)

type signage = Wasm.Ast.signage = Signed | Unsigned
type unop = Neg | Pos | Not

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

type label = string

type instr_descr =
  | Block of label option * functype * instr list
  | Loop of label option * functype * instr list
  | If of label option * functype * instr * instr list * instr list option
  | Unreachable
  | Nop
  | Null
  | Get of idx
  | Set of idx * instr
  | Tee of idx * instr
  | Call of instr * instr list
  | TailCall of instr * instr list
  | String of idx option * string
  | Int of string
  | Float of string
  | Cast of instr * valtype
  | Test of instr * reftype
  | NonNull of instr
  | Struct of idx option * (idx * instr) list
  | StructDefault of idx option
  | StructGet of instr * idx
  | StructSet of instr * idx * instr
  | Array of idx option * instr * instr
  | ArrayDefault of idx option * instr
  | ArrayFixed of idx option * instr list
  | ArrayGet of instr * instr
  | ArraySet of instr * instr * instr
  | BinOp of binop * instr * instr
  | UnOp of unop * instr
  | Let of (idx option * valtype option) list * instr option
  | Br of label * instr option
  | Br_if of label * instr
  | Br_table of label list * instr
  | Br_on_null of label * instr
  | Br_on_non_null of label * instr
  | Br_on_cast of label * reftype * instr
  | Br_on_cast_fail of label * reftype * instr
  | Throw of idx * instr list
  | Return of instr option
  | Sequence of instr list
  | Select of instr * instr * instr

and instr = instr_descr with_loc

type funsig = {
  named_params : (idx option * valtype) list;
  results : valtype list;
}

type attributes = (string * instr) list

type modulefield =
  | Type of rectype
  | Fundecl of {
      name : idx;
      typ : idx option;
      sign : funsig option;
      attributes : attributes;
    }
  | Func of {
      name : idx;
      typ : idx option;
      sign : funsig option;
      body : string option * instr list;
      attributes : attributes;
    }
  | GlobalDecl of {
      name : idx;
      mut : bool;
      typ : valtype;
      attributes : attributes;
    }
  | Global of {
      name : idx;
      mut : bool;
      typ : valtype option;
      def : instr;
      attributes : attributes;
    }
  | Tag of {
      name : idx;
      typ : idx option;
      sign : funsig option;
      attributes : attributes;
    }
