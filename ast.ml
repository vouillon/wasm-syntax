type idx = string

include Wasm.Ast.Types (struct
  type nonrec idx = idx
  type 'a annotated_array = (string * 'a) array
end)

type binop = Plus | Minus | Gtu | Ltu | Or | And
type location = { loc_start : Lexing.position; loc_end : Lexing.position }
type 'a with_loc = { descr : 'a; loc : location }

let no_loc descr =
  { descr; loc = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos } }

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
  | Test of instr * reftype
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
  | Type of rectype
  | Fundecl of { name : string; typ : string option; sign : funsig option }
  | Func of {
      name : string;
      typ : string option;
      sign : funsig option;
      body : string option * instr list;
    }
  | Global of { name : string; typ : valtype muttype option; def : instr }
