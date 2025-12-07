(** Wax Abstract Syntax Tree. *)

type ('desc, 'info) annotated = ('desc, 'info) Utils.Ast.annotated = {
  desc : 'desc;
  info : 'info;
}

type location = Utils.Ast.location

val no_loc : 'desc -> ('desc, location) annotated

type idx = (string, location) annotated

include module type of Wasm.Ast.Make_types (struct
  type nonrec idx = idx
  type 'a annotated_array = (idx * 'a) array
  type 'a opt_annotated_array = (idx option * 'a) array
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

type casttype =
  | Valtype of valtype
  | Signedtype of {
      typ : [ `I32 | `I64 | `F32 | `F64 ];
      signage : signage;
      strict : bool;
    }

val format_signed_type :
  [ `F32 | `F64 | `I32 | `I64 ] -> signage -> bool -> string
(** Helper to format signed types (e.g., "i32_s_strict"). *)

type 'info instr_desc =
  | Block of label option * functype * 'info instr list
  | Loop of label option * functype * 'info instr list
  | If of
      label option
      * functype
      * 'info instr
      * 'info instr list
      * 'info instr list option
  | Unreachable
  | Nop
  | Pop
  | Null
  | Get of idx
  | Set of idx option * 'info instr
  | Tee of idx * 'info instr
  | Call of 'info instr * 'info instr list
  | TailCall of 'info instr * 'info instr list
  | String of idx option * string
  | Int of string
  | Float of string
  | Cast of 'info instr * casttype
  | Test of 'info instr * reftype
  | NonNull of 'info instr
  | Struct of idx option * (idx * 'info instr) list
  | StructDefault of idx option
  | StructGet of 'info instr * idx
  | StructSet of 'info instr * idx * 'info instr
  | Array of idx option * 'info instr * 'info instr
  | ArrayDefault of idx option * 'info instr
  | ArrayFixed of idx option * 'info instr list
  | ArrayGet of 'info instr * 'info instr
  | ArraySet of 'info instr * 'info instr * 'info instr
  | BinOp of binop * 'info instr * 'info instr
  | UnOp of unop * 'info instr
  | Let of (idx option * valtype option) list * 'info instr option
  | Br of label * 'info instr option
  | Br_if of label * 'info instr
  | Br_table of label list * 'info instr
  | Br_on_null of label * 'info instr
  | Br_on_non_null of label * 'info instr
  | Br_on_cast of label * reftype * 'info instr
  | Br_on_cast_fail of label * reftype * 'info instr
  | Throw of idx * 'info instr list
  | ThrowRef of 'info instr
  | Return of 'info instr option
  | Sequence of 'info instr list
  | Select of 'info instr * 'info instr * 'info instr

and 'info instr = ('info instr_desc, 'info) annotated

type funsig = {
  named_params : (idx option * valtype) list;
  results : valtype list;
}

type attributes = (string * location instr) list

type 'info modulefield =
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
      body : string option * 'info instr list;
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
      def : 'info instr;
      attributes : attributes;
    }
  | Tag of {
      name : idx;
      typ : idx option;
      sign : funsig option;
      attributes : attributes;
    }
