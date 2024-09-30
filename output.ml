(*
module Types (X : sig
  type idx
  type 'a annotated_array
end) =
struct

  type comptype =
    | Func of functype
    | Struct of fieldtype X.annotated_array
    | Array of fieldtype

  type subtype = { typ : comptype; supertype : X.idx option; final : bool }
  type rectype = subtype X.annotated_array
  type nonrec limits = limits = { mi : Int32.t; ma : Int32.t option }
  type globaltype = valtype muttype
end

type idx = string

include Wasm.Ast.Types (struct
  type nonrec idx = idx
  type 'a annotated_array = (string * 'a) array
end)

type signage = Wasm.Ast.signage = Signed | Unsigned
type binop = Add | Sub | Gt of signage | Lt of signage | Or | And | Ne | Eq
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
  | Tee of idx * instr
  | Call of instr * instr list
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
*)

open Ast

let heaptype f (t : heaptype) =
  match t with
  | Func -> Format.fprintf f "func"
  | NoFunc -> Format.fprintf f "nofunc"
  | Extern -> Format.fprintf f "extern"
  | NoExtern -> Format.fprintf f "noextern"
  | Any -> Format.fprintf f "any"
  | Eq -> Format.fprintf f "eq"
  | I31 -> Format.fprintf f "i31"
  | Struct -> Format.fprintf f "struct"
  | Array -> Format.fprintf f "array"
  | None_ -> Format.fprintf f "none"
  | Type s -> Format.fprintf f "%s" s

let reftype f { nullable; typ } =
  if nullable then Format.fprintf f "&?%a" heaptype typ
  else Format.fprintf f "&%a" heaptype typ

let rec valtype f t =
  match t with
  | I32 -> Format.fprintf f "i32"
  | I64 -> Format.fprintf f "i64"
  | F32 -> Format.fprintf f "f32"
  | F64 -> Format.fprintf f "f64"
  | V128 -> Format.fprintf f "v128"
  | Ref t -> reftype f t
  | Tuple l -> tuple f l

and tuple f l =
  match l with
  | [ t ] -> valtype f t
  | _ ->
      Format.fprintf f "@[<1>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           valtype)
        l

let functype f { params; results } =
  Format.fprintf f "@[<2>fn@ %a ->@ %a@]" tuple (Array.to_list params) tuple
    (Array.to_list results)

let packedtype f t =
  match t with I8 -> Format.fprintf f "i8" | I16 -> Format.fprintf f "i16"

let storagetype f t =
  match t with Value t -> valtype f t | Packed t -> packedtype f t

let muttype t f { mut; typ } =
  if mut then Format.fprintf f "@[<2>mut@ %a@]" t typ else t f typ

let fieldtype = muttype storagetype

let comptype f (t : comptype) =
  match t with
  | Func t -> functype f t
  | Struct l ->
      Format.fprintf f "{@[<1> @[<hv>%a@]@ @]}"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
           (fun f (nm, t) -> Format.fprintf f "@[<2>%s:@ %a@]" nm fieldtype t))
        (Array.to_list l)
  | Array t -> Format.fprintf f "@[<1>[%a]@]" fieldtype t

let subtype f (nm, { typ; supertype; final }) =
  Format.fprintf f "@[<2>type@ %s%s" nm (if final then "" else " open");
  (match supertype with
  | Some supertype -> Format.fprintf f "@ :@ %s" supertype
  | None -> ());
  Format.fprintf f "@ =@ %a@]" comptype typ

let rectype f t =
  match Array.to_list t with
  | [ t ] -> subtype f t
  | l ->
      Format.fprintf f "@[<2>rec {%a}@]"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f "@ ")
           subtype)
        l

let modulefield f field =
  match field with Type t -> rectype f t | _ -> Format.fprintf f "/*...*/"
(*
  | Fundecl of { name : string; typ : string option; sign : funsig option }
  | Func of {
      name : string;
      typ : string option;
      sign : funsig option;
      body : string option * instr list;
    }
  | Global of { name : string; typ : valtype muttype option; def : instr }
*)

let module_ f l =
  Format.fprintf f "@[<hv>%a@]@."
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@ ")
       modulefield)
    l
