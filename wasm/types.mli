type t

val create : unit -> t

(*ZZZ Assumes normalized types
  (use negative numbers for recursive references)
*)

val add_rectype : t -> Ast.Binary.rectype -> int

type subtyping_info

val subtyping_info : t -> subtyping_info
val get_subtype : subtyping_info -> int -> Ast.Binary.subtype

val val_subtype :
  subtyping_info -> Ast.Binary.valtype -> Ast.Binary.valtype -> bool
