type t

val create : unit -> t

(*ZZZ Assumes normalized types
  (use negative numbers for recursive references)
*)

val add_rectype : t -> Ast.Binary.rectype -> int

val val_subtype :
  Ast.Binary.subtype array -> Ast.Binary.valtype -> Ast.Binary.valtype -> bool
