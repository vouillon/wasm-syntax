type t

val create : unit -> t

(*ZZZ Assumes normalized types
  (use negative numbers for recursive references)
*)

val add_rectype : t -> int Ast.rectype -> int

val val_subtype :
  int Ast.subtype array -> int Ast.valtype -> int Ast.valtype -> bool
