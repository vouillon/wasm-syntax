(** Type handling for Wasm. *)

type t
(** A type context holding recursive type definitions. *)

val create : unit -> t

val add_rectype : t -> Ast.Binary.rectype -> int
(** Add a recursive type definition to the context. Returns the index of the
    first type defined. Assumes normalized types (use negative numbers for
    recursive references). *)

type subtyping_info
(** Information needed for subtyping checks. *)

val subtyping_info : t -> subtyping_info
val get_subtype : subtyping_info -> int -> Ast.Binary.subtype

val val_subtype :
  subtyping_info -> Ast.Binary.valtype -> Ast.Binary.valtype -> bool
