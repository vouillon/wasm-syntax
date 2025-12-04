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

val get_all_rectypes : t -> Ast.Binary.rectype list
(** Returns all recursive type definitions from the context. *)

val heap_subtype : subtyping_info -> Ast.Binary.heaptype -> Ast.Binary.heaptype -> bool
val ref_subtype : subtyping_info -> Ast.Binary.reftype -> Ast.Binary.reftype -> bool
val val_subtype :
  subtyping_info -> Ast.Binary.valtype -> Ast.Binary.valtype -> bool
