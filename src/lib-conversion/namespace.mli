type t
(** A namespace manages a set of unique names, avoiding collisions with reserved
    keywords and previously allocated names. *)

val make : ?kind:[ `Regular | `Label | `Type ] -> unit -> t
(** Create a new namespace. [kind] determines the set of reserved words (default
    [`Regular]):
    - [`Regular]: Standard reserved keywords (e.g., "if", "loop").
    - [`Label]: No reserved words (empty).
    - [`Type]: Reserved keywords plus abstract heap types (e.g., "func", "any").
*)

val dup : t -> t
(** [dup t] returns a copy of the namespace [t]. Changes to the new namespace
    will not affect [t]. *)

val add : t -> string -> string
(** [add t name] registers [name] in the namespace [t]. If [name] is already
    taken or reserved, a unique suffix is appended (e.g., "name_1", "name_2").
    Returns the unique name that was actually registered. *)

val reserve : t -> string -> unit
(** [reserve t name] reserves [name] in the namespace [t]. If [name] is already
    taken or reserved, nothing happens. If it is free, it is marked as taken.
    This is useful to prevent subsequent [add] calls from generating this name.
*)
