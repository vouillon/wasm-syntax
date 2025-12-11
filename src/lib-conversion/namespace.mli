type t
(** A namespace manages a set of unique names, avoiding collisions with reserved
    keywords and previously allocated names. *)

val make : unit -> t
(** Create a new namespace initialized with the set of reserved keywords (e.g.,
    "if", "loop", "let", etc.). *)

val dup : t -> t
(** [dup t] returns a copy of the namespace [t]. Changes to the new namespace
    will not affect [t]. *)

val add : t -> string -> string
(** [add t name] registers [name] in the namespace [t]. If [name] is already
    taken or reserved, a unique suffix is appended (e.g., "name_1", "name_2").
    Returns the unique name that was actually registered. *)
