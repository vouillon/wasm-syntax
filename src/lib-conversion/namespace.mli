type t
(** A namespace manages a set of unique names, avoiding collisions with reserved
    keywords and previously allocated names. *)

val make : ?allow_keywords:bool -> unit -> t
(** Create a new namespace. If [allow_keywords] is true (default false),
    reserved keywords are not registered in the namespace and can be used as
    identifiers. Otherwise, reserved keywords ("if", "loop", "let", etc.) are
    registered and cannot be re-used. *)

val dup : t -> t
(** [dup t] returns a copy of the namespace [t]. Changes to the new namespace
    will not affect [t]. *)

val add : t -> string -> string
(** [add t name] registers [name] in the namespace [t]. If [name] is already
    taken or reserved, a unique suffix is appended (e.g., "name_1", "name_2").
    Returns the unique name that was actually registered. *)
