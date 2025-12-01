(** Generic AST definitions. *)

type ('desc, 'info) annotated = { desc : 'desc; info : 'info }
(** A value of type ['desc] annotated with extra information of type ['info]
    (e.g., source location). *)

type location = { loc_start : Lexing.position; loc_end : Lexing.position }
(** A source code location range. *)

val no_loc : 'desc -> ('desc, location) annotated
(** [no_loc v] wraps [v] with a dummy location. *)
