(** Type checking and validation for Wax modules. *)

exception Type_error of Ast.location * string
(** Exception raised when a type error is encountered. *)

val f : Ast.location Ast.modulefield list -> unit
(** [f fields] performs type checking on the given list of Wax module fields. It
    verifies types, signatures, and other semantic rules. Raises [Type_error] on
    failure. *)
