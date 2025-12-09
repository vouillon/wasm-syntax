(** Type checking and validation for Wax modules. *)

type typed_module_annotation = Ast.storagetype array * Ast.location

val f :
  Utils.Diagnostic.context ->
  Ast.location Ast.modulefield list ->
  typed_module_annotation Ast.modulefield list
(** [f fields] performs type checking on the given list of Wax module fields. It
    verifies types, signatures, and other semantic rules. Raises [Type_error] on
    failure. *)
