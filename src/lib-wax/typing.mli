(** Type checking and validation for Wax modules. *)

type typed_module_annotation = Ast.storagetype option array * Ast.location
type module_context

val f :
  Utils.Diagnostic.context ->
  Ast.location Ast.module_ ->
  typed_module_annotation Ast.module_
(** [f fields] performs type checking on the given list of Wax module fields. It
    verifies types, signatures, and other semantic rules. Raises [Type_error] on
    failure. *)

val erase_types :
  typed_module_annotation Ast.module_ -> Ast.location Ast.module_
