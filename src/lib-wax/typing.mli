(** Type checking and validation for Wax modules. *)

type typed_module_annotation = Ast.storagetype option array * Ast.location
type types

val f :
  Utils.Diagnostic.context ->
  Ast.location Ast.module_ ->
  types * typed_module_annotation Ast.module_
(** [f fields] performs type checking on the given list of Wax module fields. It
    verifies types, signatures, and other semantic rules. *)

val erase_types :
  typed_module_annotation Ast.module_ -> Ast.location Ast.module_

val get_type_definition :
  Utils.Diagnostic.context ->
  types ->
  (string, Ast.location) Ast.annotated ->
  Ast.subtype option
