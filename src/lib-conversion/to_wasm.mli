exception Type_error of Utils.Ast.location * string

val module_ :
  (Wax.Ast.storagetype array * Wax.Ast.location) Wax.Ast.modulefield list ->
  string option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
