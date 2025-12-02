exception Type_error of Utils.Ast.location * string

val module_ :
  (Wax.Ast.storagetype list * Wax.Ast.location) Wax.Ast.modulefield list ->
  Wasm.Ast.Text.idx option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
