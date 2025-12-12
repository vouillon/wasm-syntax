(** Pretty-printing for Wax. *)

val instr : Utils.Printer.t -> _ Ast.instr -> unit
val valtype : Utils.Printer.t -> Ast.valtype -> unit
val storagetype : Utils.Printer.t -> Ast.storagetype -> unit
val is_block : _ Ast.instr -> bool
val starts_with_block : _ Ast.instr -> bool

val module_ :
  ?color:Utils.Colors.flag ->
  ?out_channel:out_channel ->
  Utils.Printer.t ->
  'a Ast.module_ ->
  unit
