(** Pretty-printing for Wax. *)

val instr : Utils.Printer.t -> _ Ast.instr -> unit
val valtype : Utils.Printer.t -> Ast.valtype -> unit
val module_ : Utils.Printer.t -> _ Ast.modulefield list -> unit
