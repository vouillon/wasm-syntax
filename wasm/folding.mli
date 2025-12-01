(** Folded/Unfolded instruction conversion. *)

val fold : 'info Ast.Text.module_ -> 'info Ast.Text.module_
(** [fold modul] converts unfolded instructions (like [i32.add]) into folded
    S-expressions (like [(i32.add ...)]) where possible in the module. *)

val unfold : 'info Ast.Text.module_ -> 'info Ast.Text.module_
(** [unfold modul] flattens folded S-expressions into linear instruction sequences
    in the module. *)