(** [iter_instr f instr] applies the function [f] to the info field of [instr]
    and recursively to all nested instructions.
*)
val iter_instr : ('a -> unit) -> 'a Ast.Text.instr -> unit