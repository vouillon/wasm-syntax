val map_instr : ('a -> 'b) -> 'a Ast.instr -> 'b Ast.instr
(** [map_instr f instr] applies the function [f] to the info field of [instr]
    and recursively applies it to all nested instructions. *)

val map_modulefield : ('a -> 'b) -> 'a Ast.modulefield -> 'b Ast.modulefield
(** [map_modulefield f modulefield] applies the function [f] to the info field
    of instructions within [modulefield] and returns a new [modulefield]. *)
