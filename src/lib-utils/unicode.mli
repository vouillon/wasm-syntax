val terminal_width : ?offset:int -> string -> int
(** [terminal_width s] returns the width of the string [s] when displayed in a
    terminal.

    The optional argument [offset] (default 0) specifies the starting column
    position, which is used to correctly calculate the width of tab characters.
*)

val expand_tabs : ?offset:int -> string -> string
