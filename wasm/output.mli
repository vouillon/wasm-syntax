(** Pretty-printing for Wasm Text Format. *)

val escape_string : string -> int * string
(** [escape_string s] returns a pair [(len, escaped)] where [escaped] is the
    escaped version of [s] suitable for WAT string literals, and [len] is its
    display length. *)

val module_ : Utils.Printer.t -> _ Ast.Text.module_ -> unit
val instr : Utils.Printer.t -> _ Ast.Text.instr -> unit
