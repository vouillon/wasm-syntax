(** Parsing context for collecting comments and annotations. *)

type context

val make : unit -> context
(** Create a new comment context. *)

val report_comment :
  context -> Ast.location -> [ `Line | `Block ] -> string -> unit
(** [report_comment ctx loc kind content] reports a comment. *)

val report_annotation : context -> Ast.location -> unit
(** [report_annotation ctx loc] reports an annotation. *)

val report_newline : context -> Lexing.position -> unit
(** [report_newline ctx pos] reports a newline at [pos]. *)

val report_token : context -> unit
(** [report_token ctx] records that a meaningful token has been encountered on
    the current line. *)

val with_pos : context -> Ast.location -> 'a -> ('a, Ast.location) Ast.annotated
(** [with_pos ctx loc v] wraps [v] with location [loc]. *)
