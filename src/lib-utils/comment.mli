(** Parsing context for collecting comments and annotations. *)

type comment =
  | Comment of {
      kind : [ `Line | `Block ];
      at_start_of_line : bool;
      content : string;
      prev_token_end : int;
    }
  | Annotation of { at_start_of_line : bool; prev_token_end : int }
  | BlankLine of { prev_token_end : int }

type context

type associated = {
  before : comment list;
  within : comment list;
  after : comment list;
}

val associate : context -> (Ast.location, associated) Hashtbl.t
(** [associate ctx] associates comments to locations. *)

val make : unit -> context
(** Create a new comment context. *)

val report_comment : context -> [ `Line | `Block ] -> string -> unit
(** [report_comment ctx kind content] reports a comment. *)

val report_annotation : context -> unit
(** [report_annotation ctx] reports an annotation. *)

val report_newline : context -> unit
(** [report_newline ctx] reports a newline. *)

val report_token : context -> int -> unit
(** [report_token ctx pos] records that a meaningful token ending at byte [pos]
    has been encountered on the current line. *)

val with_pos : context -> Ast.location -> 'a -> ('a, Ast.location) Ast.annotated
(** [with_pos ctx loc v] wraps [v] with location [loc]. *)
