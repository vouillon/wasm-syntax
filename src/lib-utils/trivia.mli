(** Parsing context for collecting comments and annotations. *)

type position = Line_start | Inline
type kind = Line_comment | Block_comment | Annotation
type t = Item of { content : string; kind : kind } | Blank_line
type entry = { anchor : int; trivia : t; position : position }
type context

type associated = {
  before : entry list;
  within : entry list;
  after : entry list;
}

val associate : context -> (Ast.location, associated) Hashtbl.t
(** [associate ctx] associates trivia to locations. *)

val make : unit -> context
(** Create a new trivia context. *)

val report_item : context -> kind -> string -> unit
(** [report_item ctx kind content] reports a comment or an annotation. *)

val report_newline : context -> unit
(** [report_newline ctx] reports a newline. *)

val report_token : context -> int -> unit
(** [report_token ctx pos] records that a meaningful token ending at byte [pos]
    has been encountered on the current line. *)

val with_pos : context -> Ast.location -> 'a -> ('a, Ast.location) Ast.annotated
(** [with_pos ctx loc v] wraps [v] with location [loc]. *)
