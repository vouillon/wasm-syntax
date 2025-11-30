module Comments : sig
  type t
  (** A mutable collection of comments and blank lines. *)

  val create : unit -> t
  (** Creates a new empty collection. *)

  val add_line_comment : t -> string -> Ast.location -> unit
  (** Adds a line comment to the collection. *)

  val add_block_comment : t -> string -> Ast.location -> unit
  (** Adds a block comment to the collection. *)

  val add_newline : t -> Ast.location -> unit
  (** Adds a newline (potential blank line) to the collection. *)

  val visit_token : t -> Ast.location -> unit
  (** Updates the internal state (e.g. last line number) based on a visited
      normal token. *)
end

type t
(** The state of the pretty-printer, managing indentation, pending
    spaces/newlines, and attached comments/blank lines. *)

val create : Comments.t -> Format.formatter -> t
(** Creates a new printer context. *)

val finish : t -> unit
(** Finalizes the printing process, flushing any remaining output. *)

val set_indent : t -> int -> unit
(** Sets the current indentation level. *)

val string : t -> string -> unit
(** Prints a string to the formatter, flushing any pending items first. *)

val space : t -> unit -> unit
(** Queues a space to be printed, if no newlines are pending and a non-blank
    string has been emitted. *)

val newline : t -> unit -> unit
(** Queues a hard break (newline) to be printed. *)

val blank_line : t -> unit -> unit
(** Queues a blank line (two newlines) to be printed. *)

val open_box : t -> ?skip_space:bool -> int -> unit
(** Opens a formatting box with the specified indentation. *)

val open_hvbox : t -> ?skip_space:bool -> int -> unit
(** Opens a horizontal-vertical formatting box with the specified indentation.
*)

val open_vbox : t -> ?skip_space:bool -> int -> unit
(** Opens a vertical formatting box with the specified indentation. *)

val close_box : t -> unit -> unit
(** Closes the most recently opened formatting box. *)

val node : t -> int -> (unit -> unit) -> unit
(** Prints a node, consuming any preceding comments/blank lines. *)

val inline_comments : t -> int -> unit
(** Prints inline comments that occur before the given character offset. *)

val catchup : t -> int -> unit
(** Advances the printer output, consuming comments and blank lines up to a
    certain character offset. *)

val remaining_comments : t -> unit
(** Prints any remaining comments and blank lines after all nodes have been
    processed. *)
