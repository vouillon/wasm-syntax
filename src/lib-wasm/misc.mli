(** Miscellaneous utilities for validating Wasm values. *)

val is_int8 : string -> bool
(** Checks if string [s] represents a valid 8-bit integer. *)

val is_int16 : string -> bool
(** Checks if string [s] represents a valid 16-bit integer. *)

val is_int32 : string -> bool
(** Checks if string [s] represents a valid 32-bit integer. *)

val is_int64 : string -> bool
(** Checks if string [s] represents a valid 64-bit integer. *)

val is_float32 : string -> bool
(** Checks if string [s] represents a valid 32-bit float. *)

val is_float64 : string -> bool
(** Checks if string [s] represents a valid 64-bit float. *)

val escape_string : string -> int * string
(** [escape_string s] returns a pair [(len, escaped)] where [escaped] is the
    escaped version of [s] suitable for WAT string literals, and [len] is its
    display length. *)
