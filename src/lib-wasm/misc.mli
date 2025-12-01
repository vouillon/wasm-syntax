(** Miscellaneous utilities for parsing and validating Wasm values. *)

val float32_of_string : string -> float
(** [float32_of_string s] converts string [s] to a float, with specific handling
    for 32-bit float rounding and edge cases. *)

val is_int32 : string -> bool
(** Checks if string [s] represents a valid 32-bit integer. *)

val is_int64 : string -> bool
(** Checks if string [s] represents a valid 64-bit integer. *)

val is_float32 : string -> bool
(** Checks if string [s] represents a valid 32-bit float. *)

val is_float64 : string -> bool
(** Checks if string [s] represents a valid 64-bit float. *)
