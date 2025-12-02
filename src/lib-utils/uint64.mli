(** 64-bit unsigned integers. *)

type t
(** The type of 64-bit unsigned integers. *)

val of_string : string -> t
val to_string : t -> string
val of_int : int -> t

val to_int : t -> int
(** Convert to an OCaml [int]. May raise if the value doesn't fit. *)

val zero : t
val one : t
val compare : t -> t -> int
