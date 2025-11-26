type t

val of_string : string -> t
val to_string : t -> string
val of_int : int -> t
val to_int : t -> int
val zero : t
val one : t
val succ : t -> t
val add : t -> t -> t
val compare : t -> t -> int
