type shape = I8x16 | I16x8 | I32x4 | I64x2 | F32x4 | F64x2
type t = { shape : shape; components : string list }

val to_string : t -> string
val of_string : string -> t
