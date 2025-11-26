type t = Int64.t

let of_string s =
  try
    if String.starts_with ~prefix:"0x" s then Int64.of_string s
    else Int64.of_string ("0u" ^ s)
  with Failure _ as e ->
    Format.eprintf "Unsigned int overflow: %s@." s;
    raise e

let to_string s = Printf.sprintf "%Lu" s
let of_int i = Int64.of_int i
let zero = 0L
let one = 1L
