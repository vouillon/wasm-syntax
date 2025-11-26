type t = Int32.t

let of_string s =
  try
    if String.starts_with ~prefix:"0x" s then Int32.of_string s
    else Int32.of_string ("0u" ^ s)
  with Failure _ as e ->
    Format.eprintf "Unsigned int overflow: %s@." s;
    raise e

let to_string s = Printf.sprintf "%lu" s
let of_int i = Int32.of_int i

let to_int i =
  match Int32.unsigned_to_int i with Some i -> i | None -> assert false

let zero = 0l
let one = 1l
let succ = Int32.succ
let add = Int32.add
let compare = Int32.compare
