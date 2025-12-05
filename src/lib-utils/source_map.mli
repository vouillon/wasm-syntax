type t

type mapping = {
  generated_offset : int;
  original_file_idx : int;
  original_line : int;
  original_column : int;
}

val create : unit -> t
val register_file : t -> string -> int

val add_mapping :
  t -> generated_offset:int -> original_location:Utils.Ast.location -> unit

val to_json : t -> file_name:string -> string
