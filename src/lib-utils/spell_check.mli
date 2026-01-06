(** Sound-alike and spell-checking utilities. *)

val edit_distance : ?limit:int -> string -> string -> int
(** [edit_distance ?limit s0 s1] computes the Damerau-Levenshtein distance
    between [s0] and [s1]. Returns [limit] (defaulting to [Int.max_int]) if the
    distance is greater than [limit]. *)

val f :
  ?max_dist:(string -> int) ->
  ((string -> unit) -> unit) ->
  string ->
  string list
(** [f ?max_dist iter_dict s] returns a list of words from [iter_dict] that are
    within [max_dist] of [s]. [iter_dict] is a function that iterates over the
    dictionary and calls its argument with each word. *)
