(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let utf_8_decode_length_of_byte = function
  | '\x00' .. '\x7F' -> 1
  | '\x80' .. '\xC1' -> 0
  | '\xC2' .. '\xDF' -> 2
  | '\xE0' .. '\xEF' -> 3
  | '\xF0' .. '\xF4' -> 4
  | _ -> 0

let utf_8_uchar_length s =
  let slen = String.length s in
  let i = ref 0 and ulen = ref 0 in
  while !i < slen do
    let dec_len = utf_8_decode_length_of_byte (String.unsafe_get s !i) in
    i := !i + if dec_len = 0 then 1 (* count one Uchar.rep *) else dec_len;
    incr ulen
  done;
  !ulen

let uchar_array_of_utf_8_string s =
  let slen = String.length s in
  (* is an upper bound on Uchar.t count *)
  let uchars = Array.make slen Uchar.max in
  let k = ref 0 and i = ref 0 in
  while !i < slen do
    let dec = String.get_utf_8_uchar s !i in
    i := !i + Uchar.utf_decode_length dec;
    uchars.(!k) <- Uchar.utf_decode_uchar dec;
    incr k
  done;
  (uchars, !k)

let edit_distance' ?(limit = Int.max_int) s (s0, len0) s1 =
  if limit <= 1 then if String.equal s s1 then 0 else limit
  else
    let[@inline] minimum a b c = Int.min a (Int.min b c) in
    let s1, len1 = uchar_array_of_utf_8_string s1 in
    let limit = Int.min (Int.max len0 len1) limit in
    if Int.abs (len1 - len0) >= limit then limit
    else
      let s0, s1 = if len0 > len1 then (s0, s1) else (s1, s0) in
      let len0, len1 = if len0 > len1 then (len0, len1) else (len1, len0) in
      let rec loop row_minus2 row_minus1 row i len0 limit s0 s1 =
        if i > len0 then row_minus1.(Array.length row_minus1 - 1)
        else
          let len1 = Array.length row - 1 in
          let row_min = ref Int.max_int in
          row.(0) <- i;
          let jmax =
            let jmax = Int.min len1 (i + limit - 1) in
            if jmax < 0 then (* overflow *) len1 else jmax
          in
          for j = Int.max 1 (i - limit) to jmax do
            let cost = if Uchar.equal s0.(i - 1) s1.(j - 1) then 0 else 1 in
            let min =
              minimum
                (row_minus1.(j - 1) + cost) (* substitute *)
                (row_minus1.(j) + 1) (* delete *)
                (row.(j - 1) + 1)
              (* insert *)
              (* Note when j = i - limit, the latter [row] read makes a bogus read
             on the value that was in the matrix at d.(i-2).(i - limit - 1).
             Since by induction for all i,j, d.(i).(j) >= abs (i - j),
             (row.(j-1) + 1) is greater or equal to [limit] and thus does
             not affect adversely the minimum computation. *)
            in
            let min =
              if
                i > 1 && j > 1
                && Uchar.equal s0.(i - 1) s1.(j - 2)
                && Uchar.equal s0.(i - 2) s1.(j - 1)
              then Int.min min (row_minus2.(j - 2) + cost) (* transpose *)
              else min
            in
            row.(j) <- min;
            row_min := Int.min !row_min min
          done;
          if !row_min >= limit then (* can no longer decrease *) limit
          else loop row_minus1 row row_minus2 (i + 1) len0 limit s0 s1
      in
      let ignore =
        (* Value used to make the values around the diagonal stripe ignored
       by the min computations when we have a limit. *)
        limit + 1
      in
      let row_minus2 = Array.make (len1 + 1) ignore in
      let row_minus1 = Array.init (len1 + 1) (fun x -> x) in
      let row = Array.make (len1 + 1) ignore in
      let d = loop row_minus2 row_minus1 row 1 len0 limit s0 s1 in
      if d > limit then limit else d

let edit_distance ?limit s0 s1 =
  let us0 = uchar_array_of_utf_8_string s0 in
  edit_distance' ?limit s0 us0 s1

let default_max_dist s =
  match utf_8_uchar_length s with 0 | 1 | 2 -> 0 | 3 | 4 -> 1 | _ -> 2

let f ?(max_dist = default_max_dist) iter_dict s =
  let min = ref (max_dist s) in
  let acc = ref [] in
  let select_words s us word =
    let d = edit_distance' ~limit:(!min + 1) s us word in
    if d = !min then acc := word :: !acc
    else if d < !min then (
      min := d;
      acc := [ word ])
    else ()
  in
  let us = uchar_array_of_utf_8_string s in
  iter_dict (select_words s us);
  List.rev !acc
