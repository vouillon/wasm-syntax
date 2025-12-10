let terminal_width ?(offset = 0) s =
  let char_width acc u =
    let c = Uchar.to_int u in
    if (c >= 0x20 && c <= 0x7e) || (c >= 0xa0 && c <= 0xff) then 1
    else if c = 9 (* Tab *) then 8 - ((acc + offset) land 7)
    else
      match Uucp.Hangul.syllable_type u with
      | `V | `T -> 0
      | _ -> (
          if Uucp.Gen.is_default_ignorable u then 0
          else
            match Uucp.Gc.general_category u with
            | `Mn | `Me | `Cc | `Cf -> 0
            | _ -> (
                (* 2. Check for Wide Characters *)
                match Uucp.Break.east_asian_width u with
                | `W | `F -> 2
                | _ -> 1))
  in
  let rec loop acc i len =
    if i >= len then acc
    else
      let dec = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar dec in
      let acc = acc + char_width acc u in
      loop acc (i + Uchar.utf_decode_length dec) len
  in
  loop 0 0 (String.length s)
