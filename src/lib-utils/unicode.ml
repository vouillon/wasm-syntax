let terminal_width ?(offset = 0) s =
  let char_width acc u =
    let c = Uchar.to_int u in
    if c = 9 (* Tab *) then 8 - ((acc + offset) land 7)
    else Unicode_widths.get_width c
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
