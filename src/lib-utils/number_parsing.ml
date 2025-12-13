(*ZZZ We should avoid double truncation when parsing float32 floats *)

let parse_custom_nan s is_double =
  let len = String.length s in
  let has_sign = len > 0 && (s.[0] = '-' || s.[0] = '+') in
  let offset = if has_sign then 1 else 0 in
  if len > offset + 4 && String.sub s offset 4 = "nan:" then
    let payload_str = String.sub s (offset + 4) (len - (offset + 4)) in
    let payload = Int64.of_string payload_str in
    let sign_bit = if s.[0] = '-' then 1L else 0L in
    if is_double then
      let exponent = 0x7FFL in
      let bits =
        Int64.logor
          (Int64.logor
             (Int64.shift_left sign_bit 63)
             (Int64.shift_left exponent 52))
          (Int64.logand payload 0xFFFFFFFFFFFFFL)
      in
      Int64.float_of_bits bits
    else
      let exponent = 0xFFL in
      let bits =
        Int64.logor
          (Int64.logor
             (Int64.shift_left sign_bit 31)
             (Int64.shift_left exponent 23))
          (Int64.logand payload 0x7FFFFFL)
      in
      Int32.float_of_bits (Int64.to_int32 bits)
  else float_of_string s

let float32 s = parse_custom_nan s false
let float64 s = parse_custom_nan s true
let int_conv conv s = try conv s with Failure _ -> conv ("0u" ^ s)
let int32 s = int_conv Int32.of_string s
let int64 s = int_conv Int64.of_string s
