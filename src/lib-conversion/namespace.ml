module StringMap = Map.Make (String)

type t = { mutable existing_names : int StringMap.t }

let reserved =
  StringMap.of_list
    (List.map
       (fun s -> (s, 1))
       [
         "as";
         "become";
         "br";
         "br_if";
         "br_on_cast";
         "br_on_cast_fail";
         "br_on_non_null";
         "br_on_null";
         "br_table";
         "catch";
         "const";
         "do";
         "else";
         "fn";
         "inf";
         "if";
         "is";
         "let";
         "loop";
         "mut";
         "nan";
         "nop";
         "null";
         "open";
         "rec";
         "return";
         "tag";
         "throw";
         "throw_ref";
         "try";
         "type";
         "unreachable";
       ])

let rec add_indexed ns x i =
  let y = Printf.sprintf "%s_%d" x i in
  if StringMap.mem y ns.existing_names then add_indexed ns x (i + 1)
  else (
    ns.existing_names <-
      ns.existing_names |> StringMap.add y 1 |> StringMap.add x i;
    y)

let add ns x =
  match StringMap.find_opt x ns.existing_names with
  | Some i -> add_indexed ns x (i + 1)
  | None ->
      ns.existing_names <- ns.existing_names |> StringMap.add x 1;
      x

let dup { existing_names } = { existing_names }

let make ?(allow_keywords = false) () =
  { existing_names = (if allow_keywords then StringMap.empty else reserved) }
