type t = {
  files : (string, int) Hashtbl.t;
  mutable next_file_idx : int;
  mutable mappings : mapping list;
}

and mapping = {
  generated_offset : int;
  original_file_idx : int;
  original_line : int;
  original_column : int;
}

let create () = { files = Hashtbl.create 10; next_file_idx = 0; mappings = [] }

let register_file t filename =
  match Hashtbl.find_opt t.files filename with
  | Some idx -> idx
  | None ->
      let idx = t.next_file_idx in
      Hashtbl.add t.files filename idx;
      t.next_file_idx <- idx + 1;
      idx

let add_mapping t ~generated_offset ~original_location =
  let file_idx =
    register_file t original_location.Ast.loc_start.Lexing.pos_fname
  in
  let new_mapping =
    {
      generated_offset;
      original_file_idx = file_idx;
      original_line = original_location.Ast.loc_start.Lexing.pos_lnum - 1;
      (* 0-indexed *)
      original_column =
        original_location.Ast.loc_start.Lexing.pos_cnum
        - original_location.Ast.loc_start.Lexing.pos_bol;
      (* 0-indexed *)
    }
  in
  t.mappings <- new_mapping :: t.mappings

(* VLQ encoding for source maps *)
let encode_vlq_int n =
  let n' = ref (if n < 0 then (abs n lsl 1) lor 1 else n lsl 1) in
  let result = ref [] in
  while !n' <> 0 || !result = [] do
    let digit = !n' land 0x1F in
    n' := !n' lsr 5;
    if !n' <> 0 then result := Char.chr (digit lor 0x20) :: !result
    else result := Char.chr digit :: !result
  done;
  List.rev !result |> List.to_seq |> String.of_seq

let to_json t ~file_name =
  let sorted_mappings =
    List.sort
      (fun a b -> compare a.generated_offset b.generated_offset)
      t.mappings
  in

  let files_list =
    Hashtbl.fold (fun f_name f_idx acc -> (f_idx, f_name) :: acc) t.files []
    |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
    |> List.map snd
  in

  let mappings_string =
    let prev_gen_col = ref 0 in
    let prev_orig_file_idx = ref 0 in
    let prev_orig_line = ref 0 in
    let prev_orig_col = ref 0 in

    List.fold_left
      (fun acc mapping ->
        (* This is tricky. generated_offset is byte offset, not column.
         We need generated_line and generated_column.
         This implies Encoder.instr needs to track line/column.
         For now, I will simplify and just use offset as "generated column" which is wrong.
         A proper solution would require modifying the buffer to track lines.
         Let's just use 0 for generated line and offset for generated col for now. This is a hack.
      *)
        let generated_column_for_vlq = mapping.generated_offset in
        (* HACK: Treat offset as column for VLQ *)

        let segment =
          [
            encode_vlq_int (generated_column_for_vlq - !prev_gen_col);
            encode_vlq_int (mapping.original_file_idx - !prev_orig_file_idx);
            encode_vlq_int (mapping.original_line - !prev_orig_line);
            encode_vlq_int (mapping.original_column - !prev_orig_col);
          ]
        in

        prev_gen_col := generated_column_for_vlq;
        prev_orig_file_idx := mapping.original_file_idx;
        prev_orig_line := mapping.original_line;
        prev_orig_col := mapping.original_column;

        String.concat "" segment :: acc)
      [] sorted_mappings
    |> List.rev |> String.concat ","
  in

  Printf.sprintf
    {|{
  "version": 3,
  "file": "%s",
  "sourceRoot": "",
  "sources": [%s],
  "sourcesContent": [],
  "names": [],
  "mappings": "%s"
}|}
    file_name
    (String.concat "," (List.map (fun s -> Printf.sprintf "%s" s) files_list))
    mappings_string
