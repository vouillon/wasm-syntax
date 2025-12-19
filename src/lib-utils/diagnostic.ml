type theme = {
  error_header : string;
  warning_header : string;
  error_label : string;
  warning_label : string;
  line_numbers : string;
}

let get_theme ?(color = Colors.Auto) () =
  let use_color = Colors.should_use_color ~color ~out_channel:(Some stderr) in
  let open Colors in
  if use_color then
    {
      error_header = Ansi.bold ^ Ansi.high_red;
      warning_header = Ansi.bold ^ Ansi.high_yellow;
      error_label = Ansi.red;
      warning_label = Ansi.yellow;
      line_numbers = Ansi.cyan;
    }
  else
    {
      error_header = "";
      warning_header = "";
      error_label = "";
      warning_label = "";
      line_numbers = "";
    }

let with_style color g f x =
  let pr_color f c = Format.pp_print_as f 0 c in
  Format.fprintf f "%a%a%a" pr_color color g x pr_color
    (if color = "" then "" else Colors.Ansi.reset)

type severity = Error | Warning

type t = {
  location : Ast.location;
  severity : severity;
  message : Format.formatter -> unit -> unit;
}

let output_error_no_loc ~theme ~severity msg =
  Format.eprintf "@[<2>%a:@ %a@]@."
    (match severity with
    | Error ->
        with_style theme.error_header (fun f () -> Format.fprintf f "Error")
    | Warning ->
        with_style theme.warning_header (fun f () -> Format.fprintf f "Warning"))
    () msg ()

let output_error_no_source ~theme ~location:{ Ast.loc_start; loc_end } ~severity
    msg =
  let start_line = loc_start.Lexing.pos_lnum in
  let end_line = loc_end.Lexing.pos_lnum in
  let filename = loc_start.Lexing.pos_fname in
  let s_bol = loc_start.Lexing.pos_bol in
  let s_cnum = loc_start.Lexing.pos_cnum in
  let start_col = s_cnum - s_bol in
  let e_bol = loc_end.Lexing.pos_bol in
  let e_cnum = loc_end.Lexing.pos_cnum in
  let end_col = e_cnum - e_bol in
  if start_line = end_line then
    Format.eprintf "File \"%s\", line %d, characters %d-%d:@." filename
      start_line start_col end_col
  else
    Format.eprintf
      "File \"%s\", line %d, character %d to line %d, character %d:@." filename
      start_line start_col end_line end_col;
  output_error_no_loc ~theme ~severity msg

let count_leading_whitespaces s =
  let i = ref 0 in
  while !i < String.length s && s.[!i] = ' ' do
    incr i
  done;
  !i

let output_error_with_source ~theme ~source ~location:{ Ast.loc_start; loc_end }
    ~severity msg =
  let rewind_line text bol =
    if bol < 2 then 0
    else try String.rindex_from text (bol - 2) '\n' + 1 with Not_found -> 0
  in
  let find_eol text start_pos =
    try String.index_from text start_pos '\n'
    with Not_found -> String.length text
  in
  let get_line_info text pos_bol =
    let line_end = find_eol text pos_bol in
    let content = String.sub text pos_bol (line_end - pos_bol) in
    (content, line_end)
  in
  let start_line = loc_start.Lexing.pos_lnum in
  let end_line = loc_end.Lexing.pos_lnum in
  let filename = loc_start.Lexing.pos_fname in
  let s_bol = loc_start.Lexing.pos_bol in
  let s_cnum = loc_start.Lexing.pos_cnum in
  let e_cnum = loc_end.Lexing.pos_cnum in
  let start_col_byte = s_cnum - s_bol in
  let end_col_byte = e_cnum - loc_end.Lexing.pos_bol in
  if start_line = end_line then
    Format.eprintf "File \"%s\", line %d, characters %d-%d:@." filename
      start_line start_col_byte end_col_byte
  else
    Format.eprintf
      "File \"%s\", line %d, character %d to line %d, character %d:@." filename
      start_line start_col_byte end_line end_col_byte;
  output_error_no_loc ~theme ~severity msg;
  let context_lines = 2 in
  let total_lines = end_line - start_line + 1 in
  let should_truncate = total_lines > (context_lines * 2) + 1 in
  let gutter_width = String.length (string_of_int end_line) in
  let gutter_width = max 1 gutter_width in
  let first_printed_line = max 1 (start_line - 1) in
  let curr_pos =
    ref
      (if first_printed_line < start_line then rewind_line source s_bol
       else s_bol)
  in
  let current_line = ref first_printed_line in
  let print_line ?(at_end = false) header contents =
    Format.eprintf "%a %a@."
      (with_style theme.line_numbers (fun f () ->
           Format.fprintf f "%a %a" header ()
             (fun f () -> Format.pp_print_as f 1 (if at_end then " " else "│"))
             ()))
      () contents ()
  in
  let print_string s f () = Format.fprintf f "%s" s in
  let gutter_padding = String.make gutter_width ' ' in
  let print_underline ?at_end col len =
    let code_padding = String.make col ' ' in
    let underline = String.make len '^' in
    print_line ?at_end
      (fun f () -> Format.fprintf f "%s" gutter_padding)
      (fun f () ->
        Format.fprintf f "%s%a" code_padding
          (with_style
             (match severity with
             | Error -> theme.error_label
             | Warning -> theme.warning_label)
             (fun f u -> Format.fprintf f "%s" u))
          underline)
  in
  while !current_line <= end_line do
    let is_head = !current_line < start_line + context_lines in
    let is_tail = !current_line > end_line - context_lines in
    if should_truncate && (not is_head) && not is_tail then (
      if !current_line = start_line + context_lines then
        print_line (print_string gutter_padding) (print_string "...");
      let _, next_eol = get_line_info source !curr_pos in
      curr_pos := next_eol + 1;
      incr current_line)
    else
      let raw_content, next_eol = get_line_info source !curr_pos in
      let display_content = Unicode.expand_tabs raw_content in
      print_line
        (fun f () -> Format.fprintf f "%*d" gutter_width !current_line)
        (print_string display_content);
      (if !current_line = start_line then
         let prefix = String.sub raw_content 0 start_col_byte in
         let visual_col = Unicode.terminal_width prefix in
         if start_line <> end_line then
           let rest_len =
             max 1 (Unicode.terminal_width raw_content - visual_col)
           in
           print_underline visual_col (rest_len + 4)
         else
           let err_len_bytes = e_cnum - s_cnum in
           let err_part = String.sub raw_content start_col_byte err_len_bytes in
           print_underline ~at_end:true visual_col
             (max 1 (Unicode.terminal_width err_part))
       else if !current_line = end_line then
         let err_part = String.sub raw_content 0 end_col_byte in
         let n = max 0 (count_leading_whitespaces err_part - 4) in
         print_underline ~at_end:true n (Unicode.terminal_width err_part - n));
      curr_pos := next_eol + 1;
      incr current_line
  done

let output_error ~theme ~source ~location:{ Ast.loc_start; loc_end } ~severity
    msg =
  if loc_start = Lexing.dummy_pos then output_error_no_loc ~theme ~severity msg
  else
    match source with
    | None ->
        output_error_no_source ~theme ~location:{ Ast.loc_start; loc_end }
          ~severity msg
    | Some source ->
        output_error_with_source ~theme ~source
          ~location:{ Ast.loc_start; loc_end } ~severity msg

type context = {
  max : int;
  queue : t Queue.t;
  source : string option;
  theme : theme;
}

let make ?color ~source () =
  let theme = get_theme ?color () in
  { max = 1; queue = Queue.create (); source; theme }

let output_errors context =
  if not (Queue.is_empty context.queue) then (
    Queue.iter
      (fun { location; severity; message } ->
        output_error ~theme:context.theme ~source:context.source ~location
          ~severity message)
      context.queue;
    Queue.clear context.queue;
    exit 128)

let report context ~location ~severity ~message =
  match severity with
  | Warning ->
      output_error ~theme:context.theme ~source:context.source ~location
        ~severity message
  | Error ->
      Queue.push { location; severity; message } context.queue;
      if Queue.length context.queue = context.max then output_errors context

let run ?color ~source f =
  let d = make ?color ~source () in
  let res = f d in
  output_errors d;
  res
