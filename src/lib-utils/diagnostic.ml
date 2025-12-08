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

let visual_width s =
  let len = String.length s in
  let rec loop i width =
    if i >= len then width
    else
      let decode = String.get_utf_8_uchar s i in
      let char_width =
        let u = Uchar.utf_decode_uchar decode in
        if Uchar.equal u (Uchar.of_int 0x09) then
          let current_visual_col = width in
          (((current_visual_col / 8) + 1) * 8) - current_visual_col
        else 1
      in
      loop (i + Uchar.utf_decode_length decode) (width + char_width)
  in
  loop 0 0

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

let output_error_with_source ~theme ~source ~location:{ Ast.loc_start; loc_end }
    ~severity msg =
  let find_eol text start_pos =
    try String.index_from text start_pos '\n'
    with Not_found -> String.length text
  in
  let start_line = loc_start.Lexing.pos_lnum in
  let end_line = loc_end.Lexing.pos_lnum in
  let filename = loc_start.Lexing.pos_fname in
  let s_bol = loc_start.Lexing.pos_bol in
  let s_cnum = loc_start.Lexing.pos_cnum in
  let s_prefix = String.sub source s_bol (s_cnum - s_bol) in
  let start_col = visual_width s_prefix in
  let e_bol = loc_end.Lexing.pos_bol in
  let e_cnum = loc_end.Lexing.pos_cnum in
  let e_prefix = String.sub source e_bol (e_cnum - e_bol) in
  let end_col = visual_width e_prefix in
  if start_line = end_line then
    Format.eprintf "File \"%s\", line %d, characters %d-%d:@." filename
      start_line start_col end_col
  else
    Format.eprintf
      "File \"%s\", line %d, character %d to line %d, character %d:@." filename
      start_line start_col end_line end_col;
  output_error_no_loc ~theme ~severity msg;
  if start_line = end_line then (
    let line_end = find_eol source s_bol in
    let line_code = String.sub source s_bol (line_end - s_bol) in
    Format.eprintf "%a %s@."
      (with_style theme.line_numbers (fun f n -> Format.fprintf f "%5d |" n))
      start_line line_code;
    let error_len_bytes = e_cnum - s_cnum in
    let error_text = String.sub source s_cnum error_len_bytes in
    let visual_len = max 1 (visual_width error_text) in
    let padding = String.make (8 + start_col) ' ' in
    let underline = String.make visual_len '^' in
    Format.eprintf "%s%a@.@." padding
      (with_style
         (match severity with
         | Error -> theme.error_label
         | Warning -> theme.warning_label)
         (fun f u -> Format.fprintf f "%s" u))
      underline)
  else
    let curr_pos = ref s_bol in
    let max_lines_to_print = 5 in
    let total_lines = end_line - start_line + 1 in
    for i = 0 to total_lines - 1 do
      if i < max_lines_to_print then begin
        let line_num = start_line + i in
        let line_end = find_eol source !curr_pos in
        let line_len = line_end - !curr_pos in
        let line_content = String.sub source !curr_pos line_len in
        Format.eprintf "%a %s@."
          (with_style theme.line_numbers (fun f n -> Format.fprintf f "%5d |" n))
          line_num line_content;
        curr_pos := line_end + 1
      end
      else if i = max_lines_to_print then
        Format.eprintf "%a ...@."
          (with_style theme.line_numbers (fun f () ->
               Format.fprintf f "      |"))
          ()
    done;
    Format.eprintf "@."

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
  { max = 5; queue = Queue.create (); source; theme }

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
