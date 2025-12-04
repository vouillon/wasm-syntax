(*
- fix remaining issues
- try to convert non-validating code to test more failure cases
- round-trip through wasx syntax
- Somehow checks that round-tripping yield the identity
  (on unfolded code without identifiers?)
*)

let print_flushed s =
  print_string s;
  flush stdout

type pending_process = {
  pid : int;
  output_file : string;
  on_termination : bool -> string -> unit;
}

type process_pool = {
  max_concurrent : int;
  mutable running : pending_process list;
}

let create_pool max_concurrent = { max_concurrent; running = [] }
let read_file filename = In_channel.with_open_bin filename In_channel.input_all

let handle_finished_pid pool pid status =
  match List.find_opt (fun proc -> proc.pid = pid) pool.running with
  | Some proc ->
      let success = match status with Unix.WEXITED 0 -> true | _ -> false in
      let output_content = read_file proc.output_file in
      (try Sys.remove proc.output_file with _ -> ());
      pool.running <- List.filter (fun p -> p.pid <> pid) pool.running;
      proc.on_termination success output_content
  | None -> ()

let rec reap_children pool mode =
  match Unix.waitpid mode (-1) with
  | 0, _ -> () (* WNOHANG returned 0: No changes, stop recursing. *)
  | pid, status ->
      handle_finished_pid pool pid status;
      reap_children pool [ Unix.WNOHANG ]
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> pool.running <- []
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      (* System call interrupted (e.g. signal), retry exactly as we were *)
      reap_children pool mode

let wait_for_slot pool =
  reap_children pool [ Unix.WNOHANG ];
  if List.length pool.running >= pool.max_concurrent then reap_children pool []

let in_child_process_async pool ?(quiet = false) ~on_termination f =
  wait_for_slot pool;
  let output_file = Filename.temp_file "child_output_" ".txt" in
  match Unix.fork () with
  | 0 ->
      (* Child *)
      let output_fd =
        Unix.openfile output_file
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
          0o600
      in
      Unix.dup2 output_fd Unix.stdout;
      if quiet then (
        let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o666 in
        Unix.dup2 dev_null Unix.stderr;
        Unix.close dev_null)
      else Unix.dup2 output_fd Unix.stderr;
      Unix.close output_fd;
      f ();
      exit 0
  | pid ->
      (* Parent *)
      let proc = { pid; output_file; on_termination } in
      pool.running <- proc :: pool.running

let wait_all_children pool =
  while pool.running <> [] do
    reap_children pool []
  done

let in_child_process ?(quiet = false) f =
  match Unix.fork () with
  | 0 ->
      if quiet then (
        let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o666 in
        Unix.dup2 dev_null Unix.stderr;
        Unix.close dev_null);
      f ();
      exit 0
  | pid ->
      (* Parent process *)
      let _, status = Unix.waitpid [] pid in
      status = Unix.WEXITED 0

let iter_files dirs skip suffix f =
  let pool = create_pool (Domain.recommended_domain_count ()) in
  let rec visit root dir =
    let entries = Sys.readdir (Filename.concat root dir) in
    Array.iter
      (fun entry ->
        let path = Filename.concat dir entry in
        if not (skip entry) then
          let full_path = Filename.concat root path in
          if Sys.is_directory full_path then visit root path
          else if Filename.check_suffix entry suffix then
            in_child_process_async pool
              ~on_termination:(fun _ s -> print_flushed s)
              (fun () -> f full_path path))
      entries
  in
  List.iter (fun root -> visit root "") dirs;
  wait_all_children pool

type script =
  ([ `Valid | `Invalid of string | `Malformed of string ]
  * [ `Parsed of
      string option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
    | `Text of string ])
  list

module Script_parser = struct
  include Wasm.Parser

  module Incremental = struct
    let parse = Wasm.Parser.Incremental.parse_script
  end
end

module Fast_script_parser = struct
  include Wasm.Fast_parser

  let parse = Wasm.Fast_parser.parse_script
end

module ModuleParser =
  Wasm.Parsing.Make_parser
    (struct
      type t = string option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
    end)
    (Wasm.Parser)
    (Wasm.Fast_parser)
    (Wasm.Lexer)

module ScriptParser =
  Wasm.Parsing.Make_parser
    (struct
      type t = script
    end)
    (Script_parser)
    (Fast_script_parser)
    (Wasm.Lexer)

module WaxParser =
  Wasm.Parsing.Make_parser
    (struct
      type t = Wax.Ast.location Wax.Ast.modulefield list
    end)
    (Wax.Parser)
    (Wax.Fast_parser)
    (Wax.Lexer)

let print_module ~color f m =
  Utils.Printer.run f (fun p -> Wasm.Output.module_ p ~color m)

let runtest filename path =
  if true then
    Format.eprintf "%s==== %s ====%s@." Utils.Colors.Ansi.grey path
      Utils.Colors.Ansi.reset;
  let lst = ScriptParser.parse ~filename in
  (* Parsing *)
  let lst =
    List.filter_map
      (fun (status, m) ->
        match (status, m) with
        | ((`Valid | `Invalid _) as status), `Parsed m -> Some (status, m)
        | ((`Valid | `Invalid _) as status), `Text txt ->
            Some (status, ModuleParser.parse_from_string ~filename txt)
        | `Malformed _, `Parsed _ -> assert false
        | `Malformed reason, `Text txt ->
            let ok =
              in_child_process ~quiet:true (fun () ->
                  let ast = ModuleParser.parse_from_string ~filename txt in
                  Wasm.Validation.check_syntax ast;
                  (*                  Wasm.Validation.f ast;*)
                  if false then
                    Format.printf "@[<2>Result:@ %a@]@."
                      (print_module ~color:Always)
                      ast)
            in
            if ok then
              Format.eprintf "Parsing should have failed (%s): %s@." reason txt;
            None)
      lst
  in
  (* Serialization and reparsing *)
  let lst' =
    List.map
      (fun (status, m) ->
        let text = Format.asprintf "%a@." (print_module ~color:Never) m in
        if false then print_flushed text;
        (status, ModuleParser.parse_from_string ~filename text))
      lst
  in
  (* Validation *)
  let lst =
    List.filter
      (fun (status, m) ->
        match (status, m) with
        | `Valid, m ->
            Wasm.Validation.f m;
            true
        | `Invalid reason, m ->
            let ok =
              in_child_process ~quiet:true (fun () ->
                  Wasm.Validation.f m;
                  if false then
                    Format.printf "@[<2>Result:@ %a@]@."
                      (print_module ~color:Always)
                      m)
            in
            if ok then
              Format.eprintf "@[<2>Validation should have failed (%s):@ %a@]@."
                reason
                (print_module ~color:Always)
                m;
            false)
      (lst @ lst')
    |> List.map snd
  in
  (* Translation to new syntax *)
  let print_wax ~color f m =
    Utils.Printer.run f (fun p -> Wax.Output.module_ ~color p m)
  in
  List.iter
    (fun m ->
      match Conversion.From_wasm.module_ m with
      | exception e ->
          prerr_endline (Printexc.to_string e);
          Format.eprintf "@[%a@]@." (print_module ~color:Always) m
      | m ->
          let ok =
            in_child_process (fun () ->
                let m = Wax.Typing.f m in
                let m' = Conversion.To_wasm.module_ m in
                let ok = in_child_process (fun () -> Wasm.Validation.f m') in
                if not ok then (
                  Format.eprintf "@[%a@]@." (print_module ~color:Always) m';
                  Format.eprintf "@[%a@]@." (print_wax ~color:Always) m))
          in
          if not ok then Format.eprintf "@[%a@]@." (print_wax ~color:Always) m;
          let text = Format.asprintf "%a@." (print_wax ~color:Never) m in
          let ok =
            in_child_process (fun () ->
                let m' = WaxParser.parse_from_string ~filename text in
                if false (*XXX*) then
                  let ok =
                    in_child_process (fun () -> ignore (Wax.Typing.f m'))
                  in
                  if not ok then
                    if true then prerr_endline "(after parsing)"
                    else (
                      Format.eprintf "@[%a@]@." (print_wax ~color:Always) m';
                      prerr_endline "===";
                      Format.eprintf "@[%a@]@." (print_wax ~color:Always) m))
          in
          if not ok then
            if true then prerr_endline "(parsing)" else print_flushed text)
    lst

let dirs =
  [
    "/home/jerome/sources/Wasm/test/core";
    "/home/jerome/sources/Wasm/test/legacy";
    "/home/jerome/sources/Wasm/spectec/test-interpreter";
  ]

let () =
  iter_files dirs
    (fun p ->
      List.mem p
        [
          "spec-test-1";
          "spec-test-2";
          "memory64";
          "simd";
          "relaxed-simd";
          "try_delegate.wast";
          "rethrow.wast";
        ])
    ".wast" runtest
