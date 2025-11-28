(*
- perform parsing and validation tests
- output the tests and read them back to test the text output
  ==> we need to isolate the tests from what is parsed
- fix remaining issues
- conversion to rust-like format and typing
*)

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
  let rec visit dir =
    let entries = Sys.readdir dir in
    Array.iter
      (fun entry ->
        let path = Filename.concat dir entry in
        if not (skip entry) then
          if Sys.is_directory path then visit path
          else if Filename.check_suffix entry suffix then
            in_child_process_async pool
              ~on_termination:(fun _ s ->
                print_string s;
                flush stdout)
              (fun () -> f path))
      entries
  in
  List.iter visit dirs;
  wait_all_children pool

let dirs =
  [
    "/home/jerome/sources/Wasm/test/core";
    "/home/jerome/sources/Wasm/spectec/test-interpreter";
  ]

type script =
  ([ `Valid | `Invalid of string | `Malformed of string ]
  * [ `Parsed of string option * Ast.location Wasm.Ast.Text.modulefield list
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
      type t = string option * Ast.location Wasm.Ast.Text.modulefield list
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

module FancyParser =
  Wasm.Parsing.Make_parser
    (struct
      type t = Ast.location Ast.modulefield list
    end)
    (Parser)
    (Fast_parser)
    (Lexer)

let check_wellformed (_, lst) =
  let types = Hashtbl.create 16 in
  let functions = Hashtbl.create 16 in
  let memories = Hashtbl.create 16 in
  let tables = Hashtbl.create 16 in
  let globals = Hashtbl.create 16 in
  let tags = Hashtbl.create 16 in
  let elems = Hashtbl.create 16 in
  let datas = Hashtbl.create 16 in
  let check_unbound tbl id =
    Option.iter
      (fun id ->
        assert (not (Hashtbl.mem tbl id));
        Hashtbl.add tbl id ())
      id
  in
  List.iter
    (fun (field : _ Wasm.Ast.Text.modulefield) ->
      match field with
      | Types lst ->
          Array.iter
            (fun (id, subtype) ->
              check_unbound types id;
              match subtype.Wasm.Ast.Text.typ with
              | Func _ | Array _ -> ()
              | Struct lst ->
                  let fields = Hashtbl.create 16 in
                  Array.iter (fun (id, _) -> check_unbound fields id) lst)
            lst
      | Import { id; desc; _ } ->
          check_unbound
            (match desc with
            | Func _ -> functions
            | Memory _ -> memories
            | Table _ -> tables
            | Global _ -> globals
            | Tag _ -> tags)
            id
      | Func { id; _ } -> check_unbound functions id
      | Memory { id; _ } -> check_unbound memories id
      | Table { id; _ } -> check_unbound tables id
      | Tag { id; _ } -> check_unbound tags id
      | Global { id; _ } -> check_unbound globals id
      | Export _ | Start _ -> ()
      | Elem { id; _ } -> check_unbound elems id
      | Data { id; _ } -> check_unbound datas id)
    lst;
  ignore
    (List.fold_left
       (fun can_import (field : _ Wasm.Ast.Text.modulefield) ->
         match field with
         | Types _ -> can_import
         | Import _ ->
             assert can_import;
             (*ZZZ*)
             can_import
         | Func _ | Memory _ | Table _ | Tag _ | Global _ -> false
         | Export _ -> can_import
         | Start _ -> can_import
         | Elem _ -> can_import
         | Data _ -> can_import)
       true lst);
  assert (
    List.length
      (List.filter
         (fun field ->
           match field with Wasm.Ast.Text.Start _ -> true | _ -> false)
         lst)
    <= 1)

let runtest filename =
  if true then prerr_endline filename;
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
                  Wasm.Validation.f ast;
                  check_wellformed ast;
                  if false then
                    Format.printf "@[<2>Result:@ %a@]@." Wasm.Output.module_ ast)
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
        let text = Format.asprintf "%a@." Wasm.Output.module_ m in
        if false then prerr_endline text;
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
                    Format.printf "@[<2>Result:@ %a@]@." Wasm.Output.module_ m)
            in
            if ok then
              Format.eprintf "@[<2>Validation should have failed (%s):@ %a@]@."
                reason Wasm.Output.module_ m;
            false)
      (lst @ lst')
    |> List.map snd
  in
  (* Translation to new syntax *)
  List.iter
    (fun m ->
      match From_wasm.module_ m with
      | exception e ->
          prerr_endline (Printexc.to_string e);
          Format.eprintf "@[%a@]@." Wasm.Output.module_ m
      | m ->
          let ok = in_child_process (fun () -> Typing.f m) in
          if not ok then Format.eprintf "@[%a@]@." Output.module_ m;
          let text = Format.asprintf "%a@." Output.module_ m in
          let ok =
            in_child_process (fun () ->
                let m = FancyParser.parse_from_string ~filename text in
                let ok = in_child_process (fun () -> Typing.f m) in
                if not ok then Format.eprintf "@[%a@]@." Output.module_ m)
          in
          if not ok then prerr_endline text)
    lst

let () =
  iter_files dirs
    (fun p ->
      List.mem p
        [ "spec-test-1"; "spec-test-2"; "memory64"; "simd"; "relaxed-simd" ])
    ".wast" runtest
