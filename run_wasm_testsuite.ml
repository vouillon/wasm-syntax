(*
- perform parsing and validation tests
- output the tests and read them back to test the text output
  ==> we need to isolate the tests from what is parsed
- fix remaining issues
- conversion to rust-like format and typing
*)

let iter_files dirs skip suffix f =
  let rec visit dir =
    let entries = Sys.readdir dir in
    Array.iter
      (fun entry ->
        let path = Filename.concat dir entry in
        if not (skip entry) then
          if Sys.is_directory path then visit path
          else if Filename.check_suffix entry suffix then f path)
      entries
  in
  List.iter visit dirs

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

module Parser = struct
  include Wasm.Parser

  module Incremental = struct
    let parse = Wasm.Parser.Incremental.parse_script
  end
end

module Fast_parser = struct
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
    (Parser)
    (Fast_parser)
    (Wasm.Lexer)

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
  let _ =
    in_child_process (fun () ->
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
                        let ast =
                          ModuleParser.parse_from_string ~filename txt
                        in
                        check_wellformed ast;
                        if false then
                          Format.printf "@[<2>Result:@ %a@]@."
                            Wasm.Output.module_ ast)
                  in
                  if ok then
                    Format.eprintf "Parsing should have failed (%s): %s@."
                      reason txt;
                  None)
            lst
        in
        List.iter
          (fun (_, m) ->
            let text = Format.asprintf "%a@." Wasm.Output.module_ m in
            if false then prerr_endline text;
            let _ast = ModuleParser.parse_from_string ~filename text in
            ())
          lst;
        if false then
          List.iter
            (fun (status, m) ->
              match (status, m) with
              | `Valid, m -> Wasm.Validation.f m
              | `Invalid _, _ -> (* validate (should fail) *) ())
            lst)
  in
  ()

let () =
  iter_files dirs
    (fun p ->
      List.mem p
        [
          "spec-test-1" (* invalid syntax *); "memory64"; "simd"; "relaxed-simd";
        ])
    ".wast" runtest
