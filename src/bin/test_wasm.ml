open Wasm

module P =
  Parsing.Make_parser
    (struct
      type t = Ast.location Ast.Text.module_
    end)
    (Tokens)
    (Parser)
    (Fast_parser)
    (Parser_messages)
    (Lexer)

let _ =
  let p = "/home/jerome/wasm_of_ocaml/runtime/wasm" in
  let l = Sys.readdir p in
  let ctx = Utils.Comment.make () in
  Array.iter
    (fun nm ->
      if Filename.check_suffix nm ".wat" then
        ignore (P.parse ~filename:(Filename.concat p nm) ctx ()))
    l;
  ignore
    (P.parse
       ~filename:
         "/home/jerome/tmp/jane-street/_build/default/lib/bonsai/ppx_bonsai/test/inline/.ppx_bonsai_test.inline-tests/inline_test_runner_ppx_bonsai_test.bc.wasm.wat"
       ctx ())

let _ = Types.create
