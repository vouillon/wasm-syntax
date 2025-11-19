module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = string option * Wasm.Ast.Text.modulefield list
    end)
    (Wasm.Parser)
    (Wasm.Fast_parser)
    (Wasm.Lexer)

let convert ~filename =
  let ast = P.parse ~filename in
  (*
  Wasm.Validation.f ast;
*)
  Format.printf "/////////// %s //////////@.@.%a" filename Output.module_
    (List.filter_map (fun x -> x) (From_wasm.module_ ast))

let _ =
  let p = "/home/jerome/wasm_of_ocaml/runtime/wasm" in
  if false then convert ~filename:(Filename.concat p "int32.wat")
  else
    let l = Sys.readdir p in
    Array.iter
      (fun nm ->
        if Filename.check_suffix nm ".wat" then
          convert ~filename:(Filename.concat p nm))
      l
