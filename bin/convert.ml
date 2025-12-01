module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = string option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
    end)
    (Wasm.Parser)
    (Wasm.Fast_parser)
    (Wasm.Lexer)

let convert ~filename =
  let ast = P.parse ~filename in
  (*
  Wasm.Validation.f ast;
*)
  let ast' = Conversion.From_wasm.module_ ast in
  Wax.Typing.f ast';
  let print_wax f m = Utils.Printer.run f (fun p -> Wax.Output.module_ p m) in
  Format.printf "/////////// %s //////////@.@.%a" filename print_wax ast'

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
