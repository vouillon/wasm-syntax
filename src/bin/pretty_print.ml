module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = string option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
    end)
    (Wasm.Parser)
    (Wasm.Fast_parser)
    (Wasm.Lexer)

let print_module f m =
  Utils.Printer.run f (fun p -> Wasm.Output.module_ ~out_channel:stdout p m)

let convert ~filename =
  let ast = P.parse ~filename () in
  Format.printf "/////////// %s //////////@.@.%a@." filename print_module
    (Wasm.Folding.fold (Wasm.Folding.unfold ast))

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
