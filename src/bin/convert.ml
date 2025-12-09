module P =
  Wasm.Parsing.Make_parser
    (struct
      type t = string option * Wasm.Ast.location Wasm.Ast.Text.modulefield list
    end)
    (Wasm.Parser)
    (Wasm.Fast_parser)
    (Wasm.Lexer)

let convert ~filename =
  let source = In_channel.with_open_bin filename In_channel.input_all in
  let ast = P.parse_from_string ~filename source in
  prerr_endline filename;
  Wasm.Validation.validate_refs := false;
  Utils.Diagnostic.run ~source:(Some source) (fun d -> Wasm.Validation.f d ast);
  let ast' = Conversion.From_wasm.module_ ast in
  if false then
    ignore
      (Utils.Diagnostic.run ~source:(Some source) (fun d -> Wax.Typing.f d ast'));
  let print_wax f m =
    Utils.Printer.run f (fun p -> Wax.Output.module_ ~out_channel:stdout p m)
  in
  Format.eprintf "%s==== %s ====%s@.@.%a@.@." Utils.Colors.Ansi.grey filename
    Utils.Colors.Ansi.reset print_wax ast'

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
