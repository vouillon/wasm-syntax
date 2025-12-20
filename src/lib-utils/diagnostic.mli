type context
type severity = Error | Warning

val run : ?color:Colors.flag -> source:string option -> (context -> 'a) -> 'a

val report :
  context ->
  location:Ast.location ->
  severity:severity ->
  ?hint:(Format.formatter -> unit -> unit) ->
  message:(Format.formatter -> unit -> unit) ->
  unit ->
  unit

type theme

val output_error_with_source :
  theme:theme ->
  source:string ->
  location:Ast.location ->
  severity:severity ->
  ?hint:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> unit -> unit) ->
  unit

val get_theme : ?color:Colors.flag -> unit -> theme
