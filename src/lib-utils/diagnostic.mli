type context
type severity = Error | Warning

type label = {
  location : Ast.location;
  message : Format.formatter -> unit -> unit;
}

val run :
  ?color:Colors.flag ->
  source:string option ->
  ?related:label list ->
  ?exit:bool ->
  ?output:Format.formatter ->
  (context -> 'a) ->
  'a

val report :
  context ->
  location:Ast.location ->
  severity:severity ->
  ?hint:(Format.formatter -> unit -> unit) ->
  ?related:label list ->
  message:(Format.formatter -> unit -> unit) ->
  unit ->
  unit

type theme

val output_error_with_source :
  ?output:Format.formatter ->
  theme:theme ->
  source:string ->
  location:Ast.location ->
  severity:severity ->
  ?hint:(Format.formatter -> unit -> unit) ->
  ?related:label list ->
  (Format.formatter -> unit -> unit) ->
  unit

val get_theme : ?color:Colors.flag -> unit -> theme
