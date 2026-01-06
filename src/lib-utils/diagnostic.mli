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
(** [run ?color ~source ?related ?exit ?output f] runs the diagnostic context
    [f]. [source] is the source code for the diagnostics (if available). *)

val report :
  context ->
  location:Ast.location ->
  severity:severity ->
  ?hint:(Format.formatter -> unit -> unit) ->
  ?related:label list ->
  message:(Format.formatter -> unit -> unit) ->
  unit ->
  unit
(** [report context ~location ~severity ?hint ?related ~message ()] reports a
    diagnostic. *)

type theme
(** A theme for diagnostic output. *)

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
(** [output_error_with_source ?output ~theme ~source ~location ~severity ?hint
     ?related message] prints an error message with a source code snippet. *)

val get_theme : ?color:Colors.flag -> unit -> theme
(** [get_theme ?color ()] returns the diagnostic theme. *)
