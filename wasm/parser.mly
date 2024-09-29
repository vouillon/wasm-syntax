%token <string> NAT
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID
%token <Ast.valtype> VALTYPE
%token <Ast.packedtype> PACKEDTYPE
%token LPAREN "("
%token RPAREN ")"
%token EOF
%token ANY
%token EQ
%token I31
%token STRUCT
%token ARRAY
%token NONE
%token FUNC
%token NOFUNC
%token EXTERN
%token NOEXTERN
%token REF
%token NULL
%token PARAM
%token RESULT
%token MUT
%token FIELD
%token REC
%token TYPE
%token SUB
%token FINAL
%token IMPORT
%token LOCAL
%token EXPORT
%token GLOBAL
%token MODULE
%token STRUCT_NEW
%token ARRAY_NEW_FIXED
%token REF_FUNC
%token REF_NULL
%token I32_CONST

%{
open Ast

let map_fst f (x, y) = (f x, y)
%}

%start <string option * Ast.modulefield list> module_

%%

u32: n = NAT { Int32.of_string n (*ZZZ overflow / unsigned*) }

i32:
  n = NAT { Int32.of_string n }
| i = INT { Int32.of_string i }

idx:
| n = u32 { Num n }
| i = ID { Id i }

name: s = STRING { s (*ZZZ check well-formedness*) }

(* Types *)

heaptype:
| ANY { Any }
| EQ { Eq }
| I31 { I31 }
| STRUCT { Struct }
| ARRAY { Array }
| NONE { None_ }
| FUNC { Func_ }
| NOFUNC { NoFunc }
| EXTERN { Extern }
| NOEXTERN { NoExtern }
| i = idx { Type i }

reftype:
| "(" REF nullable = boption(NULL) typ = heaptype ")"
  { { nullable; typ } }
(*ZZZ abbreviations *)

valtype:
| t = VALTYPE { t }
| t = reftype { Ref t }

functype:
| "(" FUNC r = params_and_results(")")
  { fst r }

params(cont):
| c = cont { [], c }
| "(" PARAM ID t = valtype ")" rem = params(cont)
  { map_fst (fun l -> t :: l) rem }
| "(" PARAM l = valtype * ")" rem = params(cont)
  { map_fst ((@) l) rem }

results(cont):
| c = cont { [], c }
| "(" RESULT l = valtype * ")" rem = results(cont)
  { map_fst ((@) l) rem }

params_and_results(cont):
| r = params(results(cont))
  { let (params, (result, c)) = r in ({params; result }, c) }

field:
| "(" FIELD i = ID t = fieldtype ")" { [(Some i, t)] }
| "(" FIELD l = fieldtype * ")" { List.map (fun t -> (None, t)) l }

fieldtype:
| typ = storagetype { {mut = false; typ} }
| "(" MUT typ = storagetype ")" { {mut = true; typ} }

storagetype:
| t = valtype { Value t }
| t = PACKEDTYPE { Packed t }

comptype:
| "(" ARRAY t = fieldtype ")" { Array t }
| "(" STRUCT l = field * ")" { Struct (List.flatten l) }
| t = functype { Func t }

rectype:
| "(" REC l = typedef * ")" { Types l }
| t = typedef { Types [t] }

typedef:
| "(" TYPE name = ID ? t = subtype ")" { t name }

subtype:
| "(" SUB final = boption(FINAL) supertype = idx ? typ = comptype
  ")"
  { fun name -> {name; final; supertype; typ} }
| typ = comptype { fun name -> {name; final = true; supertype = None; typ } }

(* Instructions *)

plaininstr:
| STRUCT_NEW i = idx { StructNew i }
| ARRAY_NEW_FIXED i = idx l = u32 { ArrayNewFixed (i, l) }
| REF_FUNC i = idx { RefFunc i }
| REF_NULL t = heaptype { RefNull t }
| I32_CONST i = i32 { I32Const i }

instr:
| i = plaininstr { i }
| i = foldedinstr { i }

foldedinstr:
| "(" i = plaininstr l = foldedinstr * ")"
  { Folded (i, l) }

expr:
| l = instr * { l }

(* Modules *)

typeuse(cont):
| "(" TYPE i = idx ")" rem = params_and_results(cont)
  { let (s, r) = rem in
    (match s with
     | {params = []; result = []} -> Some i, None
     | _ -> Some i, Some s),
    r }
| rem = params_and_results(cont) { let (s, r) = rem in (None, Some s), r }

import:
| "(" IMPORT module_ = name name = name desc = importdesc ")"
    { Import {module_; name; desc } }

importdesc:
| "(" FUNC i = ID ? t = typeuse(")")
    { Func (i, fst t) }
| "(" GLOBAL i = ID ? t = globaltype ")"
    { (Global (i, t) : importdesc) }
    (* ZZZ *)

func:
| "(" FUNC id = ID ? r = typeuse(locals(instr *)) ")"
    { let (typ, (locals, instrs)) = r in
      Func {id; typ; locals; instrs} }

locals(cont):
| c = cont { [], c }
| "(" LOCAL i = ID ? t = valtype ")" r = locals(cont)
  { map_fst (fun l -> (i, t) :: l) r }

globaltype:
| typ = valtype { {mut = false; typ} }
| "(" MUT typ = valtype ")" { {mut = true; typ} }

global:
| "(" GLOBAL i = ID ? d = globaldesc ")"
    { d i [] }

globaldesc:
| t = globaltype e = expr
  { fun i _ -> Global (i, t, e) }
| "(" IMPORT module_ = name name = name ")" t = globaltype
  { fun i _ -> Import {module_; name; desc = Global (i, t) } }
| "(" EXPORT n = name ")" d = globaldesc
  { fun i exp -> d i (n :: exp) }

modulefield:
| t = rectype { t }
| i = import { i }
| g = global { g }
| func { assert false (*ZZZ*) }

module_:
| "(" MODULE name = ID ? l = modulefield * ")" EOF
  { (name, l) }
| l = modulefield * EOF
  { (None, l) }

(*ZZZ
exports (in globals)
*)
