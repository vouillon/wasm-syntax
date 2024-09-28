%token <string> NAT
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID
%token <Ast.valtype> VALTYPE
%token <Ast.packedtype> PACKEDTYPE
%token LPAREN
%token RPAREN
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
| LPAREN REF nullable = boption(NULL) typ = heaptype RPAREN
  { { nullable; typ } }
(*ZZZ abbreviations *)

valtype:
| t = VALTYPE { t }
| t = reftype { Ref t }

functype:
| LPAREN FUNC r = params_and_results RPAREN
  { r }

params_and_results:
| r = list(result) { {params = []; result = List.flatten r} }
| LPAREN PARAM ID t = valtype RPAREN rem = params_and_results
  { {rem with params = t :: rem.params} }
| LPAREN PARAM l = list(valtype) RPAREN rem = params_and_results
  { {rem with params = l @ rem.params} }

result:
| LPAREN RESULT l = list(valtype) RPAREN { l }

field:
| LPAREN FIELD i = ID t = fieldtype RPAREN { [(Some i, t)] }
| LPAREN FIELD l = list(fieldtype) RPAREN { List.map (fun t -> (None, t)) l }

fieldtype:
| typ = storagetype { {mut = false; typ} }
| LPAREN MUT typ = storagetype RPAREN { {mut = true; typ} }

storagetype:
| t = valtype { Value t }
| t = PACKEDTYPE { Packed t }

comptype:
| LPAREN ARRAY t = fieldtype RPAREN { Array t }
| LPAREN STRUCT l = list(field) RPAREN { Struct (List.flatten l) }
| t = functype { Func t }

rectype:
| LPAREN REC l = list(typedef) RPAREN { Types l }
| t = typedef { Types [t] }

typedef:
| LPAREN TYPE name = option(ID) t = subtype RPAREN { t name }

subtype:
| LPAREN SUB final = boption(FINAL) supertype = option(idx) typ = comptype
  RPAREN
  { fun name -> {name; final; supertype; typ} }
| typ = comptype { fun name -> {name; final = true; supertype = None; typ } }

(* Instructions *)

plaininstr:
| STRUCT_NEW i = idx { StructNew i }
| ARRAY_NEW_FIXED i = idx l = u32 { ArrayNewFixed (i, l) }
| REF_FUNC i = idx { RefFunc i }
| REF_NULL i = idx { RefNull i }
| I32_CONST i = i32 { I32Const i }

instr:
| i = plaininstr { i }
| i = foldedinstr { i }

foldedinstr:
| LPAREN i = plaininstr l = list(foldedinstr) RPAREN
  { Folded (i, l) }

expr:
| l = list(instr) { l }

(* Modules *)

typeuse:
| LPAREN TYPE i = idx RPAREN s = params_and_results
  { match s with
    | {params = []; result = []} -> Some i, None
    | _ -> Some i, Some s }
| s = params_and_results { None, Some s }

import:
| LPAREN IMPORT module_ = name name = name desc = importdesc RPAREN
    { Import {module_; name; desc } }

importdesc:
| LPAREN FUNC i = option(ID) t = typeuse RPAREN
    { Func (i, t) }
(* ZZZ *)

globaltype:
| typ = valtype { {mut = false; typ} }
| LPAREN MUT typ = valtype RPAREN { {mut = true; typ} }

global:
| LPAREN GLOBAL i = option(ID) d = globaldesc RPAREN
    { d i [] }

globaldesc:
| t = globaltype e = expr
  { fun i _ -> Global (i, t, e) }
| LPAREN IMPORT module_ = name name = name RPAREN t = globaltype
  { fun i _ -> Import {module_; name; desc = Global (i, t) } }
| LPAREN EXPORT n = name RPAREN d = globaldesc
  { fun i exp -> d i (n :: exp) }

modulefield:
| t = rectype { t }
| i = import { i }
| g = global { g }

module_:
| LPAREN MODULE name = option(ID) l = list(modulefield) RPAREN EOF
  { (name, l) }
| l = list(modulefield) EOF
  { (None, l) }

(*ZZZ
exports (in globals)
*)
