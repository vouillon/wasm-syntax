%token <string> IDENT
%token <string> INT
%token <string> STRING

%token EOF

%token AMPERSAND "&"
%token QUESTIONMARK "?"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token COMMA ","
%token COLON ":"
%token ARROW "->"
%token EQUAL "="
%token QUOTE "'"
%token DOT "."
%token MINUS "-"
%token PLUS "+"
%token LTU "<u"
%token GTU ">u"
(*
%token SEMI ";"
*)

%token I32 I64 F32 F64 V128 I8 I16
%token FUNC NOFUNC EXTERN NOEXTERN ANY EQ I31 STRUCT ARRAY NONE

%token FN
%token MUT
%token TYPE
%token REC
%token OPEN
%token NOP UNREACHABLE
%token LOOP IF ELSE
%token LET AS

%right EQUAL
%nonassoc LTU GTU
%left MINUS PLUS
%left AS
%left DOT
%nonassoc IDENT
%nonassoc LPAREN LBRACE

%{
open Ast
%}

%start <modulefield list> module_

%%

numtype:
| I32 {I32} | I64 {I64} | F32 {F32} | F64 {F64}

vectype:
| V128 {V128}

absheaptype:
| FUNC { Func }
| NOFUNC { NoFunc }
| EXTERN { Extern }
| NOEXTERN { NoExtern }
| ANY { Any }
| EQ { Eq }
| I31 { I31 }
| STRUCT { Struct }
| ARRAY { Array}
| NONE { None_ }

heaptype:
| t = absheaptype { t }
| t = IDENT { Type t }

reftype:
| "&" nullable = boption("?") typ = heaptype
  { { nullable; typ } }

valtype:
| t = numtype {t}
| t = vectype {t}
| t = reftype { Ref t }

resulttype:
| "(" ")" { [] }
| t = valtype { [t] }
| "(" l = separated_nonempty_list(",", valtype) ")" { l }

functype:
| FN params = resulttype "->" result = resulttype { {params; result} }

packedtype: | I8 { I8 } | I16 { I16 }

storagetype:
| t = valtype { Value t }
| t = packedtype { Packed t }

fieldtype:
| mut = boption(MUT) typ = storagetype { {mut; typ } }

structtype:
| "{" l = separated_list(",", x = IDENT ":" t = fieldtype { (x, t) } ) "}" { l }

arraytype:
| "[" t = fieldtype "]" { t }

comptype:
| t = structtype { Struct t }
| t = functype { Func t }
| t = arraytype { Array t }

typedef:
| TYPE name = IDENT op = boption(OPEN)
  supertype = option(":" s = IDENT { s })
  "=" typ = comptype
    { {name; typ; supertype; final = not op} }

rectype:
| REC "{" l = list(typedef) "}" { l }
| t = typedef { [t] }

funcparams:
| l = separated_list(",", x = option(x = IDENT ":" { x }) t = valtype { x, t })
  { l }

fundecl:
| FN name = IDENT
  t = option(":" t = IDENT { t } )
  "(" params = funcparams ")"
  "->" rt = resulttype
  { (name, t, params, rt) }

func:
| f = fundecl body = block
  { let (name, typ, params, result) = f in
    Func {name; typ; params; result; body} }

%inline label: l = ioption("'" l = IDENT ":" { l }) { l }

%inline block:
| label = label "{" l = list(instr) "}" { (label, List.flatten l) }

blockinstr:
| b = block { let (label, l) = b in [Block(label, l)] }
| label = label LOOP "{" l = list(instr) "}" { [Loop(label, List.flatten l)] }
| label = label IF "(" e = instr ")" "{" l1 = list(instr) "}"
  l2 = option(ELSE  "{" l = list(instr) "}" { l })
  { [If(label, e, List.flatten l1, Option.map List.flatten l2)] }

plaininstr:
| NOP { [Nop] }
| UNREACHABLE { [Unreachable] }
| x = IDENT { [Get x] }
| x = IDENT "=" i = instr { [Set (x, i)] }
| "(" l = separated_list(",", instr) ")" { List.flatten l }
| x = IDENT "(" l = separated_list(",", instr) ")" { [Call(x, l)] }
| "&" x = IDENT { [RefFunc x] }
| s = STRING { [String s] }
| i = INT { [Int i] }
| x = IDENT "{" l = separated_list(",", y = IDENT ":" i = instr { (y, i) }) "}"
  { [Struct (x, l)] }
| "{" l = separated_nonempty_list(",", y = IDENT ":" i = instr { (y, i) }) "}"
  { [Struct ("x", l)] }
| i = instr AS t = reftype { [Cast(i, t)] }
| i = instr "." x = IDENT { [StructGet(i, x)] }
| i = instr "." x = IDENT "=" j = instr { [StructSet(i, x, j)] }
| i = instr "+" j = instr { [BinOp(Plus, i, j)] }
| i = instr "-" j = instr { [BinOp(Minus, i, j)] }
| i = instr "<u" j = instr { [BinOp(Ltu, i, j)] }
| i = instr ">u" j = instr { [BinOp(Gtu, i, j)] }
| LET x = IDENT t = option(":" t = valtype {t}) i = option("=" i = instr {i})
  { [Local (x, t, i)] }

instr:
| i = blockinstr { i }
| i = plaininstr { i }

global:
| LET name = IDENT typ = option(":" t = valtype {t}) "=" def = instr
  { Global {name; typ; def} }

modulefield:
| r = rectype { Type r }
| f = fundecl
  { let (name, typ, params, result) = f in Fundecl {name; typ; params; result} }
| f = func { f }
| g = global { g }

module_: l = list(modulefield) EOF { l }
