%token <string> IDENT
%token <string> INT
%token <string> STRING

%token EOF

%token SEMI
%token AMPERSAND "&"
%token PIPE "|"
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
%token UNDERSCORE

%token FN
%token MUT
%token TYPE
%token REC
%token OPEN
%token NOP UNREACHABLE
%token LOOP IF ELSE
%token LET AS
%token BR BR_IF BR_TABLE RETURN

%nonassoc LET
%nonassoc br IDENT
%nonassoc LBRACE
%right EQUAL
%nonassoc LTU GTU
%left PIPE
%left AMPERSAND
%left MINUS PLUS
%left AS
%left DOT

(* BR foo 1 + 2 understood as BR foo (1 + 2)
   BR_TABLE { ...} 1 + 2 understood as BR_TABLE { ...} (1 + 2)
   BR foo { ... } understood as a single instruction
   LET x = br foo LET --> LET x = (br foo) LET  (LET < br)
   IF BR_TABLE {...} { ... } --> IF (BR_TABLE {...}) { ... } (br < LBRACE)
   IF foo { ... } --> IF (foo) { ... }    (not a struct)  (IDENT < LBRACE)
*)

%{
open Ast

let tbl_from_list l =
 let h = Hashtbl.create (2 * List.length l) in
 List.iter (fun (k, v) -> Hashtbl.add h k v) l;
 h

let absheaptype_tbl =
  tbl_from_list
    ["func", Func_;
     "nofunc", NoFunc;
     "extern", Extern;
     "noextern", NoExtern;
     "any", Any;
     "eq", Eq;
     "i31", I31;
     "struct", Struct;
     "array", Array;
     "none", None_]

let valtype_tbl =
  tbl_from_list
    ["i32", I32; "i64", I64; "f32", F32; "f64", F64; "v128", V128]

let storagetype_tbl =
  tbl_from_list
    ["i8", Packed I8; "i16", Packed I16;
     "i32", Value I32; "i64", Value I64; "f32", Value F32; "f64", Value F64;
     "v128", Value V128]

let with_loc (loc_start, loc_end) descr = {descr; loc = { loc_start; loc_end }}

%}

%start <modulefield list> module_

%%

heaptype:
| t = IDENT { try Hashtbl.find absheaptype_tbl t with Not_found -> Type t }

reftype:
| "&" nullable = boption("?") typ = heaptype
  { { nullable; typ } }

valtype:
| t = IDENT
   { try Hashtbl.find valtype_tbl t with Not_found ->
       raise (Misc.Syntax_error ($sloc, Printf.sprintf "Identifier '%s' is not a value type.\n" t )) }
| t = reftype { Ref t }

resulttype:
| "(" ")" { [] }
| t = valtype { [t] }
| "(" l = separated_nonempty_list(",", valtype) ")" { l }

functype:
| FN params = resulttype "->" result = resulttype { {params; result} }

storagetype:
| t = IDENT
   { try Hashtbl.find storagetype_tbl t with Not_found ->
       raise (Misc.Syntax_error ($sloc, Printf.sprintf "Identifier '%s' is not a storage type.\n" t )) }
| t = reftype { Value (Ref t) }

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
  "=" typ = comptype option(SEMI)
    { {name; typ; supertype; final = not op} }

rectype:
| REC "{" l = list(typedef) "}" { l }
| t = typedef { [t] }

simple_pat:
| x = IDENT { Some x }
| UNDERSCORE { None }

funcparams:
| l = separated_list(",", x = simple_pat ":" t = valtype { x, t })
  { l }

fundecl:
| FN name = IDENT
  t = ioption(":" t = IDENT { t } )
  sign = option ("(" named_params = funcparams ")"
  "->" result = resulttype { {named_params; result} })
  { (name, t, sign) }
| FN name = IDENT
  ":" params = resulttype "->" result = resulttype
  { (name, None, Some {named_params = List.map (fun x -> (None, x)) params; result}) }

func:
| f = fundecl body = block
  { let (name, typ, sign) = f in
    Func {name; typ; sign; body} }

%inline label: l = ioption("'" l = IDENT ":" { l }) { l }

%inline block:
| label = label "{" l = list(delimited_instr) "}" { (label, l) }

blockinstr:
| b = block { let (label, l) = b in with_loc $sloc (Block(label, l)) }
| label = label LOOP "{" l = list(delimited_instr) "}"
  { with_loc $sloc (Loop(label, l)) }
| label = label IF e = instr "{" l1 = list(delimited_instr) "}"
  l2 = option(ELSE  "{" l = list(delimited_instr) "}" { l })
  { with_loc $sloc (If(label, e, l1, l2)) }

plaininstr:
| NOP { with_loc $sloc Nop }
| UNREACHABLE { with_loc $sloc Unreachable }
| x = IDENT { with_loc $sloc (Get x) }
| x = IDENT "=" i = instr { with_loc $sloc (Set (x, i)) }
| "(" l = separated_list(",", instr) ")" { with_loc $sloc (Sequence l) }
| x = IDENT "(" l = separated_list(",", instr) ")"
   { with_loc $sloc (Call(x, l)) }
| s = STRING { with_loc $sloc (String s) }
| i = INT { with_loc $sloc (Int i) }
| x = IDENT "{" l = separated_list(",", y = IDENT ":" i = instr { (y, i) }) "}"
  { with_loc $sloc (Struct (Some x, l)) }
| "{" l = separated_nonempty_list(",", y = IDENT ":" i = instr { (y, i) }) "}"
  { with_loc $sloc (Struct (None, l)) }
| i = instr AS t = reftype { with_loc $sloc (Cast(i, t)) }
| i = instr "." x = IDENT { with_loc $sloc (StructGet(i, x)) }
| i = instr "." x = IDENT "=" j = instr { with_loc $sloc (StructSet(i, x, j)) }
| i = instr "+" j = instr { with_loc $sloc (BinOp(Plus, i, j)) }
| i = instr "-" j = instr { with_loc $sloc (BinOp(Minus, i, j)) }
| i = instr "<u" j = instr { with_loc $sloc (BinOp(Ltu, i, j)) }
| i = instr ">u" j = instr { with_loc $sloc (BinOp(Gtu, i, j)) }
| i = instr "&" j = instr { with_loc $sloc (BinOp(And, i, j)) }
| i = instr "|" j = instr { with_loc $sloc (BinOp(Or, i, j)) }
| LET x = IDENT t = option(":" t = valtype {t}) i = option("=" i = instr {i})
  { with_loc $sloc (Local (x, t, i)) }
| BR l = IDENT i = ioption(instr) { with_loc $sloc (Br (l, i)) } %prec br
| BR_IF l = IDENT i = instr { with_loc $sloc (Br_if (l, i)) } %prec br
| BR_TABLE "{" l = nonempty_list(IDENT) "}" i = instr
  { with_loc $sloc (Br_table (l, i)) } %prec br
| RETURN i = ioption(instr) { with_loc $sloc (Return i) } %prec br

instr:
| i = blockinstr { i }
| i = plaininstr { i }

delimited_instr:
| i = blockinstr { i }
| i = plaininstr SEMI { i }

global:
| LET name = IDENT
  typ = option(":" mut = boption(MUT) typ = valtype { {mut; typ} })
  "=" def = instr option(SEMI)
  { Global {name; typ; def} }

modulefield:
| r = rectype { Type r }
| f = fundecl option(SEMI)
  { let (name, typ, sign) = f in Fundecl {name; typ; sign} }
| f = func { f }
| g = global { g }

module_: l = list(modulefield) EOF { l }
