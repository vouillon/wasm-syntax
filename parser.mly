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
%token UNDERSCORE

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
  "=" typ = comptype
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
| label = label "{" l = list(instr) "}" { (label, l) }

blockinstr:
| b = block { let (label, l) = b in with_loc $sloc (Block(label, l)) }
| label = label LOOP "{" l = list(instr) "}"
  { with_loc $sloc (Loop(label, l)) }
| label = label IF "(" e = instr ")" "{" l1 = list(instr) "}"
  l2 = option(ELSE  "{" l = list(instr) "}" { l })
  { with_loc $sloc (If(label, e, l1, l2)) }

plaininstr:
| NOP { with_loc $sloc Nop }
| UNREACHABLE { with_loc $sloc Unreachable }
| x = IDENT { with_loc $sloc (Get x) }
| x = IDENT "=" i = instr { with_loc $sloc (Set (x, i)) }
| "(" l = separated_list(",", instr) ")" { with_loc $sloc (Sequence l) }
| x = IDENT "(" l = separated_list(",", instr) ")"
   { with_loc $sloc (Call(x, l)) }
| "&" x = IDENT { with_loc $sloc (RefFunc x) }
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
| LET x = IDENT t = option(":" t = valtype {t}) i = option("=" i = instr {i})
  { with_loc $sloc (Local (x, t, i)) }

instr:
| i = blockinstr { i }
| i = plaininstr { i }

global:
| LET name = IDENT
  typ = option(":" mut = boption(MUT) typ = valtype { {mut; typ} })
  "=" def = instr
  { Global {name; typ; def} }

modulefield:
| r = rectype { Type r }
| f = fundecl
  { let (name, typ, params, result) = f in Fundecl {name; typ; params; result} }
| f = func { f }
| g = global { g }

module_: l = list(modulefield) EOF { l }
