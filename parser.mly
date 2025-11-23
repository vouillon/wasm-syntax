%token <string> IDENT
%token <string> INT
%token <string> FLOAT
%token <string> STRING

%token EOF

%token SEMI ";"
%token SHARP "#"
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
%token FATARROW "=>"
%token EQUAL "="
%token COLONEQUAL ":="
%token QUOTE "'"
%token DOT "."
%token BANG "!"
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token SLASHS "/s"
%token SLASHU "/u"
%token PERCENTS "%s"
%token PERCENTU "%u"
%token AMPERSAND "&"
%token PIPE "|"
%token CARET "^"
%token SHL "<<"
%token SHRS ">>s"
%token SHRU ">>u"
%token EQUALEQUAL "=="
%token BANGEQUAL "!="
%token GT ">"
%token GTS ">s"
%token GTU ">u"
%token LT "<"
%token LTS "<s"
%token LTU "<u"
%token GE ">="
%token GES ">=s"
%token GEU ">=u"
%token LE "<="
%token LES "<=s"
%token LEU "<=u"
%token UNDERSCORE "_"

%token FN
%token TAG
%token MUT
%token TYPE
%token REC
%token OPEN
%token NOP UNREACHABLE NULL
%token DO LOOP IF ELSE
%token CONST LET AS IS
%token BR BR_IF BR_TABLE RETURN THROW
%token BR_ON_CAST BR_ON_CAST_FAIL
%token BR_ON_NULL BR_ON_NON_NULL

%nonassoc LET
%nonassoc IDENT prec_branch RBRACE
%nonassoc LBRACE LBRACKET
%right EQUAL COLONEQUAL
%right QUESTIONMARK COLON
%nonassoc EQUALEQUAL BANGEQUAL GT GTU GTS LT LTU LTS GE GES GEU LE LES LEU
%left PIPE
%left CARET
%left AMPERSAND
%left SHL SHRS SHRU
%left MINUS PLUS
%left STAR SLASH SLASHS SLASHU PERCENTS PERCENTU
%left AS IS
%right prec_unary BANG
%left DOT LPAREN
%left SHARP
(* BR foo 1 + 2 understood as BR foo (1 + 2)
   BR_TABLE { ...} 1 + 2 understood as BR_TABLE { ...} (1 + 2)
   BR foo { ... } understood as a single instruction
   LET x = br foo LET --> LET x = (br foo) LET  (LET < branch)
   IF BR_TABLE {...} { ... } --> IF (BR_TABLE {...}) { ... } (br < LBRACE)
   { ... } ( ... } --> sequence (instr < LPAREN)
   {a| b:}  ==> struct; {a|b} should need parenthesis <<< special case??
                            (IDENT < PIPE)
*)

%{
open Ast

let tbl_from_list l =
 let h = Hashtbl.create (2 * List.length l) in
 List.iter (fun (k, v) -> Hashtbl.add h k v) l;
 h

let absheaptype_tbl =
  tbl_from_list
    ["func", (Func : heaptype);
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
    ["i32", (I32 : valtype); "i64", I64; "f32", F32; "f64", F64; "v128", V128]

let storagetype_tbl =
  tbl_from_list
    ["i8", (Packed I8 : storagetype); "i16", Packed I16;
     "i32", Value I32; "i64", Value I64; "f32", Value F32; "f64", Value F64;
     "v128", Value V128]

let with_loc (loc_start, loc_end) descr = {descr; loc = { loc_start; loc_end }}

%}

%start <modulefield list> parse

%%

%inline ident:
| t = IDENT { with_loc $sloc t }

heaptype:
| t = ident { try Hashtbl.find absheaptype_tbl t.descr with Not_found -> Type t }

reftype:
| "&" nullable = boption("?") typ = heaptype
  { { nullable; typ } }

valtype:
| t = IDENT
   { try Hashtbl.find valtype_tbl t with Not_found ->
       raise (Wasm.Parsing.Syntax_error ($sloc, Printf.sprintf "Identifier '%s' is not a value type.\n" t )) }
| t = reftype { Ref t }

resulttype:
| "(" ")" { [] }
| t = valtype { [t] }
| "(" l = separated_nonempty_list(",", valtype) ")" { l }

functype:
| FN "(" params = separated_nonempty_list(",", valtype) ")" "->"
  result = resulttype
  { {params = Array.of_list params; results = Array.of_list result} }

storagetype:
| t = IDENT
   { try Hashtbl.find storagetype_tbl t with Not_found ->
       raise (Wasm.Parsing.Syntax_error ($sloc, Printf.sprintf "Identifier '%s' is not a storage type.\n" t )) }
| t = reftype { Value (Ref t) }

fieldtype:
| mut = boption(MUT) typ = storagetype { {mut; typ } }

structtype:
| "{" l = separated_list(",", x = ident ":" t = fieldtype { (x, t) } ) "}"
  { Array.of_list l }

arraytype:
| "[" t = fieldtype "]" { t }

comptype:
| t = structtype { Struct t }
| t = functype { Func t }
| t = arraytype { Array t }

typedef:
| TYPE name = ident
  supertype = option(":" s = ident { s })
  "=" op = boption(OPEN) typ = comptype option(";")
    { (name, {typ; supertype; final = not op}) }

rectype:
| REC "{" l = list(typedef) "}" { Array.of_list l }
| t = typedef { [|t|] }

attribute:
| "#" "[" name = IDENT "=" i = instr "]" { (name, i) }

simple_pat:
| x = ident { Some x }
| "_" { None }

funcparams:
| l = separated_list(",", x = simple_pat ":" t = valtype { x, t })
  { l }

fundecl:
| FN name = ident
  t = ioption(":" t = ident { t } )
  sign = option ("(" named_params = funcparams ")"
  "->" results = resulttype { {named_params; results} })
  { (name, t, sign) }

func:
| attributes = list(attribute) f = fundecl body = block
  { let (name, typ, sign) = f in
    Func {name; typ; sign; body; attributes} }

tag:
| TAG name = ident
  t = ioption(":" t = ident { t } )
  sign = option ("(" named_params = funcparams ")"
  "->" results = resulttype { {named_params; results} })
  { (name, t, sign) }

%inline label: l = ioption("'" l = IDENT ":" { l }) { l }

blocktype:
| "(" separated_list(",", valtype) ")" "->" resulttype { () }
| valtype { () }

%inline block:
| label = label "{" l = delimited_instr_list "}" { (label, l) }

%inline blockinstr:
| b = block { let (label, l) = b in with_loc $sloc (Block(label, l)) }
| label = label DO option(blocktype) "{" l = delimited_instr_list "}"
  { with_loc $sloc (Block(label, l)) }
| label = label LOOP option(blocktype) "{" l = delimited_instr_list "}"
  { with_loc $sloc (Loop(label, l)) }
| label = label IF e = instr option("=>" blocktype {()})
  "{" l1 = delimited_instr_list "}"
  l2 = option(ELSE  "{" l = delimited_instr_list "}" { l })
  { with_loc $sloc (If(label, e, l1, l2)) }

plaininstr:
| NOP { with_loc $sloc Nop }
| UNREACHABLE { with_loc $sloc Unreachable }
| NULL { with_loc $sloc Null }
| x = ident { with_loc $sloc (Get x) }
| x = ident "=" i = instr { with_loc $sloc (Set (x, i)) }
| x = ident ":=" i = instr { with_loc $sloc (Tee (x, i)) }
| "(" l = separated_list(",", instr) ")" { with_loc $sloc (Sequence l) }
| i = instr "(" l = separated_list(",", instr) ")"
   { with_loc $sloc (Call(i, l)) }
| t  = option(t = ident "#" { t }) s = STRING { with_loc $sloc (String (t, s)) }
| i = INT { with_loc $sloc (Int i) }
| f = FLOAT { with_loc $sloc (Float f) }
| "{" x = ident "|" l = separated_list(",", y = ident ":" i = instr { (y, i) }) "}"
  { with_loc $sloc (Struct (Some x, l)) }
| "{" l = separated_nonempty_list(",", y = ident ":" i = instr { (y, i) }) "}"
  { with_loc $sloc (Struct (None, l)) }
| i = instr AS t = valtype { with_loc $sloc (Cast(i, t)) }
| i = instr IS t = reftype { with_loc $sloc (Test(i, t)) }
| i = instr "." x = ident { with_loc $sloc (StructGet(i, x)) }
| i = instr "." x = ident "=" j = instr { with_loc $sloc (StructSet(i, x, j)) }
| i = instr "+" j = instr { with_loc $sloc (BinOp(Add, i, j)) }
| i = instr "-" j = instr { with_loc $sloc (BinOp(Sub, i, j)) }
| i = instr "*" j = instr { with_loc $sloc (BinOp(Mul, i, j)) }
| i = instr "/" j = instr { with_loc $sloc (BinOp(Div None, i, j)) }
| i = instr "/s" j = instr { with_loc $sloc (BinOp(Div (Some Signed), i, j)) }
| i = instr "/u" j = instr { with_loc $sloc (BinOp(Div (Some Unsigned), i, j)) }
| i = instr "%s" j = instr { with_loc $sloc (BinOp(Rem Signed, i, j)) }
| i = instr "%u" j = instr { with_loc $sloc (BinOp(Rem Unsigned, i, j)) }
| i = instr "&" j = instr { with_loc $sloc (BinOp(And, i, j)) }
| i = instr "^" j = instr { with_loc $sloc (BinOp(Xor, i, j)) }
| i = instr "|" j = instr { with_loc $sloc (BinOp(Or, i, j)) }
| i = instr "<<" j = instr { with_loc $sloc (BinOp(Shl, i, j)) }
| i = instr ">>s" j = instr { with_loc $sloc (BinOp(Shr Signed, i, j)) }
| i = instr ">>u" j = instr { with_loc $sloc (BinOp(Shr Unsigned, i, j)) }
| i = instr "==" j = instr { with_loc $sloc (BinOp(Eq, i, j)) }
| i = instr "!=" j = instr { with_loc $sloc (BinOp(Ne, i, j)) }
| i = instr ">" j = instr { with_loc $sloc (BinOp(Gt None, i, j)) }
| i = instr ">s" j = instr { with_loc $sloc (BinOp(Gt (Some Signed), i, j)) }
| i = instr ">u" j = instr { with_loc $sloc (BinOp(Gt (Some Unsigned), i, j)) }
| i = instr "<" j = instr { with_loc $sloc (BinOp(Lt None, i, j)) }
| i = instr "<s" j = instr { with_loc $sloc (BinOp(Lt (Some Signed), i, j)) }
| i = instr "<u" j = instr { with_loc $sloc (BinOp(Lt (Some Unsigned), i, j)) }
| i = instr ">=" j = instr { with_loc $sloc (BinOp(Ge None, i, j)) }
| i = instr ">=s" j = instr { with_loc $sloc (BinOp(Ge (Some Signed), i, j)) }
| i = instr ">=u" j = instr { with_loc $sloc (BinOp(Ge (Some Unsigned), i, j)) }
| i = instr "<=" j = instr { with_loc $sloc (BinOp(Le None, i, j)) }
| i = instr "<=s" j = instr { with_loc $sloc (BinOp(Le (Some Signed), i, j)) }
| i = instr "<=u" j = instr { with_loc $sloc (BinOp(Le (Some Unsigned), i, j)) }
| LET x = simple_pat
  { with_loc $sloc (Let ([(x, None)], None)) }
| LET x = simple_pat "=" i = instr
  { with_loc $sloc (Let ([(x, None)], Some i)) }
| LET x = simple_pat ":" t = valtype i = option("=" i = instr {i})
  { with_loc $sloc (Let ([(x, Some t)], i)) }
| LET
  "(" l = separated_list(",", p = simple_pat t = option(":" t = valtype {t})
                                { (p, t) })
  ")" i = option("=" i = instr {i})
  { with_loc $sloc (Let (l, i)) }
| BR "'" l = IDENT i = ioption(instr)
  { with_loc $sloc (Br (l, i)) } %prec prec_branch
| BR_IF "'" l = IDENT i = instr
  { with_loc $sloc (Br_if (l, i)) } %prec prec_branch
| BR_TABLE "{" l = nonempty_list("'" i = IDENT { i }) "}" i = instr
  { with_loc $sloc (Br_table (l, i)) } %prec prec_branch
| BR_ON_NULL "'" l = IDENT i = instr { with_loc $sloc (Br_on_null (l, i)) } %prec prec_branch
| BR_ON_NON_NULL "'" l = IDENT i = instr { with_loc $sloc (Br_on_non_null (l, i)) } %prec prec_branch
| BR_ON_CAST "'" l = IDENT t = reftype i = instr { with_loc $sloc (Br_on_cast (l, t, i)) }
| BR_ON_CAST_FAIL "'" l = IDENT t = reftype i = instr { with_loc $sloc (Br_on_cast_fail (l, t, i)) }
| RETURN i = ioption(instr) { with_loc $sloc (Return i) } %prec prec_branch
| THROW t = ident  "(" l = separated_list(",", instr) ")"
  { with_loc $sloc (Throw (t, l)) }
| "[" l = separated_list(",", i = instr { i }) "]"
  { with_loc $sloc (ArrayFixed (None, l)) }
| "[" i1 = instr ";" i2 = instr "]"
  { with_loc $sloc (Array (None, i1, i2)) }
| "[" t = ident "|" l = separated_list(",", i = instr { i }) "]"
  { with_loc $sloc (ArrayFixed (Some t, l)) }
| "[" t = ident "|" i1 = instr ";" i2 = instr "]"
  { with_loc $sloc (Array (Some t, i1, i2)) }
| i1 = instr "[" i2 = instr "]" { with_loc $sloc (ArrayGet (i1, i2)) }
| i1 = instr "[" i2 = instr "]" "=" i3 = instr
  { with_loc $sloc (ArraySet (i1, i2, i3)) }
| i1 = instr "?" i2 = instr ":" i3 = instr
  { with_loc $sloc (Select (i1, i2, i3)) }
| "!" i = instr { with_loc $sloc (UnOp (Not, i)) } %prec prec_unary
| "+" i = instr { with_loc $sloc (UnOp (Pos, i)) } %prec prec_unary
| "-" i = instr { with_loc $sloc (UnOp (Neg, i)) } %prec prec_unary
| i = instr "!" { with_loc $sloc (NonNull i) }

instr:
| i = blockinstr { i }
| i = plaininstr { i }

delimited_instr_list:
| { [] }
| i = plaininstr { [i] }
| i = blockinstr l = delimited_instr_list { i :: l }
| i = plaininstr ";" l = delimited_instr_list { i :: l }

globalmut:
| LET { true }
| CONST { false }

global:
| attributes = list(attribute) mut = globalmut name = ident
  typ = option(":" typ = valtype { typ })
  "=" def = instr option(";")
  { Global {name; mut; typ; def; attributes} }

globaldecl:
| attributes = list(attribute) mut = globalmut name = ident
  ":" typ = valtype
  { GlobalDecl {name; mut; typ; attributes} }

modulefield:
| r = rectype { Type r }
| attributes = list(attribute) f = fundecl option(";")
  { let (name, typ, sign) = f in Fundecl {name; typ; sign; attributes} }
| f = func { f }
| g = global { g }
| g = globaldecl { g }
| attributes = list(attribute) f = tag option(";")
  { let (name, typ, sign) = f in Tag {name; typ; sign; attributes} }

parse: l = list(modulefield) EOF { l }
