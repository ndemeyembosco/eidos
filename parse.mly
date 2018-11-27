%{
open EidosAST

exception EOFLex

%}


%start interpreter_block
%type <EidosAST.interp_block> interpreter_block
%type <EidosAST.statement> statement
%type <EidosAST.expr> expr
%type <EidosAST.func_decl> func_decl

%token LPAREN RPAREN LCBRACE RCBRACE SEMI EQUALS TERNARY OR AND
%token LESS GEQ PLUS MINUS TIMES DIVIDE MODULO COLON CARROT EXCLAIM
%token LBRACE RBRACE COMMA DOT DOLLAR NEQ EQ LEQ GEQ IF ELSE FOR IN DO GREAT
%token WHILE NEXT BREAK RETURN FUNCTION VOID NULL LOGICAL INTEGER FLOAT STRING OBJECT
%token NUMERIC VOIDRV NULLRV LOGICALRV INTEGERRV FLOATRV STRINGRV OBJECTRV
%token EOF
%token <int> LInt
%token <float> LFloat
%token <string> LVar
%token <string> LStr

%left LPlus
%right LTimes
%left LSemi
%%

interpreter_block:
  EOF { Empty }
| statement interpreter_block { StmtInterp($1,$2) }
| func_decl interpreter_block { FuncInterp($1,$2) }

statement:
  compound_statement { Cstmt($1) }
| expr_statement { ExprStmt($1)}
| selection_statement { SlctStmt($1) }
| for_statement { For($1) }
| do_while_statement { Do($1) }
| while_statement { While($1) }
| jump_statement { Jump($1) }

compound_statement:
  LCBRACE multiple_statements RCBRACE { CmpdStmt($2) }

multiple_statements:
  statement { [$1] }
| multiple_statements statement{ $2::$1 }

expr_statement:
  SEMI { Estmt(None) }
| assignment_expr SEMI { Estmt(Some $1) }

selection_statement:
  IF LPAREN expr RPAREN statement { If($3,$5,None) }
| IF LPAREN expr RPAREN statement ELSE statement { If($3,$5,Some $7) }

for_statement:
  FOR LPAREN identifier IN expr RPAREN statement { ForStmt($3,$5,$7) }

do_while_statement:
  DO statement WHILE LPAREN expr RPAREN SEMI { DoWhile($2,$5) }

while_statement:
  WHILE LPAREN expr RPAREN statement { WhileStmt($3,$5) }

jump_statement:
  NEXT SEMI { Next}
| BREAK SEMI { Break }
| RETURN SEMI { Return(None) }
| RETURN expr SEMI { Return(Some $2) }

expr:
  conditional_expr { E($1) }

assignment_expr:
  conditional_expr { Assign($1,None) }
| conditional_expr EQUALS conditional_expr { Assign($1,Some $3)  }

conditional_expr:
  logical_or_expr { Cond($1,None)}
| logical_or_expr TERNARY conditional_expr ELSE conditional_expr { Cond($1,Some ($3,$5)) }

logical_or_expr:
  logical_and_expr_list { Lor($1) }

logical_and_expr_list:
  logical_and_expr { [$1] }
| logical_and_expr_list OR logical_and_expr { $3::$1 }

logical_and_expr:
  equality_expr_list { Land($1) }

equality_expr_list:
  equality_expr { [$1] }
| equality_expr_list AND equality_expr { $3::$1 }

equality_expr:
  relational_expr { Eqt($1,[]) }
| relational_expr relational_expr_list { Eqt($1, $2) }

relational_expr_list:
  NEQ relational_expr { [Neq($2)] }
| EQ relational_expr { [Eq($2)] }
| NEQ relational_expr relational_expr_list { Neq($2)::$3 }
| EQ relational_expr relational_expr_list { Eq($2)::$3 }

relational_expr:
  add_expr { Rel($1,None) }
| add_expr comparison_expr_list { Rel($1, Some $2) }

comparison_expr_list:
  LESS add_expr { [Less($2)] }
| LEQ add_expr { [Leq($2)] }
| GREAT add_expr { [Great($2)] }
| GEQ add_expr { [Geq($2)] }
| LESS add_expr comparison_expr_list { Less($2)::$3 }
| LEQ add_expr comparison_expr_list { Leq($2)::$3 }
| GREAT add_expr comparison_expr_list { Great($2)::$3 }
| GEQ add_expr comparison_expr_list { Geq($2)::$3 }

add_expr:
  mult_expr { Add($1,None) }
| mult_expr add_sub_list { Add($1,Some $2) }

add_sub_list:
  PLUS mult_expr { [Plus($2)] }
| MINUS mult_expr { [Minus($2)] }
| PLUS mult_expr add_sub_list { Plus($2)::$3 }
| MINUS mult_expr add_sub_list { Minus($2)::$3 }

mult_expr:
  seq_expr { Mult($1,None) }
| seq_expr mult_div_mod_list { Mult($1, Some $2) }

mult_div_mod_list:
  TIMES seq_expr { [Times($2)] }
| DIVIDE seq_expr { [Div($2)] }
| MODULO seq_expr { [Mod($2)] }
| TIMES seq_expr mult_div_mod_list { Times($2)::$3 }
| DIVIDE seq_expr mult_div_mod_list { Div($2)::$3 }
| MODULO seq_expr mult_div_mod_list { Mod($2)::$3 }

seq_expr:
  exp_expr { Seq($1,None)}
| exp_expr COLON exp_expr { Seq($1,Some $3) }

exp_expr:
  unary_expr { Eexpr($1,None)}
| unary_expr CARROT exp_expr { Eexpr($1, Some $3)}

unary_expr:
  postfix_expr { Post($1) }
| EXCLAIM unary_expr { ExclaimExpr($2) }
| PLUS unary_expr { PlusExpr($2) }
| MINUS unary_expr { NegExpr($2) }

/* postfix_expr */
/*need to make this recursive for expressions such as function().attribute;*/
postfix_expr:
  primary_expr { PE($1,None) }
| primary_expr postfix_opt { PE($1,Some $2) }

postfix_opt:
  function_call { FC($1,None) }
| attribute_accessor { AA($1,None) }
| indexing { Ind($1,None) }
| function_call postfix_opt { FC($1, Some $2) }
| attribute_accessor postfix_opt { AA($1, Some $2) }
| indexing postfix_opt { Ind($1, Some $2) }

function_call:
  LPAREN argument_expr_list RPAREN { FuncCall($2) }

attribute_accessor:
  DOT identifier { AttAcc($2) }

indexing:
  LBRACE RBRACE { Idx(None) }
| LBRACE conditional_expr_list RBRACE { Idx(Some $2) }

conditional_expr_list:
  COMMA { [] }
| COMMA conditional_expr_list { $2 }
| conditional_expr { [$1] }
| conditional_expr COMMA { [$1] }
| conditional_expr COMMA conditional_expr_list { $1::$3 }

primary_expr:
  constant { Const($1) }
| identifier { Ident($1) }
| LPAREN expr RPAREN { E($2) }

constant:
  LInt { ConstInt($1) }
| LFloat { ConstFloat($1) }
| LStr { ConstStr($1) }

identifier:
  LVar { $1 }

argument_expr:
  conditional_expr { C($1) }
| identifier EQUALS  conditional_expr { ArgSc($1,$3)}

argument_expr_list:
| argument_expr { [$1] }
| argument_expr COMMA argument_expr_list{ $1::$3 }

/* function declarations */

func_decl:
      FUNCTION return_type_spec identifier param_list compound_statement { Func($2,$3,$4,$5) }

return_type_spec:
      LPAREN type_spec RPAREN { RTySpec($2) }

type_spec:
       types_all                          { T($1) }
     | types_all DOLLAR                   { TDollar($1)}

types_all:
     VOID                            { Void }
     | NULL                          { Null }
     | LOGICAL                       { Logical }
     | INTEGER                       { Integer }
     | FLOAT                         { Float }
     | STRING                        { String }
     | OBJECT                        { Obj(None) }
     | OBJECT obj_cls_spec           { Obj(Some $2)}
     | NUMERIC                       { Numeric }
     | PLUS                          { PlusTy }
     | TIMES                         { TimesTy }
     | type_abbrv                    { TyList($1) }

type_abbrv:
       type_abbrv1                   { [$1] }
     | type_abbrv type_abbrv1        { $2::$1 }

type_abbrv1:
     VOIDRV                          { Void }
   | NULLRV                          { Null }
   | LOGICALRV                       { Logical }
   | INTEGERRV                       { Integer }
   | FLOATRV                         { Float }
   | STRINGRV                        { String }
   | OBJECTRV                        { Obj(None) }
   | OBJECTRV  obj_cls_spec          { Obj(Some $2) }


obj_cls_spec:
    LESS    identifier    GREAT       { OSpec($2) }

param_list:
      LPAREN VOID RPAREN { Void }
    | LPAREN param_list1 RPAREN       { Pspec($2) }

param_list1:
    | param_spec                    { [$1] }
    | param_spec COMMA param_list1  { $1::$3 }

param_spec:
    type_spec identifier  { PSpec($1,$2)}
  | LBRACE type_spec identifier EQUALS constant RBRACE  { PTySpecC($2,$3,$5)}
  | LBRACE type_spec identifier EQUALS identifier RBRACE  { PTySpecI($2,$3,$5)}

%%
