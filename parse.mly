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
  IF LPAREN expr RPAREN compound_statement { If($3,$5,None) }
| IF LPAREN expr RPAREN compound_statement ELSE compound_statement { If($3,$5,Some $7) }

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
  relational_expr { Eqt($1,None) }
| relational_expr relational_expr_list { Eqt($1, Some $2) }

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
| add_expr LESS comparison_expr_list { Less($1)::$3 }
| add_expr LEQ comparison_expr_list { Leq($1)::$3 }
| add_expr GREAT comparison_expr_list { Great($1)::$3 }
| add_expr GEQ comparison_expr_list { Geq($1)::$3 }

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
  exp_expr {}
| exp_expr COLON exp_expr {}

exp_expr:
  unary_expr {}
| exp_expr CARROT unary_expr {}

unary_expr:
  postfix_expr {}
| EXCLAIM unary_expr {}
| PLUS unary_expr {}
| MINUS unary_expr {}

/* postfix_expr */

postfix_expr:
  primary_expr {}
| function_execution {}
| function_definition {}
| attribute_accessor {}
| indexing {}

function_execution:
  primary_expr LPAREN RPAREN {}

function_definition:
  primary_expr LPAREN argument_expr_list RPAREN {}

attribute_accessor:
  primary_expr DOT identifier {}

indexing:
  primary_expr LBRACE RBRACE {}
| primary_expr LBRACE conditional_expr_list RBRACE {}

conditional_expr_list:
  conditional_expr {}
| conditional_expr COMMA {}
| conditional_expr COMMA conditional_expr_list {}

constant:
  LInt { print_string ("Integer = "^(string_of_int $1)^"\n") }
| LFloat { print_string ("Float = "^(string_of_float $1)^"\n")}
| LStr { }

identifier:
  LVar { print_string ("Identifier = "^$1^"\n") }

primary_expr:
  constant {}
| identifier {}
| LPAREN expr RPAREN {}

argument_expr:
  conditional_expr {}
| identifier EQUALS  conditional_expr {}

argument_expr_list:
  argument_expr {}
| argument_expr_list COMMA argument_expr{}

/* function declarations */

func_decl  :
      FUNCTION return_type_spec identifier param_list compound_statement    {(* TODO *)}

return_type_spec :
      LPAREN type_spec RPAREN             {(* TODO *)}

type_spec  :
       types_all                          {(* TODO *)}
     | types_all DOLLAR                   {(* TODO *)}

types_all   :
     VOID                            {(* TODO *)}
     | NULL                          {(* TODO *)}
     | LOGICAL                       {(* TODO *)}
     | INTEGER                       {(* TODO *)}
     | FLOAT                         {(* TODO *)}
     | STRING                        {(* TODO *)}
     | OBJECT                        {(* TODO *)}
     | OBJECT obj_cls_spec           {(* TODO *)}
     | NUMERIC                       {(* TODO *)}
     | PLUS                          {(* TODO *)}
     | TIMES                         {(* TODO *)}
     | type_abbrv                    {(* TODO *)}

type_abbrv        :
       type_abbrv1                   {(* TODO *)}
     | type_abbrv type_abbrv1        {(* TODO *)}

type_abbrv1        :
     VOIDRV                          {(* TODO *)}
   | NULLRV                          {(* TODO *)}
   | LOGICALRV                       {(* TODO *)}
   | INTEGERRV                       {(* TODO *)}
   | FLOATRV                         {(* TODO *)}
   | STRINGRV                        {(* TODO *)}
   | OBJECTRV                        {(* TODO *)}
   | OBJECTRV  obj_cls_spec          {(* TODO *)}


obj_cls_spec       :
    LESS    identifier    GREAT       {(* TODO *)}

param_list  :
    LPAREN param_list1 RPAREN       {(* TODO *)}

param_list1 :
    VOID                             {(* TODO *)}
    | param_spec                     {(* TODO *)}   
    | param_spec opt_param_spec     {(* TODO *)}

opt_param_spec :
   COMMA param_spec                   {(* TODO *)}

param_spec     :
    type_spec identifier                                   {(* TODO *)}
  | LBRACE type_spec identifier EQUALS const_ident RBRACE  {(* TODO *)}

const_ident   :
     constant             {(* TODO *)}
  |  identifier           {(* TODO *)}
%%
