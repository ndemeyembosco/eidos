%start interpreter_block
%type <unit> interpreter_block

%token LPAREN RPAREN LCBRACE RCBRACE LBRACE RBRACE
%token DOLLAR TERNARY OR AND
%token PLUS MINUS TIMES DIVIDE MODULO
%token LESS GREATER NEQ EQ LEQ GEQ
%token COLON CARROT EXCLAIM EQUALS SEMI COMMA DOT 
%token DO FOR IN NEXT BREAK RETURN IF ELSE WHILE 
%token LEof
%token <int> LInt
%token <string> LVar
%token <string> LStr

%left PLUS
%right TIMES
%left SEMI
%%

statement:
  compound_statement {}
| expr_statement {}
| selection_statement {}
| for_statement {}
| do_while_statement {}
| while_statement {}
| jump_statement {}

compound_statement:
  LCBRACE multiple_statements RCBRACE {}

multiple_statements:
  statement {}
| statement multiple_statements {} 

expr_statement:
  SEMI {}
| assignment_expr SEMI {}

selection_statement:
  IF LPAREN expr RPAREN compound_statement {}
| IF LPAREN expr RPAREN compound_statement ELSE compound_statement {}

for_statement:
  FOR LPAREN identifier IN expr RPAREN statement {}

do_while_statement:
  DO statement WHILE LPAREN expr RPAREN SEMI {}

while_statement:
  WHILE LPAREN expr RPAREN statement {}

jump_statement:
  NEXT SEMI {}
| BREAK SEMI {}
| RETURN SEMI {}
| RETURN expr SEMI {}

primary_expr:
  constant {}
| identifier {}
| LPAREN conditional_expr RPAREN {}

constant:
  LInt {}
| LStr {}

identifier:
  LVar {}

argument_expr:
  conditional_expr {}
| identifier EQUALS  conditional_expr {}

argument_expr_list:
  argument_expr {}
| argument_expr COMMA argument_expr_list {}

/* postfix_expr */

function_execution:
  primary_expr LPAREN RPAREN {}

function_definition:
  primary_expr LPAREN argument_expr_list RPAREN {}

attribute_accessor:
  primary_expr DOT identifier {}

conditional_expr_list:
  conditional_expr {}
| conditional_expr COMMA {}
| conditional_expr COMMA conditional_expr_list {}

indexing:
  primary_expr LBRACE RBRACE {}
| primary_expr LBRACE conditional_expr_list RBRACE {}

postfix_expr:
  primary_expr {}
| function_execution {}
| function_definition {}
| attribute_accessor {}
| indexing {}

unary_expr:
  postfix_expr {}
| EXCLAIM unary_expr {}
| PLUS unary_expr {}
| MINUS unary_expr {}

exp_expr:
  unary_expr {}
| unary_expr CARROT exp_expr {}

interpreter_block:
  LEof {}
| statement interpreter_block {}
| function_decl interpreter_block {}

function_decl:
  LVar LEof{}

seq_expr:
  exp_expr {}
| exp_expr COLON exp_expr {}

mult_expr:
  seq_expr {}
| seq_expr TIMES mult_expr {}
| seq_expr DIVIDE mult_expr {}
| seq_expr MODULO mult_expr {}

add_expr:
  mult_expr {}
| mult_expr PLUS add_expr {}
| mult_expr MINUS add_expr {}

relational_expr:
  add_expr {}
| add_expr LESS relational_expr {}
| add_expr LEQ relational_expr {}
| add_expr GREATER relational_expr {}
| add_expr GEQ relational_expr {}

equality_expr:
  relational_expr {}
| relational_expr NEQ equality_expr {}
| relational_expr EQ equality_expr {}

logical_and_expr:
  equality_expr {}
| equality_expr AND logical_and_expr {}

logical_or_expr:
  logical_and_expr {}
| logical_and_expr OR logical_or_expr {}

conditional_expr:
  logical_or_expr {}
| logical_or_expr TERNARY conditional_expr ELSE conditional_expr {}

assignment_expr:
  conditional_expr {}
| conditional_expr EQUALS conditional_expr {}

expr:
  conditional_expr {}
%%
