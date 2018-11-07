%start program
%type <unit> program

%token LPAREN RPAREN LCBRACE RCBRACE LBRACE RBRACE
%token DOLLAR TERNARY OR AND
%token PLUS MINUS TIMES DIVIDE MODULO
%token LESS GREATER NEQ EQ LEQ GEQ
%token COLON CARROT EXCLAIM EQUALS SEMI COMMA DOT IF ELSE WHILE LPushheap LPopheap LPushvar LPopvar
%token LEof
%token <int> LInt
%token <string> LVar
%token <string> LStr

%left PLUS
%right TIMES
%left SEMI
%%

statement:
 expr {}
| statement SEMI statement {}
| IF conditional_expr LCBRACE statement RCBRACE ELSE LCBRACE statement RCBRACE
    {}
| WHILE conditional_expr LCBRACE statement RCBRACE
    {}
| LCBRACE statement RCBRACE {}

primary_expr:
  LInt {}
| LStr {}
| LVar {}
| LPAREN conditional_expr RPAREN {}

argument_expr:
  conditional_expr {}
| LVar EQUALS  conditional_expr {}

argument_expr_list:
  argument_expr {}
| argument_expr COMMA argument_expr_list {}

/* postfix_expr */

function_execution:
  primary_expr LPAREN RPAREN {}

function_definition:
  primary_expr LPAREN argument_expr_list RPAREN {}

attribute_accessor:
  primary_expr DOT LVar {}

conditional_expr_list:
  conditional_expr {}
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

program:
  statement LEof {}

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
  assignment_expr {}
%%
