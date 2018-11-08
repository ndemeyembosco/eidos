%{
open EidosAST

exception EOFLex

%}


%start interpreter_block
/* %type <EidosAST.interp_block> interp_block */
%type <unit> interpreter_block


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
| multiple_statements statement{}

expr_statement:
  SEMI {}
| assignment_expr SEMI {}

selection_statement:
  IF LPAREN expr RPAREN compound_statement {}
| IF LPAREN expr RPAREN compound_statement ELSE compound_statement {}

for_statement:
  FOR LPAREN identifier IN expr RPAREN statement { print_string "Doing a FOR loop \n"}

do_while_statement:
  DO statement WHILE LPAREN expr RPAREN SEMI {}

while_statement:
  WHILE LPAREN expr RPAREN statement {print_string "Doing a WHILE loop \n"}

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
  LInt { print_string ("Integer = "^(string_of_int $1)^"\n") }
| LFloat { print_string ("Float = "^(string_of_float $1)^"\n")}
| LStr { }

identifier:
  LVar { print_string ("Identifier = "^$1^"\n") }

argument_expr:
  conditional_expr {}
| identifier EQUALS  conditional_expr {}

argument_expr_list:
  argument_expr {}
| argument_expr_list COMMA argument_expr{}

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
| exp_expr CARROT unary_expr {}

interpreter_block:
  EOF {}
| statement interpreter_block {}
| func_decl interpreter_block {}


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
| add_expr GREAT relational_expr {}
| add_expr GEQ relational_expr {}

equality_expr:
  relational_expr {}
| relational_expr NEQ equality_expr {}
| relational_expr EQ equality_expr {}

logical_and_expr:
  equality_expr {}
| logical_and_expr AND equality_expr {}

logical_or_expr:
  logical_and_expr {}
| logical_or_expr OR logical_and_expr {}

conditional_expr:
  logical_or_expr {}
| logical_or_expr TERNARY conditional_expr ELSE conditional_expr {}

assignment_expr:
  conditional_expr {print_string "Node of an assignment operation \n"}
| conditional_expr EQUALS conditional_expr { print_string ("Performed an assignment operation\n") }

expr:
  conditional_expr {}

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
