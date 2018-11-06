%{
open eidosAst
%}


%start interp_block
%type <interp_block> interp_block


%token LPAREN RPAREN LCBRACE RCBRACE SEMI EQUALS TERNARY OR AND
%token LESS GEQ PLUS MINUS TIMES DIVIDE MODULO COLON CARROT EXCLAIM
%token LBRACE RBRACE COMMA DOT DOLLAR NEQ EQ LEQ GEQ IF ELSE FOR IN DO GREAT
%token WHILE NEXT BREAK RETURN FUNCTION VOID NULL LOGICAL INTEGER FLOAT STRING OBJECT
%token NUMERIC VOIDRV NULLRV LOGICALRV INTEGERRV FLOATRV STRINGRV OBJECTRV
%token LEof
%token <int> LInt
%token <string> LVar
%token <string> LStr

%left LPlus
%right LTimes
%left LSemi
%%

interp_block :
        LEof                       { I (None, EOF)}
      | interp_block1 LEof         { I (Some $1, EOF)}

interp_block1 :
       interp_block11                {[$1]}
      | interp_block1 interp_block11 { $2 :: $1 (* reverse *)}

interp_block11 :
        statement                  { Stmt $1}
      | func_decl                   { FnDecl $1}


compound_stmt  : LCBRACE opt_stmt  RCBRACE      {(* TODO *)}

opt_stmt     :
     /* empty */                  {(* TODO *)}
   | opt_stmt statement           {(* TODO *)}



statement :
        compound_stmt              {Cstmt $1 }
      | expr_stmt                  {Expr  $1 }
      | select_stmt                {Slct  $1 }
      | for_stmt                   {For   $1 }
      | do_while_stmt              {Do    $1 }
      | while_stmt                 {While $1 }
      | jump_stmt                  {Jump  $1 }

expr_stmt :
        SEMI                       {Estmt None}
      | assign_expr SEMI           {Estmt (Some $1)}

select_stmt :
      IF LPAREN expr RPAREN compound_stmt estatement {(* TODO *)}

estatement :
        /* empty */                     {(* TODO *)}
      | ELSE compound_stmt              {(* TODO *)}

for_stmt :
      FOR LPAREN LVar IN expr RPAREN statement    {ForStmt ($3, $5, $7)}

do_while_stmt :
        DO statement WHILE LPAREN expr RPAREN SEMI {DoWhile ($2, $5)}

while_stmt  :
        WHILE LPAREN expr RPAREN statement     {WhileStmt ($3, $5)}

jump_stmt   :
        NEXT  SEMI                               {Next}
      | BREAK  SEMI                              {Break}
      | RETURN mexpr SEMI                        {Return $2}

mexpr  :
      /* empty */                                {None}
    | expr                                       {Some $1}

expr   :  conditional_expr                       {E $1 }

assign_expr :
        conditional_expr        optAssign      {Assign ($1, $2)}

optAssign   :
        /* empty */                             {None}
      | EQUALS conditional_expr                 {Some $2}

conditional_expr :
       l_or_expr optCond                        {Cond ($1, $2)}

optCond    :
       /* empty */                                      {None }
    |  TERNARY conditional_expr ELSE conditional_expr   {Some ($2, $4)}

l_or_expr :
      l_and_expr  opt_l_and                      {(* TODO *)}

opt_l_and :
      /* empty */                                {(* TODO *)}
    | opt_l_and opt_l_and1                       {(* TODO *)}

opt_l_and1 : OR l_and_expr                        {(* TODO *)}

l_and_expr :
      equality_expr  opt_l_eq                    {(* TODO *)}


opt_l_eq  :
      /* empty */                                {(* TODO *)}
    | opt_l_eq opt_l_eq1                         {(* TODO *)}

opt_l_eq1 : AND  equality_expr                   {(* TODO *)}

equality_expr :
    relational_expr  opt_l_rel                   {(* TODO *)}

opt_l_rel  :
      /* empty */                                {(* TODO *)}
    | opt_l_rel opt_l_rel1                       {(* TODO *)}

opt_l_rel1 : eqt relational_expr                 {(* TODO *)}

eqt  :
      NEQ                                        {(* TODO *)}
    | EQ                                         {(* TODO *)}

relational_expr : add_expr opt_l_add             {(* TODO *)}

opt_l_add    :
          /* empty */                            {(* TODO *)}
       |  opt_l_add opt_l_add1                   {(* TODO *)}

opt_l_add1 :
      compT  add_expr                            {(* TODO *)}

compT      :
        LESS       {(* TODO *)}
       | LEQ        {(* TODO *)}
       | GREAT      {(* TODO *)}
       | GEQ        {(* TODO *)}

add_expr   :
       mult_expr   opt_mult_expr           {(* TODO *)}

opt_mult_expr :
      /* empty */                          {(* TODO *)}
    | opt_mult_expr opt_mult_expr1         {(* TODO *)}

opt_mult_expr1 :
      plus_minus  mult_expr                {(* TODO *)}

plus_minus :
      PLUS       {(* TODO *)}
    | MINUS      {(* TODO *)}

mult_expr  :
      seq_expr    opt_seq_expr             {(* TODO *)}

opt_seq_expr :
      /* empty */                          {(* TODO *)}
    | opt_seq_expr opt_seq_expr1             {(* TODO *)}

opt_seq_expr1 :
     seq_op   seq_expr                     {(* TODO *)}

seq_op   :
    TIMES         {(* TODO *)}
    | DIVIDE        {(* TODO *)}
    | MODULO        {(* TODO *)}


seq_expr : exp_expr   opt_exp_expr        {(* TODO *)}

opt_exp_expr :
         /* empty */                      {(* TODO *)}
         | COLON exp_expr                 {(* TODO *)}

exp_expr    : unary_expr  opt_exp         {(* TODO *)}

opt_exp     :
         /* empty */                      {(* TODO *)}
        | CARROT exp_expr                 {(* TODO *)}

unary_expr  :
        un_op   unary_expr                {(* TODO *)}
      | post_fix_expr                     {(* TODO *)}

un_op       :
       EXCLAIM         {(* TODO *)}
    |  PLUS            {(* TODO *)}
    |  MINUS           {(* TODO *)}

post_fix_expr :
     primary_expr opt_post_fix_expr     {(* TODO *)}


opt_post_fix_expr :
        /* empty */                             {(* TODO *)}
      | opt_post_fix_expr opt_post_fix_expr1    {(* TODO *)}

opt_post_fix_expr1 :
      LBRACE opt_expr opt_comma_expr  RBRACE      {(* TODO *)}
      | LPAREN RPAREN                             {(* TODO *)}
      | LPAREN argument_expr_list RPAREN          {(* TODO *)}
      | DOT LVar                                  {(* TODO *)}

opt_expr  :
      /* empty */         {(* TODO *)}
    | expr                {(* TODO *)}

opt_comma_expr :
     /* empty */                        {(* TODO *)}
    | opt_comma_expr opt_comma_expr1    {(* TODO *)}

opt_comma_expr1 : COMMA opt_expr        {(* TODO *)}

primary_expr    :
     LVar                 {(* TODO *)}
    | constant            {(* TODO *)}
    | LPAREN expr RPAREN  {(* TODO *)}

argument_expr_list   : argument_expr opt_argument_expr   {(* TODO *)}

opt_argument_expr  :
        /* empty */                           {(* TODO *)}
      | opt_argument_expr opt_argument_expr1  {(* TODO *)}

opt_argument_expr1 :
      COMMA argument_expr             {(* TODO *)}

argument_expr   :
      conditional_expr                {(* TODO *)}
    | LVar EQUALS conditional_expr    {(* TODO *)}

constant        :
    LInt              {(* TODO *)}
    | LStr              {(* TODO *)}


      /* function declarations */

func_decl  :
      FUNCTION return_type_spec LVar param_list compound_stmt    {(* TODO *)}

return_type_spec :
      LPAREN type_spec RPAREN             {(* TODO *)}


type_spec  :
      types_all optEnd                    {(* TODO *)}

optEnd    :
     /* empty */                          {(* TODO *)}
     | DOLLAR                               {(* TODO *)}

types_all   :
     VOID                            {(* TODO *)}
     | NULL                          {(* TODO *)}
     | LOGICAL                       {(* TODO *)}
     | INTEGER                       {(* TODO *)}
     | FLOAT                         {(* TODO *)}
     | STRING                        {(* TODO *)}
     | OBJECT    opt_obj_cls_spec    {(* TODO *)}
     | NUMERIC                       {(* TODO *)}
     | PLUS                          {(* TODO *)}
     | TIMES                         {(* TODO *)}
     | type_abbrv                    {(* TODO *)}

opt_obj_cls_spec  :
     /* empty */                     {(* TODO *)}
     | obj_cls_spec                  {(* TODO *)}

type_abbrv        :
     /* empty */                     {(* TODO *)}
     | type_abbrv type_abbrv1        {(* TODO *)}

type_abbrv1        :
     VOIDRV                          {(* TODO *)}
   | NULLRV                          {(* TODO *)}
   | LOGICALRV                       {(* TODO *)}
   | INTEGERRV                       {(* TODO *)}
   | FLOATRV                         {(* TODO *)}
   | STRINGRV                        {(* TODO *)}
   | OBJECTRV  opt_obj_cls_spec      {(* TODO *)}


obj_cls_spec       :
   LESS    LVar    GREAT               {(* TODO *)}

param_list  :
    LPAREN  param_list1 RPAREN       {(* TODO *)}

param_list1 :
    VOID                             {(* TODO *)}
    | param_spec opt_param_spec1      {(* TODO *)}

opt_param_spec1 :
    /* empty */                      {(* TODO *)}
   | opt_param_spec1 opt_param_spec  {(* TODO *)}

opt_param_spec :
   COMMA param_spec                   {(* TODO *)}


param_spec     :
    type_spec   LVar                                 {(* TODO *)}
  | LBRACE type_spec LVar EQUALS const_ident RBRACE  {(* TODO *)}


const_ident   :
     constant             {(* TODO *)}
  |  LVar                 {(* TODO *)}




%%
