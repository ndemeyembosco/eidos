%{
open eidosAst
%}



%token LPAREN RPAREN LCBRACE RCBRACE SEMI EQUALS TERNARY OR AND
%token LESS GEQ PLUS MINUS TIMES DIVIDE MODULO COLON CARROT EXCLAIM
%token LBRACE RBRACE COMMA DOT DOLLAR NEQ EQ LEQ GEQ IF ELSE FOR IN DO
%token WHILE NEXT BREAK RETURN FUNCTION NULL LOGICAL FLOAT STRING OBJECT
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
      | interp_block1 interp_block11 { $2 :: $1}    (* reverse *)

interp_block11 :
        statement                  { Stmt $1}
      | fun_decl                   { FnDecl $1}

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
      IF LPAREN expr RPAREN statement estatement {IF ($3, $5, $6)}

estatement :
        (* empty *)                 {None}
      | ELSE statement              {Some $2}

for_stmt :
      FOR LPAREN LVAR IN expr RPAREN statement    {ForStmt ($3, $5, $7)}

do_while_stmt :
        DO statement WHILE LPAREN expr RPAREN SEMI {DoWhile ($2, $5)}

while_stmt  :
        WHILE LPAREN expr RPAREN statement     {WhileStmt ($3, $5)}

jump_stmt   :
        NEXT  SEMI                               {Next}
      | BREAK  SEMI                              {Break}
      | RETURN mexpr SEMI                        {Return $2}

mexpr  :
      (* empty *)                                {None}
    | expr                                       {Some $1}

expr   :  conditional_expr                       {E $1 }

assign_expr :
        conditional_expr        optAssign      {Assign ($1, $2)}

optAssign   :
        (* empty *)                             {None}
      | EQUALS conditional_expr                 {Some $2}

conditional_expr :
       l_or_expr optCond                        {Cond ($1, $2)}

optCond    :
       (* empty *)                                      {None }
    |  TERNARY conditional_expr ELSE conditional_expr   {Some ($2, $4)}

(* l_or_expr :
      l_and_expr *)

      (* function declarations *)

func_decl  :
      FUNCTION return_type_spec LVAR param_list compound_stmt    {(* TODO *)}





%%
