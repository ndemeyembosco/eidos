%{
open Ast
%}

%start program

%type <Ast.stmt> statement program
%type <Ast.exp>  expression

%token LSkip LAssign LPushheap LPopheap LPushvar LPopvar
%token LPAREN RPAREN LCBRACE RCBRACE SEMI EQUALS TERNARY OR AND
%token LESS GEQ PLUS MINUS TIMES DIVIDE MODULO COLON CARROT EXCLAIM
%token LBRACE RBRACE COMMA DOT DOLLAR NEQ EQ LEQ GEQ IF ELSE FOR IN DO
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

expression:
  LInt { Int($1) }
| LVar { Var($1) }
| expression LPlus  expression { Plus($1,$3)  }
| expression LTimes expression { Times($1,$3) }
| LPAREN expression RPAREN { $2 }

statement:
  LSkip { Skip }
| LVar LAssign expression { Assign($1,$3) }
| statement LSemi statement { Seq($1,$3) }
| IF expression LPAREN statement RPAREN LPAREN statement RPAREN
    { If($2,$4,$7) }
| WHILE expression LPAREN statement RPAREN
    { While($2,$4) }
| LPAREN statement RPAREN {$2}

| LPushheap     { Pushheap }
| LPopheap LVar { Popheap($2) }
| LPushvar LVar { Pushvar($2) }
| LPopvar  LVar { Popvar($2)  }

program:
  statement LEof { $1 }

%%
