%{
open Ast
%}

%start program

%type <Ast.stmt> statement program
%type <Ast.exp>  expression

%token LPAREN RPAREN LCBRACE RCBRACE
%token PLUS TIMES
%token LSkip EQUALS SEMI IF ELSE WHILE LPushheap LPopheap LPushvar LPopvar
%token LEof
%token <int> LInt
%token <string> LVar

%left PLUS
%right TIMES
%left SEMI
%%

expression:
  LInt { Int($1) }
| LVar { Var($1) }
| expression PLUS  expression { Plus($1,$3)  }
| expression TIMES expression { Times($1,$3) }
| LPAREN expression RPAREN { $2 }

statement:
  LSkip { Skip }
| LVar EQUALS expression { Assign($1,$3) }
| statement SEMI statement { Seq($1,$3) }
| IF expression LCBRACE statement RCBRACE ELSE LCBRACE statement RCBRACE
    { If($2,$4,$8) }
| WHILE expression LCBRACE statement RCBRACE
    { While($2,$4) }
| LCBRACE statement RCBRACE {$2}

| LPushheap     { Pushheap }
| LPopheap LVar { Popheap($2) }
| LPushvar LVar { Pushvar($2) }
| LPopvar  LVar { Popvar($2)  }

program:
  statement LEof { $1 }

%%
