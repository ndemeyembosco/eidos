%{
open eidosAst
%}



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
      (* function declarations *)

func_decl  :
      FUNCTION return_type_spec LVAR param_list compound_stmt    {(* TODO *)}

return_type_spec :
      LPAREN type_spec RPAREN             {(* TODO *)}


type_spec  :
      types_all optEnd                    {(* TODO *)}

optEnd    :
     (* empty *)                          {(* TODO *)}
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
     (* empty *)                     {(* TODO *)}
     | obj_cls_spec                  {(* TODO *)}

type_abbrv        :
     type_abbrv                      {(* TODO *)}
     | type_abbrv1 type_abbrv        {(* TODO *)}

type_abbrv        :
     VOIDRV                          {(* TODO *)}
   | NULLRV                          {(* TODO *)}
   | LOGICALRV                       {(* TODO *)}
   | INTEGERRV                       {(* TODO *)}
   | FLOATRV                         {(* TODO *)}
   | STRINGRV                        {(* TODO *)}
   | OBJECTRV  opt_obj_cls_spec      {(* TODO *)}


obj_cls_spec       :
   LESS    LVAR    GEQ               {(* TODO *)}

param_list  :
    LPAREN  param_list1 RPAREN       {(* TODO *)}

param_list1 :
    VOID                             {(* TODO *)}
    | param_spec opt_param_spec1      {(* TODO *)}

opt_param_spec1 :
    (* empty *)                      {(* TODO *)}
   | opt_param_spec1 opt_param_spec  {(* TODO *)}

opt_param_spec :
   COMMA param_spec                   {(* TODO *)}


param_spec     :
    type_spec   LVAR                                 {(* TODO *)}
  | LBRACE type_spec LVAR EQUALS const_ident RBRACE  {(* TODO *)}


const_ident   :
     constant             {(* TODO *)}
  |  LVAR                 {(* TODO *)}









%%
