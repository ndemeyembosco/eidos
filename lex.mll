(* simple lexer.  A production version would not lex keywords
   specially. It would also hash variables. *)

(* Note: definition of variables _must_ follow keywords. *)

{
open Parse
}

let ws         = [' ' '\t' '\n' '\r']
let ident1     = ['a'-'z']|['A'-'Z']|'_'
let ident      = ident1 (['0'-'9']| ident1)*
let exp        = ['E' 'e']['+' '-']?['0'-'9']+
let decpart    = '.' ['0'-'9']*
let number     = ['0'-'9']+ decpart? exp?
let stringChar = ('\\' ['"' '\\' 'n' 'r' 't'] | [^'\"' '\\' '\n' '\r'])+
let stringLit  =  '\"' stringChar? '\"'

rule lexer = parse
  ws+   { lexer lexbuf }
| '{'   { LCBRACE }
| '}'   { RCBRACE }
| '('   { LPAREN }
| ')'   { RPAREN }
| '+'   { PLUS }
| '*'   { TIMES }
| '='  { EQUALS}
| ';'   { SEMI }
| "skip"   { LSkip }
| "if"     { IF }
| "else"     { ELSE }
| "while"  { WHILE }
| "pushheap" { LPushheap }
| "popheap"  { LPopheap  }
| "pushvar"  { LPushvar  }
| "popvar"   { LPopvar   }
| '-'? ['0'-'9']+ { LInt (int_of_string (Lexing.lexeme lexbuf)) }
| ident     { LVar (Lexing.lexeme lexbuf) }
| eof { LEof }
