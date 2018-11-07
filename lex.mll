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
let digit      = ['0'-'9']+
let number     = digit decpart? exp?
let stringChar = ('\\' ['"' '\\' 'n' 'r' 't'] | [^'\"' '\\' '\n' '\r'])+
let stringLit  =  '\"' stringChar? '\"'

rule lexer = parse
  ws+   { lexer lexbuf }
| '{'   { LCBRACE }
| '}'   { RCBRACE }
| '('   { LPAREN }
| ')'   { RPAREN }
| '['   { LBRACE }
| ']'   { RBRACE }
| '<'   { LESS }
| '>'   { GREATER }
| '+'   { PLUS }
| '-'   { MINUS }
| '*'   { TIMES }
| '/'   { DIVIDE }
| '%'   { MODULO }
| '^'   { CARROT }
| '!'   { EXCLAIM }
| '='   { EQUALS}
| '?'   { TERNARY }
| '|'   { OR }
| '&'   { AND }
| ':'   { COLON }
| ';'   { SEMI }
| ','   { COMMA }
| '.'   { DOT }
| '$'   { DOLLAR }
| "!="  { NEQ }
| "=="  { EQ }
| "<="  { LEQ }
| ">="  { GEQ }
| "if"  { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "in"  { IN }
| "do"  { DO }
| "break" { BREAK }
| "next" { NEXT }
| "return" { RETURN }
| digit { LInt (int_of_string (Lexing.lexeme lexbuf)) }
| ident     { LVar (Lexing.lexeme lexbuf) }
| stringLit { LStr (Lexing.lexeme lexbuf) }
| eof { LEof }
