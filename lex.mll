(* simple lexer.  A production version would not lex keywords
   specially. It would also hash variables. *)

(* Note: definition of variables _must_ follow keywords. *)

{
open Parse



}


let ws         = [' ' '\t' '\n' '\r']
let ident1     = ['a'-'z']|['A'-'Z']|'_'
let ident      = ident1 (['0'-'9']| ident1)*
let exp        = ['E''e']['+''-']?['0'-'9']+
let decpart    = '.'['0'-'9']*
let number     = ['0'-'9']+decpart?exp?
let stringChar = ('\\'['"''\\''n''r''t']|[^ '\n''\r''\\''\"'])+
let stringLit  =  '\"'stringChar?'\"'

rule lexer = parse
  ws+   { lexer lexbuf }
| '{'        { LCBRACE }
| '}'        { RCBRACE }
| ';'        { SEMI }
| '('        { LPAREN }
| ')'        { RPAREN }
| '='        { EQUALS }
| '?'        { TERNARY }
| '|'        { OR }
| '&'        { AND }
| '<'        { LESS }
| '>'        { GEQ }
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE }
| '%'        { MODULO }
| ':'        { COLON }
| '^'        { CARROT }
| '!'        { EXCLAIM }
| '['        { LBRACE }
| ']'        { RBRACE }
| ','        { COMMA }
| '.'        { DOT }
| '$'        { DOLLAR }
| "!="       { NEQ }
| "=="       { EQ }
| "<="       { LEQ }
| ">="       { GEQ }
| "if"       { IF }
| "else"     { ELSE }
| "for"      { FOR }
| "in"       { IN }
| "do"       { DO }
| "while"    { WHILE }
| "next"     { NEXT }
| "break"    { BREAK }
| "return"   { RETURN }
| "function" { FUNCTION }
| "NULL"     { NULL }
| "logical"  { LOGICAL }
| "float"    { FLOAT }
| "string"   { STRING }
| "object"   { OBJECT }
| "numeric"  { NUMERIC }
| "v"        { VOIDRV }
| "N"        { NULLRV }
| "l"        { LOGICALRV }
| "i"        { INTEGERRV }
| "f"        { FLOATRV }
| "s"        { STRINGRV }
| "o"        { OBJECTRV }
| number { LInt (int_of_string (Lexing.lexeme lexbuf)) }
| ident     { LVar (Lexing.lexeme lexbuf) }
| stringLit {LStr (Lexing.lexeme lexbuf)}
| eof { LEof }
