(* simple lexer.  A production version would not lex keywords
   specially. It would also hash variables. *)

(* Note: definition of variables _must_ follow keywords. *)

{
open Parse

exception Eof

}


let ws         = [' ' '\t' '\n' '\r']
let ident1     = ['a'-'z''A'-'Z''_']
let ident      = ident1 (['0'-'9']| ident1)*
let exp        = ['E''e']['+''-']?['0'-'9']+
let decpart    = '.'['0'-'9']*
let digit      = ['0'-'9']+
let number     = digit decpart? exp?
let stringChar = ('\\'['"''\\''n''r''t']|[^ '\n''\r''\\''\"'])+
let stringLit  =  '\"'stringChar?'\"' | "\'"stringChar?"\'"
let typespec_full = "logical"|"integer"|"float"|"string"|"object"|"numeric"
let typespec_abbr = "lifso"|"lifs"|"ifso"|"lif"|"lfs"|"lso"|"ifs"|"iso"|"fso"|"li"|"lf"|"ls"|"lo"|"is"|"fi"|"io"|"fs"|"fo"|"so"|"l"|"i"|"f"|"s"|"o"

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
| '>'        { GREAT }
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
| "void"     { VOID }
| "NULL"     { NULL }
| digit  { LInt (int_of_string (Lexing.lexeme lexbuf)) }
| number { LFloat (float_of_string (Lexing.lexeme lexbuf)) }
| typespec_full { LTypeF (Lexing.lexeme lexbuf) }
| typespec_abbr { LTypeA (Lexing.lexeme lexbuf) }
| ident     { LVar (Lexing.lexeme lexbuf) }
| stringLit { LStr (Lexing.lexeme lexbuf) }
| eof { EOF }
| _  {lexer lexbuf}
