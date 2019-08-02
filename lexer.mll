{
    open Parser
}

let space = ['\t' '\n' '\r' ' ']
let str = '"' (['\x20' - '\x7e'] # '"')* '"' 

rule token = parse
| space+ { token lexbuf }

| '+' { PLUS }
| '-' { MINUS }
| '*' { AST }
| '/' { SLASH }
| '&' { AMP }

| "<"  { LT }
| "<=" { LE }
| ">"  { GT }
| ">=" { GE }
| "==" { EQ }
| "!=" { NE }

| '(' { LPAR }
| ')' { RPAR }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACKET }
| ']' { RBRACKET }

| '=' { ASSIGN }
| ';' { SEMI }
| ',' { COMMA }

| "return" { RETURN }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "int"    { INT }
| "char"   { CHAR }
| "sizeof" { SIZEOF }

| ['0'-'9']+ as num { NUM num }
| ['a'-'z']['a'-'z' '0'-'9']* as name { IDENT name }
| str as str { STR str }
| eof { EOF }

{
}
