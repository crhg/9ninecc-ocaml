{
    open Parser
}

let space = ['\t' '\n' '\r' ' ']

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

| '=' { ASSIGN }
| ';' { SEMI }
| ',' { COMMA }

| "return" { RETURN }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "int"    { INT }
| "sizeof" { SIZEOF }

| ['0'-'9']+ as num { NUM num }
| ['a'-'z']['a'-'z' '0'-'9']* as name { IDENT name }
| eof { EOF }

{
}
