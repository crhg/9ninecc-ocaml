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

| "<"  { LT }
| "<=" { LE }
| ">"  { GT }
| ">=" { GE }
| "==" { EQ }
| "!=" { NE }

| '(' { LPAR }
| ')' { RPAR }

| '=' { ASSIGN }
| ';' { SEMI }

| ['0'-'9']+ as num { NUM num }
| ['a'-'z'] as name { IDENT (String.make 1 name) }
| eof { EOF }

{
}
