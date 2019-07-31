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

| "return" { RETURN }
| "if"     { IF }
| "else"   { ELSE }

| ['0'-'9']+ as num { NUM num }
| ['a'-'z']['a'-'z' '0'-'9']* as name { IDENT name }
| eof { EOF }

{
}
