{
    open Parser
}

let space = ['\t' '\n' '\r' ' ']
let str = '"' (['\x20' - '\x7e'] # '"')* '"' 

rule token = parse
| space+ { token lexbuf }
| "//" { line_comment lexbuf }
| "/*" { block_comment lexbuf }

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
| ['_' 'a'-'z' 'A' - 'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as name { IDENT name }
| str as str { STR (String.sub str 1 (String.length str - 2)) }
| eof { EOF }

and line_comment = parse
| '\n' { token lexbuf }
| _    { line_comment lexbuf }

and block_comment = parse
| "*/" { token lexbuf }
| _    { block_comment lexbuf }

{
}
