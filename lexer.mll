{
    open Parser
}

let space = ['\t' '\n' '\r' ' ']

rule token = parse
| space+ { token lexbuf }

| '+' { PLUS }
| '-' { MINUS }

| ['0'-'9']+ as num { Parser.NUM (num) }
| eof { Parser.EOF }

{
}
