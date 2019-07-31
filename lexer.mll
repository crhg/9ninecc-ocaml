{
}

let space = ['\t' '\n' '\r' ' ']

rule token = parse
| space+ { token lexbuf }

| ['0'-'9']+ as num { Parser.NUM (num) }
| eof { Parser.EOF }

{
}
