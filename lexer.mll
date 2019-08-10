{
    open Parser
    module B = Buffer
    module L = Lexing

    let get = L.lexeme

    let position lexbuf =
        let p = lexbuf.L.lex_curr_p in
        Printf.sprintf "%s:%d:%d" 
        p.L.pos_fname p.L.pos_lnum (p.L.pos_cnum - p.L.pos_bol)

    exception Error of string
    let error lexbuf fmt = 
        Printf.kprintf (fun msg -> 
            raise (Error ((position lexbuf)^" "^msg))) fmt
}

let space = ['\t' ' ']
let nl = [ '\n' ]

rule token = parse
| space+ { token lexbuf }
| nl     { L.new_line lexbuf; token lexbuf }
| "//" { line_comment lexbuf }
| "/*" { block_comment lexbuf }

| '+' { PLUS }
| '-' { MINUS }
| '*' { AST }
| '/' { SLASH }
| '&' { AMP }

| '.' { DOT }
| "->" { ARROW }

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
| "long"   { LONG }
| "int"    { INT }
| "short"  { SHORT }
| "char"   { CHAR }
| "sizeof" { SIZEOF }
| "struct" { STRUCT }
| "union"  { UNION }

| ['0'-'9']+ as num { NUM num }
| ['_' 'a'-'z' 'A' - 'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as name { IDENT name }
| '"' { STR (string_literal (B.create 100) lexbuf) }
| eof { EOF }

and string_literal buf = parse
| [^ '"' '\\' '\n']+ {
    let s = get lexbuf in
    B.add_string buf s;
    string_literal buf lexbuf
}
| '\\' '"' {
    B.add_char buf '"';
    string_literal buf lexbuf
}
| '\\' '\\' {
    B.add_char buf '\\';
    string_literal buf lexbuf
}
| '\\' ['a' - 'z'] {
    let s = get lexbuf in
    let c = s.[1] in
    let r = match c with
        |'a' -> '\x07'
        |'b' -> '\x08'
        |'t' -> '\x09'
        |'n' -> '\n'
        |'v' -> '\x0b'
        |'f' -> '\x0c'
        |'r' -> '\r'
        |'e' -> '\x1b'
        |_   -> c in
    B.add_char buf r;
    string_literal buf lexbuf
}
| '"' { B.contents buf }
| eof { error lexbuf "eof inside of string" }
| _ { error lexbuf "string?: '%s'" @@ get lexbuf }

and line_comment = parse
| '\n' { L.new_line lexbuf; token lexbuf }
| _    { line_comment lexbuf }

and block_comment = parse
| "*/" { token lexbuf }
| '\n' { L.new_line lexbuf; block_comment lexbuf }
| _    { block_comment lexbuf }

{
}
