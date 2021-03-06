{
    open Token
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

let octal_digit = ['0' - '7']
let octal_digit3 = octal_digit octal_digit octal_digit | octal_digit octal_digit | octal_digit
let hexadecimal_digit = ['0' - '9' 'a' - 'f' 'A' - 'F']

rule token = parse
| space+ { token lexbuf }
| nl     { L.new_line lexbuf; token lexbuf }
| "//" { line_comment lexbuf }
| "/*" { block_comment lexbuf }
| '#' {
    (* Printf.fprintf stderr "sharp line\n"; *)
    (* let open Lexing in *)
    (* let pos = lexbuf.lex_start_p in *)
    (* (if pos.pos_cnum <> pos.pos_bol then *)
        (* Printf.fprintf stderr "cnum<>bol?: cnum=%d bol=%d\n" pos.pos_cnum pos.pos_bol); *)
    sharp_line (B.create 50) lexbuf
}


| '+' { PLUS }
| '-' { MINUS }
| '*' { AST }
| '/' { SLASH }
| '%' { MOD }
| '&' { AMP }
| '^' { XOR }
| '|' { OR }
| "&&" { LAND }
| "||" { LOR }
| "<<" { LSHIFT }
| ">>" { RSHIFT }
| '!' { NOT }
| '~' { TILDA }

| '.' { DOT }
| "->" { ARROW }

| '?' { QUESTION }
| ':' { COLON }

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

| '='  { ASSIGN }
| "+=" { PLUS_ASSIGN }
| "-=" { MINUS_ASSIGN }
| "*=" { AST_ASSIGN }
| "/=" { SLASH_ASSIGN }
| "%=" { MOD_ASSIGN }
| "&=" { AMP_ASSIGN }
| "^=" { XOR_ASSIGN }
| "|=" { OR_ASSIGN }
| "<<=" { LSHIFT_ASSIGN }
| "<<=" { RSHIFT_ASSIGN }

| "++" { PLUSPLUS }
| "--" { MINUSMINUS }

| ';' { SEMI }
| ',' { COMMA }
| "..." { DOTS }

| "return"  { RETURN }
| "if"      { IF }
| "else"    { ELSE }
| "while"   { WHILE }
| "do"      { DO }
| "for"     { FOR }
| "break"   { BREAK }
| "continue" { CONTINUE }
| "switch"  { SWITCH }
| "case"    { CASE }
| "default" { DEFAULT }
| "void"    { VOID }
| "long"    { LONG }
| "int"     { INT }
| "short"   { SHORT }
| "char"    { CHAR }
| "_Bool"   { BOOL }
| "sizeof"  { SIZEOF }
| "struct"  { STRUCT }
| "union"   { UNION }
| "enum"    { ENUM }
| "typedef" { TYPEDEF }
| "extern"  { EXTERN }
| "static"  { STATIC }

| ['0'-'9']+ as num { NUM num }
| ['_' 'a'-'z' 'A' - 'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as name { IDENT name }
| '"' { STR (string_literal (B.create 100) lexbuf) }

| "'" ([' ' - '\x7e'] # [ '\'' '\\'] as c) "'"  { 
    NUM (string_of_int @@ Char.code c)
}
| "'\\" (octal_digit3 as o) "'" {
    let o = "0o" ^ o in
    NUM (string_of_int @@ int_of_string o)
}
| "'\\x" (hexadecimal_digit+ as h) "'" {
    let h = "0x" ^ h in
    NUM (string_of_int @@ int_of_string h)
}
| "'\\" ([' ' - '\x7e'] as c) "'"  { 
    let c = match c with
        |'a' -> '\x07'
        |'b' -> '\x08'
        |'t' -> '\x09'
        |'n' -> '\n'
        |'v' -> '\x0b'
        |'f' -> '\x0c'
        |'r' -> '\r'
        |'e' -> '\x1b'
        |_   -> c in
    NUM (string_of_int @@ Char.code c)
}

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

and sharp_line buf = parse
| '\n' {
    let line = B.contents buf in
    let open Str in
    let open L in
    (if string_match (regexp (" \\([0-9]+\\) \"\\(.*\\)\"")) line 0 then (
        let lnum = int_of_string @@ matched_group 1 line in
        let fname = matched_group 2 line in
        let cur = lexbuf.lex_curr_p in
        let cur = { cur with pos_fname = fname; pos_lnum = lnum } in
        (* Printf.fprintf stderr "linemarker %d %s\n" lnum fname; *)
        lexbuf.lex_curr_p <- cur
    ) else (
        Printf.fprintf stderr "? #%s\n" line;
        L.new_line lexbuf
    ));

    token lexbuf
}
| _ { 
    B.add_string buf (get lexbuf);
    sharp_line buf lexbuf
}

{
}
