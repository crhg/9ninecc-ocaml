{
    open Pp_parser
    open Pp_token
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

    type state =
    | BeginOfLine
    | AfterSharp
    | Other

    let state = ref BeginOfLine

    let other x =
        state := Other;
        x

    (* 行頭の#の直後にだけ現れるトークンの処理 *)
    (* 直後の状態かどうかで返すトークンを切り替える *)
    (* XXX: 作ってすぐ捨てる時はもったいないが気にしない *)
    (* どの場合でも状態はOtherになる *)
    let after_sharp x y = 
        other @@ if !state = AfterSharp then x else y

}

let space = ['\t' ' ']
let nl = [ '\n' ]

rule token = parse
| space+ as wsp { WSP wsp }
| nl     { L.new_line lexbuf; state := BeginOfLine; NL }

| "//" { line_comment lexbuf }
| "/*" { block_comment false lexbuf }

| '#' {
    (if !state = BeginOfLine then state := AfterSharp);
    SHARP
}

| "define" as name {
    after_sharp DEFINE (ID name)
}

| ['0'-'9']+ as num { other(NUM num) }
| ['_' 'a'-'z' 'A' - 'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as name { other(ID name) }
| '"' { 
    let buf = B.create 100 in
    B.add_char buf '"';
    other(STR (string_literal buf lexbuf))
}
| ['\x21'-'\x7e'] { other(PUNCT (get lexbuf)) }
| eof { EOF }

and string_literal buf = parse
| [^ '"' '\\' '\n']+ {
    let s = get lexbuf in
    B.add_string buf s;
    string_literal buf lexbuf
}
| '\\' '"' {
    B.add_string buf @@ get lexbuf;
    string_literal buf lexbuf
}
| '\\' '\\' {
    B.add_string buf @@ get lexbuf;
    string_literal buf lexbuf
}
| '\\' [' ' - '\x7e'] {
    B.add_string buf @@ get lexbuf;
    string_literal buf lexbuf
}
| '"' { B.add_char buf '"'; B.contents buf }
| eof { error lexbuf "eof inside of string" }
| _ { error lexbuf "string?: '%s'" @@ get lexbuf }

(* 行コメントは必ず行末で終わるので改行と同じ *)
(* 改行で終わらないファイルの時おかしくなるので *)
(* かならず末尾にあらかじめ改行をつけておくこと *)
and line_comment = parse
| '\n' { L.new_line lexbuf; NL }
| _    { line_comment lexbuf }

(* ブロックコメントは中に改行を含んでいれば改行, 含まなければ空白とみなす *)
and block_comment new_line_found = parse
| "*/" { 
    if new_line_found then NL else WSP " "
}
| '\n' { L.new_line lexbuf; block_comment true lexbuf }
| _    { block_comment new_line_found lexbuf }

{
}
