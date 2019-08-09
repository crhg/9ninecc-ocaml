open Printf
open Lexer
open Lexing
open String
open Misc


(* 文字列sの位置posを含む行を取り出す *)
(* 戻り値は (行の文字列(改行を含まない), 行番号(1-origin), 行の先頭からのposの位置(0-origin)) *)
let get_line s pos =
    let pos = min pos (length s) in
    let s = s ^ "<<EOF>>\n" in (* 番兵を兼ねたEOFマーク *)
    let rec get_line' cur line_no =
        let next = 1 + index_from s cur '\n' in (* 次の行の頭 *)
            if pos < next
                then (sub s cur (next-cur-1), line_no, pos-cur)
                else get_line' next (line_no+1) in
    get_line' 0 1

(* 文字列sをn回繰り返した文字列 *)
(* XXX: 富豪的!! *)
let rec repeat n s =
    if n == 0
        then ""
        else s ^ repeat (n-1) s

let compile filename source =
    let source = source ^ "\n" in
    let lexbuf = Lexing.from_string source in

    let error_at pos s =
        let (line, line_no, pos_in_line) = get_line source pos in
        fprintf stderr "%s:%d:%d: %s\n" filename line_no pos_in_line s;
        fprintf stderr "%s\n" line;
        fprintf stderr "%s^\n" (repeat pos_in_line " ")
    in

    let error_at_lex_pos s =
        let pos = (Lexing.lexeme_start lexbuf) in
        error_at pos s
    in

    try
        let ast = Parser.translation_unit Lexer.token lexbuf in
        Type_check.check ast;
        Gen.gen ast
    with
    | Failure s ->
        error_at_lex_pos s;
        exit (-1)
    | Parser.Error ->
        error_at_lex_pos "Parser.Error";
        exit (-1)
    | Error_at (message, loc) ->
        let pos = loc.pos_cnum in
        error_at pos message;
        exit (-1)

let compile_file filename =
    let file = open_in filename in
    let source = really_input_string file (in_channel_length file) in
    compile filename source

let verbose = ref false

let filenames = ref []

let spec = [
    ("-v", Arg.Set verbose, "Turn on verbose message");
    ("-s", Arg.String (compile "-"), "Source string");
]

let () =
    Arg.parse spec
        (fun s -> filenames := s :: !filenames)
        "Usage: 9ninecc -v filename ...";
    List.iter compile_file !filenames
