(* 文字列sの位置posを含む行を取り出す *)
(* 戻り値は (行の文字列(改行を含まない), 行番号(1-origin), 行の先頭からのposの位置(0-origin)) *)
let rec get_line s pos =
    let pos = min pos (String.length s) in
    let s = s ^ "<<EOF>>\n" in (* 番兵を兼ねたEOFマーク *)
    let rec get_line' cur line_no =
        let next = 1 + String.index_from s cur '\n' in (* 次の行の頭 *)
            if pos < next
                then (String.sub s cur (next-cur-1), line_no, pos-cur)
                else get_line' next (line_no+1) in
    get_line' 0 1

and process filename source parser lexer action=
    let source = source ^ "\n" in
    let lexbuf = Lexing.from_string source in

    let error_at pos s =
        let (line, line_no, pos_in_line) = get_line source pos in
        Printf.fprintf stderr "%s:%d:%d: %s\n" filename line_no pos_in_line s;
        Printf.fprintf stderr "%s\n" line;
        Printf.fprintf stderr "%s^\n" (String.make pos_in_line ' ')
    in

    let error_at_lex_pos s =
        let pos = (Lexing.lexeme_start lexbuf) in
        error_at pos s
    in

    try
        let ast = parser lexer lexbuf in
        action(ast)
    with
    (* | Failure s -> *)
    (*     error_at_lex_pos s; *)
    (*     exit (-1) *)
    | Parser.Error ->
        error_at_lex_pos "Parser.Error";
        exit (-1)
    | Misc.Error_at (message, loc) ->
        let pos = loc.pos_cnum in
        error_at pos message;
        Printf.fprintf stderr "%s\n" @@ Printexc.get_backtrace();
        exit (-1)
