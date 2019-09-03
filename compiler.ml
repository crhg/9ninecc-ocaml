(* ファイルfilenameの内容文字列sの位置posを含む行を取り出す *)
(* 戻り値は (行の文字列(改行を含まない), ファイル名, 行番号(1-origin), 行の先頭からのposの位置(0-origin)) *)
(* もしlinemarkerがみつかればそれを使います *)
let rec get_line filename s pos =
    let re_marker = Str.regexp "^# \\([0-9]+\\) \"\\(.*\\)\"" in
    let (filename, lno) = 
        try
            let marker_pos = Str.search_backward re_marker s pos in
            let marker_lno = int_of_string @@ Str.matched_group 1 s in
            let filename = Str.matched_group 2 s in
            let lno = marker_lno - 1 + count '\n' (String.sub s marker_pos (pos - marker_pos)) in
            (filename, lno)
        with
        | Not_found  ->
            (filename, 1 + count '\n' (String.sub s 0 pos)) in
    let (line_string, bol_pos) = pickup_line s pos in
    (line_string, filename, lno, pos - bol_pos)

(* 文字列sのなかのposを含む行の文字列(改行含まず)と行頭の位置を返す *)
and pickup_line s pos =
    let open Str in
    let bol = search_backward (regexp "^") s pos in
    let eol = search_forward (regexp "$") s pos in
    (String.sub s bol (eol-bol), bol)

(* 文字列sの中の文字cを数える *)
and count c s =
    let r = ref 0 in
    s |> String.iter (fun c' ->
        if c' = c then incr r
    );
    !r

and process filename source parser lexer action=
    let source = source ^ "\n" in
    let lexbuf = Lexing.from_string source in

    let error_at pos s =
        let (line, filename, line_no, pos_in_line) = get_line filename source pos in
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
