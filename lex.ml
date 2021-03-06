(* lexerのラッパ *)
open Token

type typedef_status = {
    found: int option;
    level: int;
}

let typedef_status = ref {
    found = None;
    level = 0
}

let typedef_buf = Buffering_wrapper.make()
let brace_buf = Buffering_wrapper.make()
let insert_empty_buf = Buffering_wrapper.make()

let rec token lexbuf =
    let t = token' lexbuf in
    (* Printf.fprintf stderr "token=%s\n" (Token.show_token t); *)
    t

and token' lexbuf =
    lexbuf |> (
        Lexer.token
        |> typedef_id_wrapper
        |> typedef_hack_wrapper
        |> insert_empty_statement_wrapper
        |> brace_wrapper
    )

and typedef_id_wrapper get_token lexbuf =
    match get_token lexbuf with
    | IDENT name -> make_token name
    | token -> token

and make_token name = 
    if Typedef_env.mem name then
        TYPEDEF_ID name
    else
        IDENT name

(*  typedefの次の{}外の;の次にint;を出力するhack *)
and typedef_hack_wrapper get_token lexbuf =
    Buffering_wrapper.next typedef_buf
        (fun buf ->
            let token = get_token lexbuf in
            typedef_status := next_typedef_status !typedef_status buf token;
            token
        )

and next_typedef_status status buf token =
    match token with
    | TYPEDEF ->
        { status with found = Some status.level }
    | SEMI ->
        (match status with
        | { found = Some lv; level = level; _ } when lv = level ->
            Buffering_wrapper.push_list buf [INT;SEMI];
            { status with found = None }
        | _ ->
            status
        )
    | LBRACE ->
        { status with level = status.level + 1 }
    | RBRACE ->
        { status with level = status.level - 1 }
    | _ ->
        status

(* {の後ろと}の前にDUMMYを入れる *)
and brace_wrapper get_token lexbuf =
    Buffering_wrapper.next brace_buf
        (fun buf ->
            match get_token lexbuf with
            | LBRACE ->
                Buffering_wrapper.push buf DUMMY;
                LBRACE
            | RBRACE ->
                Buffering_wrapper.push buf RBRACE;
                DUMMY
            | token ->
                token
        )

and insert_empty_statement_wrapper get_token lexbuf =
    Buffering_wrapper.next insert_empty_buf
        (fun buf ->
            match get_token lexbuf with
            | RBRACE ->
                Buffering_wrapper.push buf RBRACE;
                SEMI
            | token ->
                token
        )
