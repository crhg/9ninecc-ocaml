(* lexerのラッパ *)
open Token

type typedef_status = {
    found: int option;
    level: int;
    buf: Token.token list
}

let typedef_status = ref {
    found = None;
    level = 0;
    buf = []
}

let brace_buf = ref []

let rec token lexbuf =
    let t = token' lexbuf in
    Printf.fprintf stderr "token=%s\n" (Token.show_token t);
    t

and token' lexbuf =
    lexbuf |> (
        Lexer.token
        |> typedef_id_wrapper
        |> typedef_hack_wrapper
        |> brace_wrapper
    )

and typedef_id_wrapper get_token lexbuf =
    match get_token lexbuf with
    | IDENT name -> make_token name
    | token -> token

(*  typedefの次の{}外の;の次にint;を出力するhack *)
and typedef_hack_wrapper get_token lexbuf =
    match !typedef_status.buf with
    | token::rest ->
        typedef_status := { !typedef_status with buf = rest };
        token
    | [] ->
        let token = get_token lexbuf in
        typedef_status := next_typedef_status !typedef_status token;
        token

and next_typedef_status status token =
    match token with
    | TYPEDEF ->
        { status with found = Some status.level }
    | SEMI ->
        (match status with
        | { found = Some lv; level = level; _ } when lv = level ->
            { status with 
                buf = INT :: SEMI :: status.buf;
                found = None
            }
        | _ ->
            status
        )
    | LBRACE ->
        { status with level = status.level + 1 }
    | RBRACE ->
        { status with level = status.level - 1 }
    | _ ->
        status

and make_token name = 
    if Typedef_env.mem name then
        TYPEDEF_ID name
    else
        IDENT name

and brace_wrapper get_token lexbuf =
    match !brace_buf with
    | token::rest ->
        brace_buf := rest;
        token
    | [] ->
        (match get_token lexbuf with
        | LBRACE ->
            brace_buf := DUMMY :: !brace_buf;
            LBRACE
        | RBRACE ->
            brace_buf := RBRACE :: !brace_buf;
            DUMMY
        | token ->
            token
        )
