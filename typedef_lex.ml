(* lexerのラッパ *)
open Token

let typedef_found = ref None
let level = ref 0
let buf = ref []

let rec token lexbuf =
    let t = token' lexbuf in
    Printf.fprintf stderr "token=%s\n" (Token.show_token t);
    t

and token' lexbuf =
    lexbuf |> (
        Lexer.token
        |> typedef_id_wrapper
        |> typedef_hack_wrapper
    )

and typedef_id_wrapper get_token lexbuf =
    match get_token lexbuf with
    | IDENT name -> make_token name
    | token -> token

(*  typedefの次の{}外の;の次にint;を出力するhack *)
and typedef_hack_wrapper get_token lexbuf =
    match !buf with
    | token::rest ->
        buf := rest;
        token
    | [] -> typedef_hack @@ get_token lexbuf

and typedef_hack token =
    match token with
    | TYPEDEF ->
        typedef_found := Some !level;
        TYPEDEF
    | SEMI ->
        (match !typedef_found with
        | Some lv when lv = !level ->
            buf := INT :: SEMI :: !buf;
            typedef_found := None
        | _ -> ()
        );
        SEMI
    | LBRACE ->
        buf := DUMMY :: !buf;
        level := !level + 1;
        LBRACE
    | RBRACE ->
        level := !level - 1;
        buf := RBRACE :: !buf;
        DUMMY
    | _ ->
        token

and make_token name = 
    if Typedef_env.mem name then
        TYPEDEF_ID name
    else
        IDENT name
