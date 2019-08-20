(* lexerのラッパ *)
open Token

(* let show token = match token with *)
(* | TYPEDEF -> "TYPEDEF" *)
(* | IDENT s -> "IDENT("^s^")" *)
(* | TYPEDEF_ID s -> "TYPEDEF_iD("^s^")" *)
(* | SEMI -> ";" *)
(* | DUMMY -> "DUMMY" *)
(* | _ -> "." *)

let delayed_name = ref None
let delay_count = ref 0

let rec token lexbuf =
    let t = token' lexbuf in
    Printf.fprintf stderr "token=%s\n" (Token.show_token t);
    t

and token' lexbuf =
    lexbuf |> (
        Lexer.token
        |> delay_id_wrapper
    )

and delay_id_wrapper get_token lexbuf =
    match !delayed_name with
    | None ->
        let token = get_token lexbuf in
        (match token with
        | IDENT name ->
            delayed_name := Some name;
            delay_count := 1;
            DUMMY
        | _ -> token
        )
    | Some name ->
        if !delay_count = 1 then (
            delayed_name := None;
            make_token name
        ) else (
            delay_count := !delay_count - 1;
            DUMMY
        )

and make_token name = 
    if Typedef_env.mem name then
        TYPEDEF_ID name
    else
        IDENT name
