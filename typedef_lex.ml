(* lexerのラッパ *)
open Token

(* let show token = match token with *)
(* | TYPEDEF -> "TYPEDEF" *)
(* | IDENT s -> "IDENT("^s^")" *)
(* | TYPEDEF_ID s -> "TYPEDEF_iD("^s^")" *)
(* | SEMI -> ";" *)
(* | DUMMY -> "DUMMY" *)
(* | _ -> "." *)

(* let delayed_name = ref None *)
(* let delay_count = ref 0 *)

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
        (* |> delay_id_wrapper *)
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

(* and delay_id_wrapper get_token lexbuf = *)
(*     match !delayed_name with *)
(*     | None -> *)
(*         let token = get_token lexbuf in *)
(*         (match token with *)
(*         | IDENT name -> *)
(*             delayed_name := Some name; *)
(*             delay_count := 1; *)
(*             DUMMY *)
(*         | _ -> token *)
(*         ) *)
(*     | Some name -> *)
(*         if !delay_count = 1 then ( *)
(*             delayed_name := None; *)
(*             make_token name *)
(*         ) else ( *)
(*             delay_count := !delay_count - 1; *)
(*             DUMMY *)
(*         ) *)
(*  *)
and make_token name = 
    if Typedef_env.mem name then
        TYPEDEF_ID name
    else
        IDENT name
