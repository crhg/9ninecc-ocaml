(* lexerのラッパ *)
open Parser

type state =
| Null
| Typedef of int
| GenerateDummy of int

let state = ref Null

let show token = match token with
| TYPEDEF -> "TYPEDEF"
| IDENT s -> "IDENT("^s^")"
| TYPEDEF_ID s -> "TYPEDEF_iD("^s^")"
| SEMI -> ";"
| DUMMY -> "DUMMY"
| _ -> "."

let next_token lexbuf =
    let token = Lexer.token lexbuf in
    match token with
    | IDENT name when Typedef_env.mem name ->
        TYPEDEF_ID name
    | _ -> token

let rec token lexbuf =
    let t = token' lexbuf in
    (* Printf.fprintf stderr "token=%s\n" (show t); *)
    t

and token' lexbuf =
    match !state with
    | Null -> (
        let token = next_token lexbuf in

        (match token with
        | TYPEDEF ->
            state := Typedef 0
        | _ -> ()
        );

        token
    )
    | Typedef level ->
        let token = next_token lexbuf in

        (match token with
        | SEMI when level = 0 ->
            state := GenerateDummy 1
        | LBRACE ->
            state := Typedef (level + 1)
        | RBRACE ->
            state := Typedef (level - 1)
        | _ -> ()
        );

        token
    | GenerateDummy 0 ->
        state := Null;
        next_token lexbuf
    | GenerateDummy n ->
        state := GenerateDummy (n - 1);
        DUMMY
