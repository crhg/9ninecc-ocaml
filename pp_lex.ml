(* pp_lexerのラッパ *)

module Token = Pp_token

type state =
| Bol
| AfterSharp
| AfterKeyword
| Other

let to_return = ref []
let buffer = ref []
let state = ref Bol

let rec token lexbuf =
    lexbuf |> (
        Pp_lexer.token
        |> normalize_line
    )

(* 行の先頭で判別がつくようにするラッパ *)
and normalize_line get_token lexbuf =
    match !to_return with
    | token :: rest -> to_return := rest; token
    | [] -> 
        let open Token in
        (match !state with
        | Bol ->
            (match get_token lexbuf with
            | WSP _ as wsp -> 
                (* #以外が来るときのために空白を保存する *)
                buffer := wsp :: !buffer;
                normalize_line get_token lexbuf
            | SHARP ->
                state := AfterSharp;
                buffer := [];
                normalize_line get_token lexbuf
            | EOF ->
                EOF
            | _ as token ->
                state := (if token = NL then Bol else Other);
                buffer := token :: !buffer;
                to_return := LINE :: List.rev !buffer;
                buffer := [];
                normalize_line get_token lexbuf
            )
        | AfterSharp ->
            (match get_token lexbuf with
            | NL ->
                state := Bol;
                NL
            | WSP _ ->
                normalize_line get_token lexbuf
            | IF ->
                state := AfterKeyword;
                SHARP_IF
            | ELIF ->
                state := AfterKeyword;
                SHARP_ELIF
            | ELSE ->
                state := AfterKeyword;
                SHARP_ELSE
            | ENDIF ->
                state := AfterKeyword;
                SHARP_ENDIF
            | DEFINE ->
                state := AfterKeyword;
                SHARP_DEFINE
            | INCLUDE ->
                state := AfterKeyword;
                SHARP_INCLUDE
            | token -> 
                state := AfterKeyword;
                SHARP_NON_DIRECTIVE token
            )
        | AfterKeyword ->
            (match get_token lexbuf with
            | NL ->
                state := Bol;
                NL
            | WSP _ ->
                normalize_line get_token lexbuf
            | token -> 
                state := Other;
                token
            )
        | Other ->
            (match get_token lexbuf with
            | NL ->
                state := Bol;
                NL
            | token -> 
                token
            )
        )
