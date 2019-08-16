type token = Pp_token.token

type entry = 
| List of token list
| Lexer of (Lexing.lexbuf -> token) * Lexing.lexbuf

let empty_buffer = []

let push_buffer buf entry = entry::buf

let rec token buf = match buf with
| [] -> (Pp_token.EOF, [])
| entry::rest ->
    let (tk, next) = next_token entry in
    if tk = Pp_token.EOF then
        token rest
    else 
        (tk, next::rest)

and next_token entry = match entry with
| List tokens ->
    let (tk, rest) = next_token_of_list tokens in
    (tk, List rest)
| Lexer (token, buf) -> (token buf, entry)

and next_token_of_list tokens = match tokens with
| [] -> (Pp_token.EOF, [])
| t::rest -> (t, rest)
