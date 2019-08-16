type t = {
    mutable tokens: Pp_ast.pp_token list;
    mutable group_parts : Pp_ast.group_part list;
    env : Pp_env.t;
    expand_tokens : Pp_ast.pp_token list -> Pp_env.t -> string
}

let make_empty env expand_tokens =
    { tokens = []; group_parts = []; env = env; expand_tokens = expand_tokens }

let rec token buf =
    match buf with
    | {tokens = t::rest} -> 
        buf.tokens <- rest;
        t
    | {tokens = []; group_parts = g::rest} ->
        do_group_part g buf ;
        buf.group_parts <- rest;
        token buf
    | {tokens = []; group_parts = []} ->
        Pp_ast.Eof

and do_group_part g buf =
    let open Pp_ast in
    match g with
    | DefineObject (name, pp_tokens) ->
        let open Pp_env in
        add name (ObjectMacro pp_tokens) buf.env
    | DefineFunction (name, params, pp_tokens) ->
        let open Pp_env in
        add name (FunctionMacro (params, pp_tokens)) buf.env
    | Include pp_tokens ->
        let s = buf.expand_tokens pp_tokens buf.env in
        let filename, from_current = pickup_filename s in
        if from_current then
            Printf.fprintf stderr "include \"%s\"\n" filename
        else
            Printf.fprintf stderr "include <%s> \n" filename
    | NonDirective _ ->
        ()
    | Line line ->
        buf.tokens <- line

and pickup_filename s =
    let s = String.trim s in
    let regexps = [
        ("^\"\\([^\"]*\\)\"", true); 
        ("^<\\([^>]*\\)>", false) 
    ] in

    let (_, from_current) = regexps |> List.find (fun (re, _) ->
        let regexp = Str.regexp re in
        Str.string_match regexp s 0
    ) in

    (Str.matched_group 1 s, from_current)

and push_group_part g buf =
    buf.group_parts <- g :: Pp_ast.Line buf.tokens :: buf.group_parts;
    buf.tokens <- []

and push_group_parts gs buf = 
    buf.group_parts <- gs @ (Pp_ast.Line buf.tokens :: buf.group_parts);
    buf.tokens <- []

and back_token t buf = 
    buf.tokens <- t :: buf.tokens
