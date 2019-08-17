type t = {
    mutable tokens: Pp_ast.pp_token list;
    mutable group_parts : Pp_ast.group_part list;
    env : Pp_env.t; [@opaque]
    expand_tokens : Pp_ast.pp_token list -> Pp_env.t -> string [@opaque];
    ast_of : string -> string -> Pp_ast.group_part list [@opaque]
}
[@@deriving show]

let make_empty env expand_tokens ast_of =
    {
        tokens = [];
        group_parts = [];
        env = env;
        expand_tokens = expand_tokens;
        ast_of = ast_of
    }

let rec token buf =
    match buf with
    | {tokens = t::rest} -> 
        buf.tokens <- rest;
        t
    | {tokens = []; group_parts = g::rest} ->
        buf.group_parts <- rest;
        do_group_part g buf ;
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
        let filename, contents = find_include_file filename from_current in
        (* Printf.fprintf stderr "include %s\n" filename; *)
        let ast = buf.ast_of filename contents in
        (* Printf.fprintf stderr "include ast=%s\n" (Pp_ast.show_ast ast); *)
        push_group_parts ast buf
    | NonDirective _ ->
        ()
    | Line line ->
        buf.tokens <- line

and find_include_file filename from_current =
    if filename.[0] = '/' then
        (filename, Source.read filename)
    else (
        let include_path = !Setting.include_path in
        let include_path = if from_current then "."::include_path else include_path in

        find_include_file_with_path filename include_path
    )

and find_include_file_with_path filename include_path = match include_path with
| [] -> failwith("not found: "^filename)
| path::rest ->
    let f = path ^ "/" ^ filename in
    (try (f, Source.read f) with
    | Sys_error _ -> 
        find_include_file_with_path filename rest
    )

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
    buf.group_parts <- gs @ [Pp_ast.Line buf.tokens] @ buf.group_parts;
    buf.tokens <- []
    ;Printf.fprintf stderr "push_group_parts %s" (show buf)


and back_token t buf = 
    buf.tokens <- t :: buf.tokens
