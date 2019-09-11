module StringMap = Map.Make(String)

let fprintf = Printf.fprintf

let rec preprocess filename contents =
    try
        let ast = ast_of filename contents in
        let env = Pp_env.make() in
        preprocess_with_env ast env
    with
    | Misc.Error_at (message, pos) ->
        fprintf stderr "%s:%s\n" (Source.show_pos pos) message;
        fprintf stderr "%s\n" (Source.line_at pos);
        fprintf stderr "%s\n" (Source.marker_of pos);
        exit(-1)
    | Misc.Error (message) ->
        fprintf stderr "%s\n" message;
        exit(-1)

and ast_of filename contents =
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    (* デバッグ用 *)
    let token lexbuf =
        let t = Pp_lex.token lexbuf in
        (* Printf.fprintf stderr "pp_token=%s\n" (Pp_token.show_token t); *)
        t in

    let ast = (Pp_parse.parse Pp_parser.Incremental.preprocessing_file) token lexbuf in
    let line_marker = Pp_ast.(Line [{exp=LineMarker(1, filename, None); loc=Lexing.dummy_pos}]) in
    line_marker :: ast

and preprocess_with_env ast env =
    let buffer = Buffer.create 1000 in
    let out s = Buffer.add_string buffer s in

    let expand_tokens tokens env =
        let open Pp_ast in
        let ast = [Line tokens] in
        preprocess_with_env ast env in

    let token_buffer = Pp_token_buffer_data.make_empty env expand_tokens ast_of in

    let token _ = Pp_token_buffer.token token_buffer in

    let rec process _ =
        match token() with
        | {exp=Pp_ast.Eof;_} -> Buffer.contents buffer
        | t -> process_token t; process()

    and process_token pp_token =
        let open Pp_ast in
        match pp_token.exp with
        | Wsp s
        | Punct s
        | Str s
        | Char s
        | Num s ->
            out s
        | NewLine ->
            out "\n"
        | NewLines n ->
            out @@ String.make n '\n'
        | Id name ->
            (match Pp_env.find_opt name env with
            | None -> out name
            | Some entry -> expand name entry
            )
        | Eof ->
            failwith "Eof?"
        | LineMarker (lno, filename, None) ->
            out @@ Printf.sprintf "# %d \"%s\"\n" lno filename
        | LineMarker (lno, filename, Some flag) ->
            out @@ Printf.sprintf "# %d \"%s\" %d\n" lno filename flag

    and expand name entry =
        let open Pp_ast in
        let open Pp_token_buffer in
        Pp_env.remove name env;
        match entry with
        | ObjectMacro tokens ->
            let define = Pp_ast.DefineObject(name, tokens) in
            push_group_parts [Line tokens; define] token_buffer
        | FunctionMacro (params, tokens) ->
            let expanded = expand_function params tokens in
            let define = Pp_ast.DefineFunction(name, params, tokens) in
            push_group_parts [Line expanded; define] token_buffer

    and expand_function params tokens =
        let param_map = make_param_map params in

        let open Pp_ast in
        let expand_param token = match token.exp with
            | Id name when StringMap.mem name param_map ->
                StringMap.find name param_map
            | _ -> [token] in

        List.concat @@ List.map expand_param tokens

    and make_param_map params =
        let open Pp_ast in
        let {loc=loc;_} = check_token (Punct "(") in
        (* "("を","に置き換えて残りの処理 *)
        Pp_token_buffer.back_token {exp=Punct ",";loc=loc} token_buffer;
        make_param_map_rest params

    and make_param_map_rest params =
        let open Pp_ast in
        match params with
        | [] -> 
            ignore @@ check_token (Punct ")");
            StringMap.empty
        | p::rest ->
            ignore @@ check_token (Punct ",");
            let tokens = get_param () in
            let map = make_param_map_rest rest in
            StringMap.add p tokens map

    and get_param _ =
        let open Pp_ast in
        let token = Pp_token_buffer.token token_buffer in
        match token.exp with
        | Eof -> failwith "unexpected eof"
        | Punct ")"
        | Punct "," ->
            Pp_token_buffer.back_token token token_buffer;
            []
        | Punct "(" ->
            let paren_rest = get_paren_rest () in
            let rest = get_param() in
            token :: (paren_rest @ rest)
        | _ ->
            token :: get_param()

    and get_paren_rest _ =
        let open Pp_ast in
        let token = Pp_token_buffer.token token_buffer in
        match token.exp with
        | Eof -> failwith "unexpected eof"
        | Punct ")" -> [token]
        | Punct "(" -> token :: (get_paren_rest() @ get_paren_rest())
        | _ -> token :: get_paren_rest()

    and check_token expected = 
        let open Pp_ast in
        let actual = skip_sp() in
        if actual.exp = expected then
            actual
        else
            raise(Misc.Error_at("not found: " ^ (Pp_ast.show_pp_token_exp expected), actual.loc))

    and skip_sp _ =
        let open Pp_ast in
        let token = Pp_token_buffer.token token_buffer in
        match token.exp with
        | Eof -> failwith "unexpected eof"
        | Wsp _
        | NewLine ->
            skip_sp()
        | _ -> token
    in

    Pp_token_buffer.push_group_parts ast token_buffer;
    process()
