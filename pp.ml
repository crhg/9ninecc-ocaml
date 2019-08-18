module StringMap = Map.Make(String)

let rec ast_of filename contents =
    let lexbuf = Lexing.from_string contents in

    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    (* デバッグ用 *)
    let token lexbuf =
        let t = Pp_lex.token lexbuf in
        Printf.fprintf stderr "pp_token=%s\n" (Pp_token.show_token t);
        t in

    let ast = try Pp_parser.preprocessing_file token lexbuf with
        | e ->
            let pos = lexbuf.lex_curr_p in
            Printf.fprintf stderr "%s:error\n" @@ Source.show_pos pos;
            Printf.fprintf stderr "%s\n" @@ Source.line_at pos;
            Printf.fprintf stderr "%s\n" @@ Source.marker_of pos;

            raise e in
    let line_marker = Pp_ast.(Line [LineMarker(1, filename, None)]) in
    line_marker :: ast


and preprocess ast =
    let env = Pp_env.make() in
    preprocess_with_env ast env

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
        | Pp_ast.Eof -> Buffer.contents buffer
        | t -> process_token t; process()

    and process_token pp_token =
        let open Pp_ast in
        match pp_token with
        | Wsp s
        | Punct s
        | Str s
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

        let expand_param token = match token with
        | Pp_ast.Id name when StringMap.mem name param_map ->
            StringMap.find name param_map
        | _ -> [token] in

        List.concat @@ List.map expand_param tokens

    and make_param_map params =
        let open Pp_ast in
        check_token (Punct "(");
        Pp_token_buffer.back_token (Punct ",") token_buffer;
        make_param_map_rest params

    and make_param_map_rest params =
        let open Pp_ast in
        match params with
        | [] -> 
            check_token (Punct ")");
            StringMap.empty
        | p::rest ->
            check_token (Punct ",");
            let tokens = get_param () in
            let map = make_param_map_rest rest in
            StringMap.add p tokens map

    and get_param _ =
        let open Pp_ast in
        let token = Pp_token_buffer.token token_buffer in
        match token with
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
        match token with
        | Eof -> failwith "unexpected eof"
        | Punct ")" -> [token]
        | Punct "(" -> token :: (get_paren_rest() @ get_paren_rest())
        | _ -> token :: get_paren_rest()

    and check_token token = 
        if skip_sp() <> token then
            failwith ("not found: " ^ (Pp_ast.show_pp_token token))

    and skip_sp _ =
        match Pp_token_buffer.token token_buffer with
        | Eof -> failwith "unexpected eof"
        | Wsp _
        | NewLine ->
            skip_sp()
        | token -> token

    in

    Pp_token_buffer.push_group_parts ast token_buffer;
    process()
