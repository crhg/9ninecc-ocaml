module StringMap = Map.Make(String)

let preprocess ast =
    let buffer = Buffer.create 1000 in
    let out s = Buffer.add_string buffer s in

    let rec process _ =
        match Pp_token_buffer.token() with
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
        | Id name ->
            (match Pp_env.find_opt name with
            | None -> out name
            | Some entry -> expand name entry
            )
        | Eof ->
            failwith "Eof?"

    and expand name entry =
        let open Pp_ast in
        let open Pp_token_buffer in
        Pp_env.remove name;
        match entry with
        | ObjectMacro tokens ->
            let define = Pp_ast.DefineObject(name, tokens) in
            push_group_parts [Line tokens; define]
        | FunctionMacro (params, tokens) ->
            let expanded = expand_function params tokens in
            let define = Pp_ast.DefineFunction(name, params, tokens) in
            push_group_parts [Line expanded; define]

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
        Pp_token_buffer.back_token (Punct ",");
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
        let token = Pp_token_buffer.token() in
        match token with
        | Eof -> failwith "unexpected eof"
        | Punct ")"
        | Punct "," ->
            Pp_token_buffer.back_token token;
            []
        | Punct "(" ->
            let paren_rest = get_paren_rest () in
            let rest = get_param() in
            token :: (paren_rest @ rest)
        | _ ->
            token :: get_param()

    and get_paren_rest _ =
        let open Pp_ast in
        let token = Pp_token_buffer.token() in
        match token with
        | Eof -> failwith "unexpected eof"
        | Punct ")" -> [token]
        | Punct "(" -> token :: (get_paren_rest() @ get_paren_rest())
        | _ -> token :: get_paren_rest()

    and check_token token = 
        if skip_sp() <> token then
            failwith ("not found: " ^ (Pp_ast.show_pp_token token))

    and skip_sp _ =
        match Pp_token_buffer.token() with
        | Eof -> failwith "unexpected eof"
        | Wsp _
        | NewLine ->
            skip_sp()
        | token -> token

    in

    Pp_token_buffer.push_group_parts ast;
    process()
