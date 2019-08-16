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

    and expand name entry =
        let open Pp_ast in
        let open Pp_token_buffer in
        Pp_env.remove name;
        match entry with
        | ObjectMacro tokens ->
            let define = Pp_ast.DefineObject(name, tokens) in
            push_group_parts [Line tokens; define]
    in

    Pp_token_buffer.push_group_parts ast;
    process()
