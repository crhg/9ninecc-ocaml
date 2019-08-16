let rec preprocess ast =
    let buffer = Buffer.create 1000 in
    let out = Printf.bprintf buffer in

    let out_pp_token pp_token =
        let open Pp_ast in
        match pp_token with
        | Wsp s
        | Punct s
        | Id s
        | Str s
        | Num s ->
            out "%s" s in

    let group_part g = 
        let open Pp_ast in
        match g with
            | Line pp_tokens -> List.iter out_pp_token pp_tokens
            | _ -> () in

    List.iter group_part ast;
    Buffer.contents buffer
