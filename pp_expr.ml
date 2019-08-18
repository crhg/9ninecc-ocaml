let rec eval expr buf =
    let open Pp_token_buffer_data in
    let expr = remove_white_space expr in
    let expr = eval_defined expr buf.env in
    let expr = buf.expand_tokens expr buf.env in
    Printf.fprintf stderr "eval expr=%s\n" expr;
    let ast = ast_of expr in
    Printf.fprintf stderr "eval ast=%s\n" (Ast.show_expr ast);
    let _, i_expr = Type_check.convert ast in
    Printf.fprintf stderr "eval ast=%s\n" (Ast.show_i_expr i_expr);
    let value = Const.eval_int i_expr in
    Printf.fprintf stderr "eval value=%d\n" value;
    value <> 0

and remove_white_space tokens = 
    tokens |> List.filter_map (fun token ->
        match token with
        | Pp_ast.Wsp _ -> None
        | _ -> Some token
    )

and eval_defined expr env =
    let open Pp_ast in
    match expr with
    | [] -> []
    | Id "define" :: Id id :: rest ->
        (if Pp_env.mem id env then Num "1" else Num "0") :: eval_defined rest env
    | token :: rest ->
        token :: eval_defined rest env

and ast_of expr =
    Printf.fprintf stderr "astof\n";
    let lexbuf = Lexing.from_string expr in
    let token lexbuf =
        let tk = Lexer.token lexbuf in
        Printf.fprintf stderr "token=%s\n" (Token.show_token tk);
        tk in
    Parser.expr_eof token lexbuf


