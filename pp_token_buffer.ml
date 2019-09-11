(* token buffer といいながら各種 # xx の処理もほぼここでやっている *)

open Extension

let rec token buf =
    let open Pp_token_buffer_data in
    match buf with
    | {tokens = t::rest; _} -> 
        buf.tokens <- rest;
        t
    | {tokens = []; group_parts = g::rest; _} ->
        buf.group_parts <- rest;
        do_group_part g buf ;
        token buf
    | {tokens = []; group_parts = []; _} ->
        Pp_ast.{exp=Eof;loc=Lexing.dummy_pos}

and do_group_part g buf =
    let open Pp_ast in
    match g with
    | If conds ->
        do_if conds buf
    | DefineObject (name, pp_tokens) ->
        let open Pp_env in
        add name (ObjectMacro pp_tokens) buf.env
    | DefineFunction (name, params, pp_tokens) ->
        let open Pp_env in
        add name (FunctionMacro (params, pp_tokens)) buf.env
    | Include { include_pp_tokens = pp_tokens; include_loc = loc } ->

        let s = buf.expand_tokens pp_tokens buf.env in
        let filename, from_current = pickup_filename s in
        let filename, contents = find_include_file filename from_current in

        (* Printf.fprintf stderr "include %s\n" filename; *)
        let ast = buf.ast_of filename contents in
        (* Printf.fprintf stderr "include ast=%s\n" (Pp_ast.show_ast ast); *)

        push_group_part (Line [{exp=LineMarker(loc.pos_lnum + 1, loc.pos_fname, Some 2); loc=Lexing.dummy_pos}]) buf;
        push_group_parts ast buf;
        push_group_part (Line [{exp=LineMarker(1, filename, Some 1); loc=Lexing.dummy_pos}]) buf
    | NonDirective _ ->
        ()
    | Line line ->
        buf.tokens <- line

and do_if conds buf = 
    (* Printf.fprintf stderr "do_if\n"; *)
    let open Pp_ast in
    match conds with
    | [] -> push_group_part (Line [{exp=NewLine;loc=Lexing.dummy_pos}]) buf
    | {cond_expr = expr; cond_groups = gs} :: rest ->
        if Pp_expr.eval expr buf then (
            let rest_lines = line_count_of_conds rest in
            push_group_part (Line [{exp=NewLines rest_lines;loc=Lexing.dummy_pos}]) buf;
            push_group_parts gs buf;
            push_group_part (Line [{exp=NewLine;loc=Lexing.dummy_pos}]) buf
        ) else (
            do_if rest buf;
            let n = 1 + line_count_of_groups gs in
            push_group_part (Line [{exp=NewLines n;loc=Lexing.dummy_pos}]) buf
        )

and line_count_of_groups gs =
    List.sum @@ List.map line_count_of_group gs

and line_count_of_group g = match g with
| Pp_ast.If conds -> line_count_of_conds conds
| _ -> 1

and line_count_of_conds conds =
    let line_count_of_cond cond = line_count_of_groups cond.Pp_ast.cond_groups in
    1 (* #endifの分 *)
    + List.length conds (* #if, #elif, #elseの分 *)
    + List.sum (List.map line_count_of_cond conds) (* その他の行 *)

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
    (* ;Printf.fprintf stderr "push_group_parts %s\n" (show buf) *)


and back_token t buf = 
    let open Pp_token_buffer_data in
    buf.tokens <- t :: buf.tokens
