let tokens = ref []
let group_parts = ref []

let rec token _ =
    match !tokens, !group_parts with
    | t::rest, _ -> 
        tokens := rest;
        t
    | [], g::rest ->
        do_group_part g;
        group_parts := rest;
        token()
    | [], [] ->
        Pp_ast.Eof

and do_group_part g =
    let open Pp_ast in
    match g with
    | DefineObject (name, pp_tokens) ->
        let open Pp_env in
        add name @@ ObjectMacro pp_tokens
    | NonDirective _ ->
        ()
    | Line line ->
        tokens := line

and push_group_part g =
    group_parts := g :: Pp_ast.Line !tokens :: !group_parts;
    tokens := []

and push_group_parts gs = 
    group_parts := gs @ (Pp_ast.Line !tokens :: !group_parts);
    tokens := []
