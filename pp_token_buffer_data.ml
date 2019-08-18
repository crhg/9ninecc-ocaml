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

