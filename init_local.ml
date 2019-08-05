let rec gen ty d init =
    let ty, name = Type_check.type_and_var ty d in
    let ident = make_ident name d.loc in
    match ty with
    | Type.Char
    | Type.Int
    | Type.Ptr _ ->
        gen_scalar_init ident init
    | Type.Array _ ->
        gen_array_init ty ident init
    | _ -> raise (Misc.Error_at("cannot initialize", init.Ast.loc))

and gen_scalar_init name init = Ast.(match init.exp with
    | ExprInitializer expr
    | ListInitializer [{exp=ExprInitializer expr}] ->
        gen_assign name expr
    | _ -> raise(Misc.Error_at("invalid initializer", init.loc))
)

and gen_assign ident expr =
    let assign = make_assign ident expr in
    ignore(Gen_expr.gen_expr assign)

and gen_array_init ty ident init =
    failwith "not implemented"

and make_ident name loc =
    Ast.({
        exp = {
            e = Ident (name, ref Env.DummyEntry);
            ty = None
        };
        loc = loc
    })

and make_assign lhs rhs =
    Ast.({
        exp = {
            e = Assign (lhs, rhs);
            ty = None
        };
        loc = lhs.loc
    })
