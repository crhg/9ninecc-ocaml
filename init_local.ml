let rec gen ty d init =
    let ty, name = Type_check.type_and_var ty d in
    let ident = make_ident ty name d.loc in
    match ty with
    | Type.Char
    | Type.Int
    | Type.Ptr _ ->
        gen_scalar_init ty ident init
    | Type.Array _ ->
        gen_array_init ty ident init
    | _ -> raise (Misc.Error_at("cannot initialize", init.Ast.loc))

and gen_scalar_init ty name init = Ast.(match init.exp with
    | ExprInitializer expr
    | ListInitializer [{exp=ExprInitializer expr}] ->
        gen_assign ty name expr
    | _ -> raise(Misc.Error_at("invalid initializer", init.loc))
)

and gen_assign ident expr =
    let assign = gen_assign ident expr in
    Gen_expr.gen_expr assign;

    (* Gen_expr.gen_lval_name name; *)
    (* Gen_expr.gen_expr expr; *)
    (* Stack.pop "rdi"; *)
    (* Stack.pop "rax"; *)
    (* Gen_misc.store ty "[rax]" "rdi"; *)
    (* Stack.push "rdi" *)

and gen_array_init ty name init =
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
