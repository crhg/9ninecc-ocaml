let rec gen ty d init =
    let _, name = Type_check.type_and_var ty d in
    let ty = Env.entry_type (Env.get_entry name) in
    let ident = make_ident name d.loc in
    gen_init ty ident init

and gen_init ty lhs init =
    match ty with
    | Type.Char
    | Type.Int
    | Type.Ptr _ ->
        gen_scalar_init lhs init
    | Type.Array _ ->
        gen_array_init ty lhs init
    | _ -> raise (Misc.Error_at("cannot initialize", init.Ast.loc))

and gen_scalar_init lhs init = Ast.(match init.exp with
    | ExprInitializer expr
    | ListInitializer [{exp=ExprInitializer expr}] ->
        gen_assign lhs expr
    | _ -> raise(Misc.Error_at("invalid initializer", init.loc))
)

and gen_assign lhs expr =
    let assign = make_assign lhs expr in
    ignore(Gen_expr.gen_expr assign)

and gen_array_init ty lhs init = Ast.(match ty, init.exp with
    | Type.Array(Char, Some size), ExprInitializer ({ exp = { e = Str _ } } as s) ->
        gen_array_init_by_string_literal lhs s size
    | Type.Array(ty, Some size), ListInitializer l ->
        gen_array_init_by_list ty lhs size l
    | _ -> raise(Misc.Error_at("gen_array_init: " ^ (Type.show ty), lhs.loc))
)

and gen_array_init_by_string_literal lhs s size =
    let call_strncpy = Ast.( 
        {
            exp = no_type @@ Call ("strncpy", [lhs; s; make_num size lhs.loc]);
            loc = lhs.loc
        }
    ) in
    ignore(Gen_expr.gen_expr call_strncpy)

and gen_array_init_by_list ty lhs size l =
    let gen_init_at i init =
        if i < size then
            let array_at = make_array_at lhs i in
            gen_init ty array_at init in
    List.iteri gen_init_at l

and make_ident name loc =
    Ast.({
        exp = no_type @@ Ident (name, ref Env.DummyEntry);
        loc = loc
    })

and make_num n loc =
    Ast.({
        exp = no_type @@ Num (string_of_int n);
        loc = loc
    })

and make_assign lhs rhs =
    Ast.({
        exp = no_type @@ Assign (lhs, rhs);
        loc = lhs.loc
    })

and make_array_at a i =
    let open Ast in
    let offset = make_num i a.loc in
    let ptr = {
        exp = no_type @@ Add (a, offset);
        loc = a.loc
    } in
    {
        exp = no_type @@ Deref ptr;
        loc = a.loc
    }
