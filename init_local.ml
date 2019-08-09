(* ローカル変数の初期化のために代入式のリストを作る *)

let rec to_assign (ty:Type.t) (lhs:Ast.expr) (init:Ast.init) =
    match ty with
    | Type.Char
    | Type.Int
    | Type.Ptr _ ->
        [to_assign_scalar lhs init]
    | Type.Array _ ->
        to_assign_array ty lhs init
    | _ -> raise (Misc.Error_at("cannot initialize", init.Ast.loc))

and to_assign_scalar (lhs:Ast.expr) (init:Ast.init) = Ast.(match init.exp with
    | ExprInitializer expr
    | ListInitializer [{exp=ExprInitializer expr}] ->
        make_assign lhs expr
    | _ -> raise(Misc.Error_at("invalid initializer", init.loc))
)

and to_assign_array (ty:Type.t) (lhs:Ast.expr) (init:Ast.init) = Ast.(match ty, init.exp with
    | Type.Array(Char, Some size), ExprInitializer ({ exp = { e = Str _ } } as s) ->
        [make_call_strncpy lhs s size]
    | Type.Array(ty, Some size), ListInitializer l ->
        to_assign_array_by_list ty lhs size l
    | _ -> raise(Misc.Error_at("gen_array_init: " ^ (Type.show ty), lhs.loc))
)

and make_call_strncpy (lhs:Ast.expr) (s:Ast.expr) (size:int) =
    Ast.({
        exp = no_type @@ Call ("strncpy", [lhs; s; make_num size lhs.loc]);
        loc = lhs.loc
    })

and to_assign_array_by_list ty lhs size l =
    let rec to_assign_array_by_list' i l = match l with
    | []
        -> []
    | _ when i >= size
        -> []
    | x::rest ->
        let lhs_at_i = make_array_at lhs i in
        let assign = to_assign ty lhs_at_i x in
        assign @ to_assign_array_by_list' (i+1) rest in
    to_assign_array_by_list' 0 l

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
