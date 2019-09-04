(* ローカル変数の初期化のために代入式のリストを作る *)

let rec to_assign ty lhs init =
    match ty with
    | Type.Char
    | Type.Short
    | Type.Int
    | Type.Long
    | Type.Ptr _ ->
        [to_assign_scalar lhs init]
    | Type.Array _ ->
        to_assign_array ty lhs init
    | Type.(Struct { body = Some { fields = fields; _ }; _ }) ->
        to_assign_struct lhs fields init
    | _ -> raise (Misc.Error_at("cannot initialize(to_assign): " ^ (Type.show ty), init.Ast.loc))

and to_assign_scalar lhs init = Ast.(match init.exp with
    | ExprInitializer expr
    | ListInitializer [{exp=ExprInitializer expr; _}] ->
        make_assign lhs expr.expr
    | _ -> raise(Misc.Error_at("invalid initializer", init.loc))
)

and to_assign_array ty lhs init = Ast.(match ty, init.exp with
    | Type.Array(Char, Some size), ExprInitializer { expr = { exp = Str (s, _); loc = s_loc }; _} ->
        to_assign_char_array_by_string lhs size s s_loc
    | Type.Array(ty, Some size), ListInitializer l ->
        to_assign_array_by_list ty lhs (Misc.take size l)
    | _ -> raise(Misc.Error_at("gen_array_init: " ^ (Type.show ty), lhs.loc))
)

(* XXX: とりあえず1文字ずつの文字コードの代入文列に変換 *)
and to_assign_char_array_by_string lhs size s s_loc =
    let s = s ^ "\000" in
    let l = String.length s in
    let copy_size = min size l in
    let range = List.init (copy_size-1) (fun x -> x) in
    range |> List.map (fun i ->
        let lhs_at_i = make_array_at lhs i in
        let rhs = make_num (Char.code s.[i]) s_loc in
        make_assign lhs_at_i rhs
    )

and to_assign_array_by_list ty lhs l =
    let make_assign i x =
        let lhs_at_i = make_array_at lhs i in
        to_assign ty lhs_at_i x in
    List.concat @@ List.mapi make_assign l

and to_assign_struct lhs fields init =
    let open Ast in
    match init.exp with
    | ListInitializer inits ->
        let to_assign_field (field, init) =
            let p = { exp = Addr lhs; loc = lhs.loc } in
            let p_arrow_field = { exp = Arrow (p, field.Type.field_name); loc = lhs.loc } in
            to_assign field.Type.field_type p_arrow_field init in
        List.concat @@ List.map to_assign_field @@ Misc.zip fields inits
    | ExprInitializer _ ->
        failwith "cannot initialize struct with scalar"

and make_num n loc =
    Ast.({
        exp = Num (string_of_int n);
        loc = loc
    })

and make_assign lhs rhs =
    Ast.({
        exp = Assign (lhs, rhs);
        loc = lhs.loc
    })

and make_array_at a i =
    let open Ast in
    let offset = make_num i a.loc in
    let ptr = {
        exp = Binop(Add, a, offset);
        loc = a.loc
    } in
    {
        exp = Deref ptr;
        loc = a.loc
    }
