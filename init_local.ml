(* ローカル変数の初期化のために代入式のリストを作る *)

open Extension

let rec to_assign ty lhs init =
    let open Type in
    match ty with
    | Bool
    | Char
    | Short
    | Int
    | Long
    | Ptr _ ->
        [to_assign_scalar lhs init]
    | Array _ ->
        to_assign_array ty lhs init
    | (Struct { body = Some { fields = fields; _ }; _ }) ->
        to_assign_struct lhs fields init
    | (Union { body = Some { fields = fields; _ }; _ }) ->
        ignore fields;
        failwith "initalization of union is not implemented"
    | Void
    | Function _
    | Struct _
    | Union _ ->
        raise (Misc.Error_at("cannot initialize(to_assign): " ^ (Type.show ty), init.Ast.loc))

and to_assign_opt ty lhs init = match init with
| Some init -> to_assign ty lhs init
| None -> to_assign_default ty lhs

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
        to_assign_array_by_list ty lhs size l
    | _ -> raise(Misc.Error_at("gen_array_init: " ^ (Type.show ty), lhs.loc))
)

(* XXX: とりあえず1文字ずつの文字コードの代入文列に変換 *)
and to_assign_char_array_by_string lhs size s s_loc =
    let l = String.length s in
    List.range 0 (size - 1) |> List.map (fun i ->
        let lhs_at_i = make_array_at lhs i in
        let c = if i < l then Char.code s.[i] else 0 in
        let rhs = make_num c s_loc in
        make_assign lhs_at_i rhs
    )

and to_assign_array_by_list ty lhs size l =
    List.range 0 (size - 1) |> List.map (fun i ->
        to_assign_opt ty (make_array_at lhs i) (List.nth_opt l i)
    ) 
    |> List.concat

and to_assign_struct lhs fields init =
    let open Ast in
    match init.exp with
    | ListInitializer inits ->
        fields 
            |> List.mapi (fun i Type.{ field_name = field; field_type = ty; _ } ->
                to_assign_opt ty (make_aggregate_dot_field lhs field) (List.nth_opt inits i)
            )
            |> List.concat
    | ExprInitializer _ ->
        failwith "cannot initialize struct with scalar"

and to_assign_default ty lhs =
    match ty with
    | Type.Char
    | Type.Short
    | Type.Int
    | Type.Long
    | Type.Ptr _ ->
            [to_assign_default_scalar lhs]
    | Type.Array (ty, Some size) ->
        to_assign_default_array ty lhs size
    | Type.(Struct { body = Some { fields = fields; _ }; _ }) ->
        to_assign_default_struct lhs fields
    | _ -> raise (Misc.Error_at("cannot initialize(to_assign): " ^ (Type.show ty), lhs.Ast.loc))

and to_assign_default_scalar lhs =
    make_assign lhs @@ make_num 0 lhs.loc

and to_assign_default_array ty lhs size = 
    List.range 0 (size - 1)
        |> List.map @@ (fun i -> to_assign_default ty @@ make_array_at lhs i)
        |> List.concat

and to_assign_default_struct lhs fields =
    fields 
        |> List.map (fun Type.{ field_name = field; field_type = ty; _ } ->
            to_assign_default ty @@ make_aggregate_dot_field lhs field
        )
        |> List.concat

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

and make_aggregate_dot_field a field =
    let open Ast in
    let p = { exp = Addr a; loc = a.loc } in
    { exp = Arrow (p, field); loc = a.loc }
