(* 型チェック *)
(* 以下の事を行う *)
(* - 変数宣言をみて変数に型を付ける *)
(*    - 配列のサイズが省略されていたら決定する *)
(* - 式に型を付ける *)
open Ast
open Env
open Misc

let rec check decl_list =
    List.iter check_decl decl_list

and check_decl decl = match decl.exp with
| FunctionDecl ({ func_ts = ts; func_decl = decl; func_body={exp=Block stmt_list} } as fd ) ->
    let ty, name = type_and_var ts decl in
    (match ty with
        | Type.Function (ret_ty, params) ->
            fd.func_name <- Some name;
            let params = params |> List.map (fun (pname, pty) ->
                {
                    param_ty = pty;
                    param_name = pname;
                    param_entry = None
                }
            ) in
            fd.func_params <- Some params;
            let size = prepare_func params stmt_list in
            fd.func_frame_size <- Some size
        | _ -> failwith "not function"
    )

| GlobalVarDecl ({ gv_ts = ts; gv_decl = d; gv_init = None } as v) ->
    let ty, name = type_and_var ts d in

    (if not @@ Type.is_complete_type ty then raise(Error_at("incomplete type", decl.loc)));

    register_global_var ty name;

    let entry = get_entry name in
    v.gv_entry <- Some entry

| GlobalVarDecl ({ gv_ts = ts; gv_decl = d; gv_init = Some init } as v) ->
    let ty, name = type_and_var ts d in

    (* 最上位の配列サイズが未定なら初期化子から求める *)
    let ty = match ty with
        | Type.Array (t, None) ->
            let size = determine_array_size t init in
            Type.Array(t, Some size)
        | _ -> ty in
    (if not @@ Type.is_complete_type ty then raise(Error_at("incomplete type", decl.loc)));

    register_global_var ty name;

    let entry = get_entry name in
    v.gv_entry <- Some entry;

    check_init init
| _ -> failwith ("not yet:" ^ (Ast.show_decl decl))

and check_init init = match init.exp with
| ExprInitializer expr ->
    ignore @@ check_expr expr
| ListInitializer l ->
    List.iter check_init l

(* ローカル変数にオフセットを割り当てる *)
and prepare_func params stmt_list =
    Env.with_new_local_frame (fun _ ->
    Env.with_new_scope (fun _ ->
        let register param = match param with
        | { param_ty = ty; param_name = name } as p ->
            register_local_var ty name;
            let entry = get_entry name in
            p.param_entry <- Some entry in
        List.iter register params;

        List.iter check_stmt stmt_list
    ))

and determine_array_size element_ty init =
    match element_ty, init.exp with
    (* charの配列のときのみ 文字列リテラル or {文字列リテラル} でも初期化可能 *)
    | Type.Char, ExprInitializer {exp = {e = Str(s,_)}} ->
        String.length s + 1
    | Type.Char, ListInitializer [{ exp = ExprInitializer {exp = {e = Str(s,_)}}}] ->
        String.length s + 1
    | _, ListInitializer l ->
        List.length l
    | _, _ -> raise(Misc.Error_at(
        Printf.sprintf "connot determine array size: ty=%s, init=%s"
            (Type.show element_ty)
            (Ast.show_init init),
        init.loc))

and check_stmt stmt = match stmt.exp with
| Var {var_ts = ts; var_decl = d; var_init = None} ->
    let ty, name = type_and_var ts d in
    register_local_var ty name;
| Var ({var_ts = ts; var_decl = d; var_init = Some init} as v) ->
    let ty, name = type_and_var ts d in

    (* 最上位の配列サイズが未定なら初期化子から求める *)
    let ty = match ty with
        | Type.Array (t, None) ->
            let size = determine_array_size t init in
            Type.Array(t, Some size)
        | _ -> ty in
    (if not @@ Type.is_complete_type ty then raise(Error_at("incomplete type", stmt.loc)));
    register_local_var ty name;

    let entry = get_entry name in
    let ident = {
        exp = { 
            e = Ident { name=name; entry=Some entry };
            ty = Some ty
        };
        loc=d.loc
    } in
    let assign = Init_local.to_assign ty ident init in
    List.iter check_expr assign;
    v.var_init_assign <- Some assign
| Expr expr ->
    check_expr expr
| Return expr ->
    check_expr expr
| If (expr, then_stmt, else_stmt_opt) ->
    check_expr expr;
    check_stmt then_stmt;
    may check_stmt else_stmt_opt
| While (expr, stmt) ->
    check_expr expr;
    check_stmt stmt
| For (init, cond, next, stmt) ->
    may check_expr init;
    may check_expr cond;
    may check_expr next;
    check_stmt stmt;
| Block stmt_list ->
    List.iter check_stmt stmt_list
| _ -> ()

and check_expr expr = ignore @@ assign_type expr

(* 式に型をつける *)
and assign_type_plane expr = 
    let ty = find_type expr in
    expr.exp.ty <- Some ty;
    ty

(* 型の正規化 *)
and normalize_type ty = match ty with
    | Type.Char -> Type.Int (* 式中ではcharもintも同一視してintとみなす。 *)
    | Type.Array (t, _) -> Type.Ptr t (* 配列型はポインタ型に読みかえる *)
    | _ -> ty

and assign_type expr = 
    let ty = normalize_type (find_type expr) in
    expr.exp.ty <- Some ty;
    ty

and find_type expr = match expr.exp.e with
| Num _ -> Type.Int
| Str _ -> Type.Ptr Type.Char
| Ident ({ name = name } as ident)->
    let entry = get_entry name in
    ident.entry <- Some entry;
    entry_type entry
| Assign (l, r) ->
    let lty = assign_type l in
    let rty = assign_type r in
    (if not @@ is_scalar_type lty then raise(Misc.Error_at("cannot assign", expr.loc)));
    
    (* (if lty <> rty then raise(Misc.Error_at("assign type mismatch", expr.loc))); *)

    lty
| Call (_, expr_list) ->
    let _ = List.map assign_type expr_list in
    Type.Int
| Addr e ->
    let ty = assign_type_plane e in
    Type.Ptr ty
| Deref e ->
    let ty = assign_type e in
    begin
        match ty with
        | Type.Ptr t -> t
        | _ -> raise (Error_at("deref of non pointer", expr.loc))
    end
| Arrow (e, field_name) ->
    let ty = assign_type e in
    (match ty with
        | Type.Ptr ((Type.Struct _) as st_ty) ->
            let field = (
                try Type.get_field st_ty field_name with
                |Not_found -> raise(Error_at("-> field? " ^ (Type.show ty), expr.loc))
            ) in
            field.field_type
        | _ -> raise(Error_at("-> type?" ^ (Type.show ty), expr.loc))
    )
| Sizeof e ->
    let _ = assign_type_plane e in
    Type.Int
| Add (l, r) ->
    let lty = assign_type l in
    let rty = assign_type r in
    begin
        match (lty, rty) with
        | (Type.Int, Type.Int) -> Type.Int
        | (Type.Ptr _, Type.Int) -> lty
        | (Type.Int, Type.Ptr _) -> rty
        | _ -> raise (Error_at("cannot add", expr.loc))
    end
| Sub (l, r) ->
    let lty = assign_type l in
    let rty = assign_type r in
    begin
        match (lty, rty) with
        | (Type.Int, Type.Int) -> Type.Int
        | (Type.Ptr _, Type.Ptr _) -> Type.Int
        | (Type.Ptr _, Type.Int) -> lty
        | _ -> raise (Error_at("cannot sub", expr.loc))
    end
| Mul (l, r)
| Div (l, r) ->
    let lty = assign_type l in
    let rty = assign_type r in
    begin
        match (lty, rty) with
        | (Type.Int, Type.Int) -> Type.Int
        | _ -> raise (Error_at("cannot mul/div", expr.loc))
    end
| Eq (l, r)
| Ne (l, r)
| Lt (l, r)
| Le (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int

and is_scalar_type ty = match ty with
| Char
| Int
| Ptr _ ->
    true
| _ ->
    false

and type_and_var ts d =
let ty = type_of_type_spec ts in
type_and_var' ty d

and type_and_var' ty d =
match d.exp with
| DeclIdent var ->
    (ty, var)
| PointerOf d ->
    type_and_var' (Type.Ptr ty) d
| Array (d, e) ->
    let n = Option.map Const.eval_int e in
    type_and_var' (Type.Array(ty, n)) d 
| Func (d, params) ->
    let tv = fun (ts, d) ->
        let ty, name = type_and_var ts d in
        (name, ty) in
    let params = List.map tv params in
    type_and_var' (Type.Function(ty, params)) d

and type_of_type_spec ts = match ts.exp with
| Int -> Type.Int
| Char -> Type.Char
| Struct { su_tag = None; su_fields = Some fields } ->
    let st_un = st_un_of_struct fields in
    Type.(Struct { exp = Some st_un })

and st_un_of_struct fields =
    let size, alignment, fields = st_un_of_struct' 0 0 fields in
    Type.{ 
        fields = fields;
        size = size;
        alignment = alignment
    }

and st_un_of_struct' offset alignment fields = match fields with
| [] -> (Misc.round_up offset alignment, alignment, [])
| (ts, d)::rest ->
    let ty, name = type_and_var ts d in
    let size = Type.get_size ty in
    let align = Type.get_alignment ty in
    let this_offset = Misc.round_up offset align in
    let new_alignment = max align alignment in
    let next_offset = this_offset + size in
    let field = Type.{ field_name = name; field_type = ty; field_offset = this_offset } in
    let (total_size, max_alignment, rest_fields) = st_un_of_struct' next_offset new_alignment rest in
    (total_size, max_alignment, field :: rest_fields)
