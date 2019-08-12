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
    let ty, name = type_and_var_ts ts decl in
    (match ty with
        | Type.Function (ret_ty, params) ->
            fd.func_name <- Some name;
            let params = params |> List.map (fun (pname, pty) ->
                {
                    param_ty = pty;
                    param_name = pname;
                    param_entry = None;
                    param_loc = decl.loc
                }
            ) in
            fd.func_params <- Some params;
            let size = prepare_func params stmt_list in
            fd.func_frame_size <- Some size
        | _ -> failwith "not function"
    )

| GlobalVarDecl { gv_ts = ts; gv_decl_inits = decl_inits } ->
    let ty = type_of_type_spec ts in
    decl_inits |> List.iter (fun ({ di_decl = d; di_init = init } as di) ->
            let ty, name = type_and_var_ty ty d in

            (* 最上位の配列サイズが未定で初期化子があれば求める *)
            let ty = match ty, init with
                | Type.Array (t, None), Some init ->
                    let size = determine_array_size t init in
                    Type.Array(t, Some size)
                | _, _ -> ty in

            check_complete ty d.loc;

            register_global_var ty name;

            let entry = get_entry name in
            di.di_entry <- Some entry;

            Option.may check_init init
    )
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
        | { param_ty = ty; param_name = name; param_loc = loc } as p ->
            check_complete ty loc;

            (try register_local_var ty name with
                | Type.Incomplete -> raise(Misc.Error_at("incomplete", loc))
            );
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
| Var ({var_ts = ts; var_decl_inits = decl_inits} as v) ->
    let ty = type_of_type_spec ts in

    decl_inits |> List.iter (fun decl_init -> match decl_init with
    | { di_decl = d; di_init = init } ->
        let ty, name = type_and_var_ty ty d in

        let ty = match ty, init with
            | Type.Array (t, None), Some init ->
                let size = determine_array_size t init in
                Type.Array(t, Some size)
            | _ -> ty in

        check_complete ty d.loc;

        register_local_var ty name;

        init |> Option.may (fun init ->
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
            decl_init.di_init_assign <- assign
        )
    )

| Expr expr ->
    check_expr expr
| Return expr ->
    check_expr expr
| If (expr, then_stmt, else_stmt_opt) ->
    check_expr expr;
    check_stmt then_stmt;
    Option.may check_stmt else_stmt_opt
| While (expr, stmt) ->
    check_expr expr;
    check_stmt stmt
| For (init, cond, next, stmt) ->
    Option.may check_expr init;
    Option.may check_expr cond;
    Option.may check_expr next;
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
    | Type.Char
    | Type.Short
    | Type.Long -> Type.Int (* 式中ではcharもintも同一視してintとみなす。 *)
    | Type.Array (t, _) -> Type.Ptr t (* 配列型はポインタ型に読みかえる *)
    | _ -> ty

and assign_type expr = 
    try assign_type' expr with
    | Misc.Error msg -> raise(Misc.Error_at(msg, expr.loc))

and assign_type' expr = 
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
    (if not @@ is_scalar_type lty then raise(Misc.Error("cannot assign")));
    
    (* (if lty <> rty then raise(Misc.Error("assign type mismatch"))); *)

    lty
| Call (_, expr_list) ->
    let _ = List.map assign_type expr_list in
    Type.Int
| BlockExpr block ->
    check_stmt block;

    (* TODO: 本当はblockの最後に実行した式文の型になりそうだが *)
    (*       真面目に考えると分岐だのループだので面倒なので当面int固定 *)
    Type.Int
| Addr e ->
    let ty = assign_type_plane e in
    Type.Ptr ty
| Deref e ->
    let ty = assign_type e in
    begin
        match ty with
        | Type.Ptr t -> t
        | _ -> raise (Error("deref of non pointer"))
    end
| Arrow (e, field_name) ->
    let ty = assign_type e in
    (match ty with
        | Type.Ptr ((Type.Struct _) as st_ty)
        | Type.Ptr ((Type.Union _) as st_ty) ->
            let field = (
                try Type.get_field st_ty field_name with
                |Not_found -> raise(Error("-> field? " ^ (Type.show ty)))
            ) in
            field.field_type
        | _ -> raise(Error("-> type?" ^ (Type.show ty)))
    )
| Sizeof e ->
    let _ = assign_type_plane e in
    Type.Int
| Binop ({ op=op; lhs=l; rhs=r} as binop) ->
    let lty = assign_type l in
    let rty = assign_type r in
    (match op with
    | Add ->
        (match (lty, rty) with
        | (Type.Int, Type.Int) -> Type.Int
        | (Type.Ptr t, Type.Int) ->
            binop.op <- PtrAdd (Type.get_size t);
            lty
        | (Type.Int, Type.Ptr t) ->
            binop.op <- PtrAdd (Type.get_size t);
            binop.lhs <- r;
            binop.rhs <- l;
            rty
        | _ -> raise (Misc.Error(Printf.sprintf "cannot add %s %s" (Type.show_type lty) (Type.show_type rty)))
        )
    | Sub ->
        (match (lty, rty) with
        | (Type.Int, Type.Int) ->
            Type.Int
        | (Type.Ptr t, Type.Ptr _) when lty = rty ->
            binop.op <- PtrDiff (Type.get_size t);
            Type.Int
        | (Type.Ptr t, Type.Int) ->
            binop.op <- PtrSub (Type.get_size t);
            lty
        | _ -> raise (Misc.Error(Printf.sprintf "cannot sub %s %s" (Type.show_type lty) (Type.show_type rty)))
        )
    | Mul
    | Div ->
        (match (lty, rty) with
        | (Type.Int, Type.Int) -> Type.Int
        | _ -> raise (Misc.Error(Printf.sprintf "cannot %s %s %s" (Ast.show_binop op) (Type.show_type lty) (Type.show_type rty)))
        )
    | Eq
    | Ne
    | Lt
    | Le ->
        Type.Int
    )

and is_scalar_type ty = match ty with
| Char
| Short
| Int
| Long
| Ptr _ ->
    true
| _ ->
    false

and type_and_var_ts ts d =
let ty = type_of_type_spec ts in
type_and_var_ty ty d

and type_and_var_ty ty d =
match d.exp with
| DeclIdent var ->
    (ty, var)
| PointerOf d ->
    type_and_var_ty (Type.Ptr ty) d
| Array (d, e) ->
    let n = Option.map Const.eval_int e in
    type_and_var_ty (Type.Array(ty, n)) d 
| Func (d, params) ->
    let tv = fun (ts, d) ->
        let ty, name = type_and_var_ts ts d in
        (name, ty) in
    let params = List.map tv params in
    type_and_var_ty (Type.Function(ty, params)) d

and type_of_type_spec ts = match ts.exp with
| Long -> Type.Long
| Int -> Type.Int
| Short -> Type.Short
| Char -> Type.Char
| Struct { su_tag = None; su_fields = Some fields } ->
    let body = body_of_struct fields in
    Type.Struct {
        id = Unique_id.new_id "struct-";
        tag = None;
        body = Some body
    }
| Struct { su_tag = Some tag; su_fields = None } ->
    (match Env.get_tag_opt tag with
        | Some ((Type.Struct _) as ty) -> ty
        | _ ->
            let ty = Type.Struct {
                id = Unique_id.new_id "struct-";
                tag = Some tag;
                body = None
            } in
            Env.register_tag tag ty;
            ty
    )
| Struct { su_tag = Some tag; su_fields = Some fields } ->
    let body = body_of_struct fields in
    (match Env.get_tag_opt tag with
        | Some ((Type.Struct s) as ty) ->
            (* Printf.fprintf stderr "ty before: %s\n" (Type.show_type ty); *)
            s.body <- Some body;
            (* Printf.fprintf stderr "ty after: %s\n" (Type.show_type ty); *)
            ty
        | _ ->
            let ty = Type.Struct {
                id = Unique_id.new_id "struct-";
                tag = Some tag;
                body =Some body
            } in
            Env.register_tag tag ty;
            ty
    )
| Struct _ -> failwith("invalid type_spec: " ^ (Ast.show_type_spec ts))
| Union { su_tag = None; su_fields = Some fields } ->
    let body = body_of_union fields in
    Type.Union {
        id = Unique_id.new_id "union-";
        tag = None;
        body = Some body
    }
| Union { su_tag = Some tag; su_fields = None } ->
    (match Env.get_tag_opt tag with
        | Some ((Type.Union s) as ty) -> ty
        | _ ->
            let ty = Type.Union {
                id = Unique_id.new_id "union-";
                tag = Some tag;
                body = None
            } in
            Env.register_tag tag ty;
            ty
    )
| Union { su_tag = Some tag; su_fields = Some fields } ->
    let body = body_of_union fields in
    (match Env.get_tag_opt tag with
        | Some ((Type.Union s) as ty) ->
            (* Printf.fprintf stderr "ty before: %s\n" (Type.show_type ty); *)
            s.body <- Some body;
            (* Printf.fprintf stderr "ty after: %s\n" (Type.show_type ty); *)
            ty
        | _ ->
            let ty = Type.Union {
                id = Unique_id.new_id "union-";
                tag = Some tag;
                body =Some body
            } in
            Env.register_tag tag ty;
            ty
    )
| Union _ -> failwith("invalid type_spec: " ^ (Ast.show_type_spec ts))

and body_of_struct fields =
    let size, alignment, fields = body_of_struct' 0 0 fields in
    Type.{ 
        fields = fields;
        size = size;
        alignment = alignment
    }

and body_of_struct' offset alignment fields = match fields with
| [] -> (Misc.round_up offset alignment, alignment, [])
| (ts, d)::rest ->
    let ty, name = type_and_var_ts ts d in
    let size = Type.get_size ty in
    let align = Type.get_alignment ty in
    let this_offset = Misc.round_up offset align in
    let new_alignment = max align alignment in
    let next_offset = this_offset + size in
    let field = Type.{ field_name = name; field_type = ty; field_offset = this_offset } in
    let (total_size, max_alignment, rest_fields) = body_of_struct' next_offset new_alignment rest in
    (total_size, max_alignment, field :: rest_fields)

and body_of_union fields =
    let size, alignment, fields = body_of_union' 0 0 fields in
    Type.{ 
        fields = fields;
        size = size;
        alignment = alignment
    }

and body_of_union' size alignment fields = match fields with
| [] -> (Misc.round_up size alignment, alignment, [])
| (ts, d)::rest ->
    let ty, name = type_and_var_ts ts d in
    let sz = Type.get_size ty in
    let align = Type.get_alignment ty in
    let new_size = max sz size in
    let new_alignment = max align alignment in
    let field = Type.{ field_name = name; field_type = ty; field_offset = 0 } in
    let (total_size, max_alignment, rest_fields) = body_of_union' new_size new_alignment rest in
    (total_size, max_alignment, field :: rest_fields)

and check_complete ty loc = 
        if not @@ Type.is_complete_type ty then
            raise(Error_at("incomplete type: " ^ (Type.show ty), loc))
