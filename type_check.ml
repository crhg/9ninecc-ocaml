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
| TypedefDecl (ts, decl) ->
    typedef ts decl;
| DummyDecl -> ()
| _ -> failwith ("not yet:" ^ (Ast.show_decl decl))

and check_init init = match init.exp with
| ExprInitializer expr ->
    ignore @@ convert_and_store expr
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
    | Type.Char, ExprInitializer {expr={exp = Str(s,_)}} ->
        String.length s + 1
    | Type.Char, ListInitializer [{ exp = ExprInitializer {expr={exp = Str(s,_)}}}] ->
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
                exp = Ident { name=name };
            loc=d.loc
            } in
            let assign = Init_local.to_assign ty ident init in
            decl_init.di_init_assign <- List.map (Misc.compose snd convert) assign
        )
    )
| Typedef (ts, decl) ->
    typedef ts decl;
| Expr expr ->
    convert_and_store expr
| Return expr ->
    convert_and_store expr
| If (expr, then_stmt, else_stmt_opt) ->
    convert_and_store expr;
    check_stmt then_stmt;
    Option.may check_stmt else_stmt_opt
| While (expr, stmt) ->
    convert_and_store expr;
    check_stmt stmt
| For (init, cond, next, stmt) ->
    Option.may convert_and_store init;
    Option.may convert_and_store cond;
    Option.may convert_and_store next;
    check_stmt stmt;
| Block stmt_list ->
    Env.with_new_scope (fun _ ->
        List.iter check_stmt stmt_list
    )
| _ -> ()

and convert_and_store (expr_s:Ast.expr_s) =
    (* Printf.fprintf stderr "convert_and_store start %s\n" (Ast.show_expr_short expr_s.expr); *)
    let _, i_expr = convert expr_s.expr in
    expr_s.i_expr <- Some i_expr
    (* ;Printf.fprintf stderr "convert_and_store end %s\n" (Ast.show_i_expr i_expr); *)

(* 型の正規化 *)
and normalize_type ty = match ty with
    | Type.Char
    | Type.Short
    | Type.Long -> Type.Int (* 式中ではcharもintも同一視してintとみなす。 *)
    | Type.Array (t, _) -> Type.Ptr t (* 配列型はポインタ型に読みかえる *)
    | _ -> ty

and convert_normalized expr = 
    try convert_normalized' expr with
    | Misc.Error msg -> raise(Misc.Error_at(msg, expr.loc))

and convert_normalized' expr = 
    let ty, expr = convert expr in
    (normalize_type ty, expr)

and convert expr =
    let (ty, i_expr) = convert' expr in
    (* Printf.fprintf stderr "convert_expr:\n"; *)
    (* Printf.fprintf stderr "    expr = %s\n"  (Ast.show_expr expr); *)
    (* Printf.fprintf stderr "    ty = %s\n"  (Type.show_type ty); *)
    (* Printf.fprintf stderr "    i_expr = %s\n"  (Ast.show_i_expr i_expr); *)
    (ty, i_expr)

and convert' expr = match expr.exp with
| Num n -> (Type.Int, Const (int_of_string n))
| Str (s,label) ->
    (Type.Array(Type.Char, Some (String.length s + 1)), Label label)
| Ident ({ name = name } as ident)->
    let get_entry name = try get_entry name with
    | Not_found -> raise(Misc.Error(Printf.sprintf "not_found: "^(Ast.show_expr expr))) in
    (match get_entry name with
    | LocalVar ((Type.Array _) as t, offset) ->
        (t, LVar offset)
    | GlobalVar ((Type.Array _) as t, label) ->
        (t, Label label)
    | LocalVar _
    | GlobalVar _ ->
        let ty, pointer = convert_lval expr in
        (* if not @@ simple_type ty then (raise(Misc.Error_at("cannot load "^(Type.show_type ty), expr.loc))); *)
        (ty, Load(ty, pointer))
    | EnumConstant (value) ->
        (Type.Int, Const value)
    | TypeDef _ ->
        raise(Misc.Error_at("type name in expr" ^ name, expr.loc))
    )
| Assign ({assign_lhs=lhs; assign_rhs=rhs} as r) ->
    (* Printf.fprintf stderr "convert assign %s\n" (Ast.show_expr_short expr); *)
    let lty, l = convert_lval lhs in
    let rty, r = convert_normalized rhs in

    (* TODO: 型チェック *)

    if not @@ simple_type lty then (raise(Misc.Error_at("cannot assign "^(Type.show_type lty), expr.loc)));

    (lty, I_binop(Store lty, l, r))
| Call (label, expr_list) ->
    (Type.Int, ICall(label, List.map (Misc.compose snd convert) expr_list))
| BlockExpr block ->
    check_stmt block;

    (* TODO: 本当はblockの最後に実行した式文の型になりそうだが *)
    (*       真面目に考えると分岐だのループだので面倒なので当面int固定 *)
    (Type.Int, I_block block)
| Addr e ->
    let ty, e = convert_lval e in
    (Ptr ty, e)
| Deref ({deref_expr = e} as r)->
    let ty, e = convert_normalized e in
    (match ty with
    | Type.Ptr t ->
        (* if not @@ simple_type t then (raise(Misc.Error_at("cannot deref "^(Type.show_type t)^" "^(Ast.show_expr expr), expr.loc))); *)
        (match t with
        | Array _ -> (t, e)
        | _ -> (t, Load (t, e))
        )
    | _ -> raise(Error_at("not a pointer" ^ (Type.show_type ty), expr.loc))
    )
| Arrow _ ->
    (* Printf.fprintf stderr "type_check Arrow!! %s\n" (Ast.show_expr expr); *)
    let ty, lval = convert_lval expr in
    (ty, Load (ty, lval))
| Sizeof ({sizeof_expr = e} as r) ->
    let ty, _ = convert e in
    (Type.Int, Const (Type.get_size ty))
| Binop ({ op=op; lhs=l; rhs=r} as binop) ->
    let lty, l = convert_normalized l in
    let rty, r = convert_normalized r in
    (match op with
    | Add ->
        (match (lty, rty) with
        | (Type.Int, Type.Int) ->
            (Type.Int, I_binop(Add, l, r))
        | (Type.Ptr t, Type.Int) ->
            (lty, I_binop(Add, l, I_binop(Mul, r, (Const (Type.get_size t)))))
        | (Type.Int, Type.Ptr t) ->
            (rty, I_binop(Add, r, I_binop(Mul, l, (Const (Type.get_size t)))))
        | _ -> raise (Misc.Error(Printf.sprintf "cannot add %s %s" (Type.show_type lty) (Type.show_type rty)))
        )
    | Sub ->
        (match (lty, rty) with
        | (Type.Int, Type.Int) ->
            (Type.Int, I_binop(Sub, l, r))
        | (Type.Ptr t, Type.Ptr _) when lty = rty ->
            (Type.Int, I_binop(Div, I_binop(Sub, l, r), (Const (Type.get_size t))))
        | (Type.Ptr t, Type.Int) ->
            (lty, I_binop(Sub, l, I_binop(Mul, r, (Const (Type.get_size t)))))
        | _ -> raise (Misc.Error(Printf.sprintf "cannot sub %s %s" (Type.show_type lty) (Type.show_type rty)))
        )
    | Mul
    | Div ->
        (match (lty, rty) with
        | (Type.Int, Type.Int) ->
            (Type.Int, I_binop(op, l, r))
        | _ ->
            raise (Misc.Error(Printf.sprintf "cannot %s %s %s" (Ast.show_binop op) (Type.show_type lty) (Type.show_type rty)))
        )
    | Eq
    | Ne
    | Lt
    | Le ->
        (Type.Int, I_binop(op, l, r))
    )

and to_pointer ty =
    let open Type in
    match ty with
    | Array(t,_) -> Ptr t
    | _ -> Ptr ty

(* lvalの変換, (式の型, 式のポインタを求める式) *)
and convert_lval expr = match expr.exp with
| Ident ({ name = name } as ident)->
    let get_entry name = try get_entry name with
    | Not_found -> raise(Misc.Error(Printf.sprintf "not_found: "^(Ast.show_expr expr))) in
    (match get_entry name with
    | LocalVar (ty, offset) ->
        (ty, LVar offset)
    | GlobalVar (ty, label) ->
        (ty, Label label)
    | _ ->
        raise(Misc.Error_at("not lval", expr.loc))
    )
| Deref ({deref_expr = e} as r)->
    let ty, e = convert_normalized e in
    (match ty with
    | Type.Ptr t ->
        (t, e)
    | _ -> raise(Error_at("not a pointer" ^ (Type.show_type ty), expr.loc))
    )
| Arrow { arrow_expr = e; arrow_field = f } ->
    let Ptr ty, e = convert e in
    let f = Type.get_field ty f in
    (f.field_type, I_binop(Add, e, (Const f.field_offset)))
| _ ->
    raise(Misc.Error_at("not lval", expr.loc))

(* declarator から定義すべき変数名のみ求める *)
and var_of_d d = match d.exp with
| DeclIdent var ->
    var
| PointerOf d
| Array (d, _)
| Func (d, _) ->
    var_of_d d

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
    let n = Option.map eval_expr e in
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
    if Env.defined_tag_in_current_scope tag then
        (match Env.get_tag_opt tag with
            | Some ((Type.Struct ({body = None} as s)) as ty) ->
                s.body <- Some body;
                ty
            | _ ->
                raise(Misc.Error("redifinition"));
        )
    else (
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
    if Env.defined_tag_in_current_scope tag then
        (match Env.get_tag_opt tag with
            | Some ((Type.Union ({body = None} as s)) as ty) ->
                s.body <- Some body;
                ty
            | _ ->
                raise(Misc.Error("redifinition"));
        )
    else (
        let ty = Type.Union {
            id = Unique_id.new_id "union-";
            tag = Some tag;
            body =Some body
        } in
        Env.register_tag tag ty;
        ty
    )
| Union _ -> failwith("invalid type_spec: " ^ (Ast.show_type_spec ts))

| Enum { enum_tag = None; enum_list = Some enum_list } ->
    declare_enum_list enum_list;
    Type.Int
| Enum { enum_tag = Some tag; enum_list = None } ->
    if Env.defined_tag_in_current_scope tag then (
        (match Env.get_tag_opt tag with
            | Some Type.Int ->
                Type.Int
            | Some ty ->
                raise(Misc.Error("tag mismatch: "^(Type.show_type ty)))
            | _ ->
                failwith "?"
        )
    ) else (
        raise(Misc.Error("no such enum tag: "^tag))
    )
| Enum { enum_tag = Some tag; enum_list = Some enum_list } ->
    if Env.defined_tag_in_current_scope tag then (
        raise(Misc.Error("redefine tag"))
    ) else (
        Env.register_tag tag Type.Int;
        declare_enum_list enum_list;
        Type.Int
    )

| Type id ->
    let get_entry id = try Env.get_entry id with
    | Not_found -> raise(Misc.Error("type not found: "^id)) in
    (match get_entry id with
    | Env.TypeDef ty -> ty
    | _ -> raise(Misc.Error("not typedef id: "^id))
    )

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

and declare_enum_list l =
    let rec declare_enum_list' n l = match l with
    | [] -> ()
    | {exp=e}::rest ->
        let n = match e.en_expr with
        | None -> n
        | Some expr -> eval_expr expr.expr in
        Env.register_enum e.en_name n;
        declare_enum_list' (n+1) rest in
    declare_enum_list' 0 l

and check_complete ty loc = 
        if not @@ Type.is_complete_type ty then
            raise(Error_at("incomplete type: " ^ (Type.show ty), loc))

and typedef ts decl =
    let ty, name = type_and_var_ts ts decl in
    Env.register_typedef ty name

and eval_expr expr = Const.eval_int (snd (convert expr))

and simple_type ty =
    let open Type in
    match ty with
    | Char | Short | Int | Long | Ptr _ ->
        true
    | _ ->
        false

