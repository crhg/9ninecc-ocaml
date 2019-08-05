open Ast
open Env
open Misc

(* ローカル変数にオフセットを割り当てる *)
let rec prepare_func params stmt =
    let register (ty, name) = register_local_var ty name in
    reset();
    List.iter register params;
    allocate_stmt stmt

and allocate_stmt stmt = match stmt.exp with
| Var (ty, name) ->
    begin
        try register_local_var ty name with
        | DuplicatedLocal _ ->
            raise (Error_at("duplicated: " ^ name, stmt.loc))
    end
| If (expr, then_stmt, else_stmt_opt) ->
    allocate_stmt then_stmt;
    may allocate_stmt else_stmt_opt
| While (expr, stmt) ->
    allocate_stmt stmt
| For (init, cond, next, stmt) ->
    allocate_stmt stmt;
| Block stmt_list ->
    List.iter allocate_stmt stmt_list
| _ -> ()

(* 初期化子の式に型を付ける *)
and prepare_init init = match init.exp with
| ExprInitializer expr -> ignore (assign_type expr)
| ListInitializer l -> List.iter prepare_init l

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
| Ident (name, entry_ref) ->
    let entry = get_entry name in
    entry_ref := entry;
    entry_type entry
| Assign (l, r) ->
    let lty = assign_type l in
    let _ = assign_type r in
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
| Mul (l, r) ->
    let lty = assign_type l in
    let rty = assign_type r in
    begin
        match (lty, rty) with
        | (Type.Int, Type.Int) -> Type.Int
        | _ -> raise (Error_at("cannot mul", expr.loc))
    end
| Div (l, r) ->
    let lty = assign_type l in
    let rty = assign_type r in
    begin
        match (lty, rty) with
        | (Type.Int, Type.Int) -> Type.Int
        | _ -> raise (Error_at("cannot div", expr.loc))
    end
| Eq (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int
| Ne (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int
| Lt (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int
| Le (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int

and type_and_var t d = match d.exp with
| DeclIdent var ->
    (t, var)
| PointerOf d ->
    type_and_var (Type.Ptr t) d
| Array (d, e) ->
    let n = Option.map Const.eval_int e in
    type_and_var (Type.Array(t, n)) d 
| Func (d, params) ->
    let param_type_list = List.map (fun (t, _)->t) params in
    type_and_var (Type.Function(t, param_type_list)) d
