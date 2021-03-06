type 't node = {
    exp: 't;
    loc: Lexing.position [@opaque]
}

and st_un = {
    su_tag: string option;
    su_fields: (type_spec * declarator) list option
}

and enum = {
    enum_tag: string option;
    enum_list: enumarator list option
}

and enumerator_exp = {
    en_name: string;
    en_expr: expr_s option
}
and enumarator = enumerator_exp node

and type_name_exp = {
    type_name_ts: type_spec;
    type_name_decl: declarator
}
and type_name = type_name_exp node

and decl_spec = {
    ds_type_spec: type_spec option;
    ds_storage_class_spec: storage_class_spec option
}

and storage_class_spec_exp =
| Typedef
| Extern
| Static
and storage_class_spec = storage_class_spec_exp node

and type_spec_exp =
| Void
| Long
| Int
| Short
| Char
| Bool
| Struct of st_un
| Union of st_un
| Enum of enum
| Type of string
and type_spec = type_spec_exp node

and function_decl_r =
{
    func_ds: decl_spec;
    func_decl: declarator;
    func_body: stmt;
    func_has_varargs: bool;
}

and decl_exp =
| FunctionDecl of function_decl_r
| GlobalVarDecl of {
    gv_ds: decl_spec;
    gv_decl_inits: decl_init list
}
| TypedefDecl of type_spec * declarator list
| DummyDecl
and decl = decl_exp node

and decl_init = {
    di_decl: declarator;
    di_init: init option;
    mutable di_init_assign: i_expr list (* ローカル変数用。初期化を行う代入式のリスト。型が決まってから設定 *)
}

and declarator_exp =
| DeclIdent of string
| PointerOf of declarator
| Array of declarator * expr option
| Func of declarator * (type_spec * declarator) list * bool (* ...の有無 *)
and declarator = declarator_exp node

and init_exp =
| ExprInitializer of expr_s
| ListInitializer of init list
and init = init_exp node

and label_kind =
| Case of expr_s
| Default

and stmt_exp = 
| LabeledStmt of label_kind * string * stmt
| Empty
| Var of  {
    var_ds: decl_spec;
    var_decl_inits: decl_init list
}
| TmpVar of string * expr (* 内部での一時変数導入用, exprの型の値がはいる変数が宣言する *)
| TypedefStmt of type_spec * declarator list
| Expr of expr_s
| Return of expr_s option
| If of expr_s * stmt * stmt option
| While of expr_s * stmt
| Do of stmt * expr_s
| For of expr_s option * expr_s option * expr_s option * stmt
| Switch of expr_s * stmt
| Break
| Continue
| Block of stmt
| StmtList of stmt list
and stmt = stmt_exp node

and binop =
| Add
| Sub
| Mul
| Div
| Mod
| Lt
| Le
| Eq
| Ne
| BitAnd
| BitXor
| BitOr
| LShift
| RShift
| LAnd
| LOr
| Comma
| Store of Type.t

and expr_exp =
| Num of string
| Str of string * string (* 文字列そのものとラベル *)
| Ident of string
| Binop of binop * expr * expr
| Assign of expr * expr
| Call of expr * expr list
| Deref of expr
| Addr of expr
| Sizeof of expr
| SizeofType of type_name
| Arrow of expr * string (* フィールド名 *)
| Cast of type_name * expr
| BitComplement of expr
| Cond of expr * expr * expr
| BlockExpr of stmt
| CompoundLiteral of type_name * init
and expr = expr_exp node

(* 式の中間表現 *)
and i_expr =
| Const of int
| Label of string (* ラベルのアドレス *)
| LVar of int (* 指定されたオフセットの位置にあるローカル変数のアドレス *)
| Load of Type.t * i_expr
| ICall of i_expr * i_expr list
| I_binop of binop * i_expr * i_expr
| ICond of i_expr * i_expr * i_expr
| I_block of stmt
| IBitComplement of i_expr
| IBoolOfRetval of i_expr
| IBoolOfInt of i_expr
| ICharOfInt of i_expr
| IShortOfInt of i_expr
| IIntOfInt of i_expr
| Error of error_r

and error_r = {
    error_exn : exn [@opaque]
}

(* 文の中に置く式, 中間表現に変換した結果を格納できる *)
and expr_s = { expr: expr; mutable i_expr: i_expr option }

[@@deriving show {with_path = false}]

let rec show_expr_short expr = match expr.exp with
| Num n ->
    n
| Str (s, _) ->
    Printf.sprintf "\"%s\"" (String_literal.escaped s)
| Ident name ->
    name
| Binop (op, l, r) ->
    Printf.sprintf "(%s)%s(%s)" (show_expr_short l) (show_binop_short op) (show_expr_short r)
| Assign (l, r) ->
    Printf.sprintf "(%s)=(%s)" (show_expr_short l) (show_expr_short r)
| Call (f, params) ->
    Printf.sprintf "(%s)(%s)" (show_expr_short f) (String.concat ", " (List.map show_expr_short params))
| Deref e ->
    Printf.sprintf "*(%s)" (show_expr_short e)
| Addr e ->
    Printf.sprintf "&(%s)" (show_expr_short e)
| Sizeof e ->
    Printf.sprintf "sizeof(%s)" (show_expr_short e)
| SizeofType _ ->
    Printf.sprintf "sizeof(...)"
| Arrow (e, f) ->
    Printf.sprintf "(%s)->(%s)" (show_expr_short e) f
| Cast(_, e) ->
    Printf.sprintf "Cast(...,%s)" (show_expr_short e)
| BitComplement e ->
    Printf.sprintf "~(%s)" (show_expr_short e)
| Cond(c, t, e) ->
    Printf.sprintf "(%s)?(%s):(%s)" (show_expr_short c) (show_expr_short t) (show_expr_short e)
| BlockExpr _ ->
    "{...}"
| CompoundLiteral (tn, init) ->
    Printf.sprintf "(%s)%s" (show_type_name_exp tn.exp) (show_init_exp init.exp)

and show_i_expr_short i_expr = match i_expr with
| Const x -> Printf.sprintf "%d" x
| Label label -> label
| LVar offset -> Printf.sprintf "$%d" offset
| Load (ty, e) -> Printf.sprintf "*[%s](%s)" (Type.show_type ty) (show_i_expr_short e)
| ICall (f, params) -> Printf.sprintf "(%s)(%s)" (show_i_expr_short f) (String.concat ", " (List.map show_i_expr_short params))
| I_binop (op, l, r) -> Printf.sprintf "(%s)%s(%s)" (show_i_expr_short l) (show_binop_short op) (show_i_expr_short r)
| I_block _ -> "{...}"
| ICond (c, t, e) -> Printf.sprintf "(%s)?(%s):(%s)" (show_i_expr_short c) (show_i_expr_short t) (show_i_expr_short e)
| IBitComplement e -> Printf.sprintf "~(%s)" (show_i_expr_short e)
| IBoolOfRetval e -> Printf.sprintf "BoolOfRetval(%s)" (show_i_expr_short e)
| IBoolOfInt e -> Printf.sprintf "BoolOfInt(%s)" (show_i_expr_short e)
| ICharOfInt e -> Printf.sprintf "CharOfInt(%s)" (show_i_expr_short e)
| IShortOfInt e -> Printf.sprintf "ShortOfInt(%s)" (show_i_expr_short e)
| IIntOfInt e -> Printf.sprintf "IntOfInt(%s)" (show_i_expr_short e)
| Error _ -> Printf.sprintf "Error(...)"

and show_binop_short op = match op with
| Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%"
| Lt -> "<"
| Le -> "<="
| Eq -> "=="
| Ne -> "!="
| BitAnd -> "&"
| BitXor -> "^"
| BitOr -> "|"
| LShift -> "<<"
| RShift -> ">>"
| LAnd -> "&&"
| LOr -> "||"
| Comma -> ","
| Store ty -> Printf.sprintf "<-[%s]" (Type.show_type ty)

let make_expr_s expr = { expr = expr; i_expr = None }

let is_extern ds = match ds with
| { ds_storage_class_spec = Some { exp = Extern; _ }; _ } -> true
| _ -> false

let is_static ds = match ds with
| { ds_storage_class_spec = Some { exp = Static; _ }; _ } -> true
| _ -> false

(* l <op>= r は適当なtmpを用意して { tmp=&l; *tmp = *tmp <op> r; } にする *)
let op_assign op loc l r =
    let tmp_name = "#tmp#" in
    let tmp = { exp = Ident tmp_name; loc = l.loc } in
    let deref_tmp = { exp = Deref tmp; loc = tmp.loc } in
    let addr_of_l = { exp = Addr l; loc = l.loc } in 
    let expr_st expr = { exp = Expr (make_expr_s expr); loc = expr.loc } in
    let stmts = [
            { exp = TmpVar (tmp_name, addr_of_l); loc = l.loc };
            expr_st { exp = Assign (tmp, addr_of_l); loc=l.loc };
            expr_st {
                exp = Assign (
                    deref_tmp,
                    { exp = Binop(op, deref_tmp, r); loc=loc }
                );
                loc = loc
            }
        ] in
    let stmt_list = { exp = StmtList stmts; loc = loc } in
    let block = { exp = Block stmt_list; loc = loc } in
    { exp = BlockExpr block; loc = block.loc }

(* 左辺値である式lの値を保存しそれを更新するような式を実行後 *)
(* 保存した値を返すブロック式に変換する *)
(* { #tmp# = &l; #save# = *#tmp#; *#tmp#を使った式; #save# } *)
let save_and_return_l_with l f =
    let save_name = "#save#" in
    let save = { exp = Ident save_name; loc = l.loc } in
    let tmp_name = "#tmp#" in
    let tmp = { exp = Ident tmp_name; loc = l.loc } in
    let deref_tmp = { exp = Deref tmp; loc = tmp.loc } in
    let addr_of_l = { exp = Addr l; loc = l.loc } in 
    let expr_st expr = { exp = Expr (make_expr_s expr); loc = expr.loc } in
    let stmts = [
            { exp = TmpVar (save_name, l); loc = l.loc };
            { exp = TmpVar (tmp_name, addr_of_l); loc = l.loc };
            expr_st { exp = Assign (tmp, addr_of_l); loc=l.loc };
            expr_st { exp = Assign (save, deref_tmp); loc=l.loc };
            expr_st @@ f deref_tmp;
            expr_st save
        ] in
    let stmt_list = { exp = StmtList stmts; loc = l.loc } in
    let block = { exp = Block stmt_list; loc = l.loc } in
    { exp = BlockExpr block; loc = block.loc }
