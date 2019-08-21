type 't node = {
    exp: 't;
    loc: Lexing.position [@opaque]
}

and param = {
    param_ty: Type.t;
    param_name: string;
    mutable param_entry: Env.entry option
    [@priter fun fmt entry -> match entry with
    | None -> fpriintf fmt "?"
    | Some entry -> fprintf fmt "%s" (Env.show_entry entry)
    ];
    param_loc: Lexing.position [@opaque]
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
and storage_class_spec = storage_class_spec_exp node

and type_spec_exp =
| Long
| Int
| Short
| Char
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
    mutable func_ty: Type.t option;
    mutable func_name: string option;
    mutable func_params: param list option;
    mutable func_frame_size: int option (* ローカル変数領域に必要なサイズ。type_check時に決まる *)
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
    mutable di_entry : Env.entry option (* グローバル変数用。ここからラベルと型を知る *)
    [@priter fun fmt entry -> match entry with
    | None -> fpriintf fmt "?"
    | Some entry -> fprintf fmt "%s" (Env.show_entry entry)
    ];
    mutable di_init_assign: i_expr list (* ローカル変数用。初期化を行う代入式のリスト。型が決まってから設定 *)
}

and declarator_exp =
| DeclIdent of string
| PointerOf of declarator
| Array of declarator * expr option
| Func of declarator * (type_spec * declarator) list
and declarator = declarator_exp node

and init_exp =
| ExprInitializer of expr_s
| ListInitializer of init list
and init = init_exp node

and stmt_exp = 
| Empty
| Var of  {
    var_ds: decl_spec;
    var_decl_inits: decl_init list
}
| TypedefStmt of type_spec * declarator list
| Expr of expr_s
| Return of expr_s
| If of expr_s * stmt * stmt option
| While of expr_s * stmt
| For of expr_s option * expr_s option * expr_s option * stmt
| Block of stmt list
and stmt = stmt_exp node

and binop =
| Add
| Sub
| Mul
| Div
| Lt
| Le
| Eq
| Ne
| Store of Type.t

and binop_r = {
    op: binop;
    lhs: expr;
    rhs: expr
}

and ident_r = { 
    name : string;
}

and assign_r = {
    assign_lhs : expr;
    assign_rhs : expr
}

and deref_r = {
    deref_expr : expr;
}

and sizeof_r = {
    sizeof_expr : expr;
}

and arrow_r = {
    arrow_expr : expr;
    arrow_field : string;
}

and expr_exp =
| Num of string
| Str of string * string (* 文字列そのものとラベル *)
| Ident of ident_r
| Binop of binop_r
| Assign of assign_r
| Call of string * expr list
| Deref of deref_r
| Addr of expr
| Sizeof of sizeof_r
| Arrow of arrow_r
| Cast of type_name * expr
| BlockExpr of stmt
and expr = expr_exp node

(* 式の中間表現 *)
and i_expr =
| Const of int
| Label of string (* ラベルのアドレス *)
| LVar of int (* 指定されたオフセットの位置にあるローカル変数のアドレス *)
| Load of Type.t * i_expr
| ICall of string * i_expr list
| I_binop of binop * i_expr * i_expr
| I_block of stmt

(* 文の中に置く式, 中間表現に変換した結果を格納できる *)
and expr_s = { expr: expr; mutable i_expr: i_expr option }

[@@deriving show {with_path = false}]

let rec show_expr_short expr = match expr.exp with
| Num n ->
    n
| Str (s, _) ->
    Printf.sprintf "\"%s\"" (String_literal.escaped s)
| Ident { name = name } ->
    name
| Binop { op=op; lhs=l; rhs=r } ->
    Printf.sprintf "(%s)%s(%s)" (show_expr_short l) (show_binop_short op) (show_expr_short r)
| Assign { assign_lhs = l; assign_rhs = r } ->
    Printf.sprintf "(%s)=(%s)" (show_expr_short l) (show_expr_short r)
| Call (f, params) ->
    Printf.sprintf "%s(%s)" f (String.concat ", " (List.map show_expr_short params))
| Deref { deref_expr = e } ->
    Printf.sprintf "*(%s)" (show_expr_short e)
| Addr e ->
    Printf.sprintf "&(%s)" (show_expr_short e)
| Sizeof {sizeof_expr = e} ->
    Printf.sprintf "sizeof(%s)" (show_expr_short e)
| Arrow { arrow_expr = e; arrow_field = f} ->
    Printf.sprintf "(%s)->(%s)" (show_expr_short e) f
| Cast(_, e) ->
    Printf.sprintf "Cast(...,%s)" (show_expr_short e)
| BlockExpr _ ->
    "{...}"

and show_i_expr_short i_expr = match i_expr with
| Const x -> Printf.sprintf "%d" x
| Label label -> label
| LVar offset -> Printf.sprintf "$%d" offset
| Load (ty, e) -> Printf.sprintf "*[%s](%s)" (Type.show_type ty) (show_i_expr_short e)
| ICall (f, params) -> Printf.sprintf "%s(%s)" f (String.concat ", " (List.map show_i_expr_short params))
| I_binop (op, l, r) -> Printf.sprintf "(%s)%s(%s)" (show_i_expr_short l) (show_binop_short op) (show_i_expr_short r)
| I_block _ -> "{...}"

and show_binop_short op = match op with
| Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Lt -> "<"
| Le -> "<="
| Eq -> "=="
| Ne -> "!="
| Store ty -> Printf.sprintf "<-[%s]" (Type.show_type ty)


let make_expr_s expr = { expr = expr; i_expr = None }

let is_extern ds = match ds with
| { ds_storage_class_spec = Some { exp = Extern; _ }; _ } -> true
| _ -> false
