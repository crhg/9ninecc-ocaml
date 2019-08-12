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

and type_spec_exp =
| Long
| Int
| Short
| Char
| Struct of st_un
| Union of st_un
| Type of string
and type_spec = type_spec_exp node

and decl_exp =
| FunctionDecl of {
    func_ts: type_spec;
    func_decl: declarator;
    func_body: stmt;
    mutable func_ty: Type.t option;
    mutable func_name: string option;
    mutable func_params: param list option;
    mutable func_frame_size: int option (* ローカル変数領域に必要なサイズ。type_check時に決まる *)
}
| GlobalVarDecl of {
    gv_ts: type_spec;
    gv_decl_inits: decl_init list
}
| TypedefDecl of type_spec * string
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
    mutable di_init_assign: expr list (* ローカル変数用。初期化を行う代入式のリスト。型が決まってから設定 *)
}

and declarator_exp =
| DeclIdent of string
| PointerOf of declarator
| Array of declarator * expr option
| Func of declarator * (type_spec * declarator) list
and declarator = declarator_exp node

and init_exp =
| ExprInitializer of expr
| ListInitializer of init list
and init = init_exp node

and stmt_exp = 
| Empty
| Var of  {
    var_ts: type_spec;
    var_decl_inits: decl_init list
}
| Typedef of type_spec * string
| Expr of expr
| Return of expr
| If of expr * stmt * stmt option
| While of expr * stmt
| For of expr option * expr option * expr option * stmt
| Block of stmt list
and stmt = stmt_exp node

and binop =
| Add
| Sub
| PtrAdd of int (* ポインタ+整数 *)
| PtrSub of int (* ポインタ-整数 *)
| PtrDiff of int (* ポインタ-ポインタ *)
| Mul
| Div
| Lt
| Le
| Eq
| Ne

and binop_r = {
    mutable op: binop;
    mutable lhs: expr;
    mutable rhs: expr
}

and ident_r = { 
    name : string;
    mutable entry : Env.entry option
    [@printer fun fmt entry -> match entry with
    | None -> fprintf fmt "?"
    | Some entry -> fprintf fmt "%s" (Env.show_entry entry)
    ]
}

and assign_r = {
    assign_lhs : expr;
    assign_rhs : expr;
    mutable assign_lhs_type : Type.t option
}

and deref_r = {
    deref_expr : expr;
    mutable deref_type: Type.t option
}

and sizeof_r = {
    sizeof_expr : expr;
    mutable sizeof_size : int
}

and arrow_r = {
    arrow_expr : expr;
    arrow_field : string;
    mutable arrow_field_type: Type.t option;
    mutable arrow_field_offset : int
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
| BlockExpr of stmt
and expr = expr_exp node
[@@deriving show {with_path = false}]

let rec show_expr_short expr = match expr.exp with
| Num n ->
    n
| Str (s, _) ->
    Printf.sprintf "\"%s\"" (String_literal.escaped s)
| Ident { name = name } ->
    name
| Binop { op=op; lhs=l; rhs=r } ->
    Printf.sprintf "(%s %s %s)" (show_binop op) (show_expr_short l) (show_expr_short r)
| Assign { assign_lhs = l; assign_rhs = r } ->
    Printf.sprintf "(= %s %s)" (show_expr_short l) (show_expr_short r)
| Call (f, params) ->
    Printf.sprintf "(%s %s)" f (String.concat " " (List.map show_expr_short params))
| Deref { deref_expr = e } ->
    Printf.sprintf "*%s" (show_expr_short e)
| Addr e ->
    Printf.sprintf "&%s" (show_expr_short e)
| Sizeof {sizeof_expr = e} ->
    Printf.sprintf "(sizeof %s)" (show_expr_short e)
| Arrow { arrow_expr = e; arrow_field = f} ->
    Printf.sprintf "(-> %s %s)" (show_expr_short e) f
| BlockExpr _ ->
    "{...}"
