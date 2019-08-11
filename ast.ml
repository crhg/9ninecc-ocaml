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
| Expr of expr
| Return of expr
| If of expr * stmt * stmt option
| While of expr * stmt
| For of expr option * expr option * expr option * stmt
| Block of stmt list
and stmt = stmt_exp node

and 't with_type = {
    e: 't;
    mutable ty: Type.t option
    [@printer fun fmt ty -> match ty with
    | None -> fprintf fmt "?"
    | Some ty -> fprintf fmt "%s" (Type.show_type ty)
    ]
}

and expr_e =
| Num of string
| Str of string * string (* 文字列そのものとラベル *)
| Ident of { 
    name : string;
    mutable entry : Env.entry option
    [@printer fun fmt entry -> match entry with
    | None -> fprintf fmt "?"
    | Some entry -> fprintf fmt "%s" (Env.show_entry entry)
    ]
}
| Add of expr * expr
| Sub of expr * expr
| Mul of expr * expr
| Div of expr * expr
| Lt of expr * expr
| Le of expr * expr
| Eq of expr * expr
| Ne of expr * expr
| Assign of expr * expr
| Call of string * expr list
| Deref of expr
| Addr of expr
| Sizeof of expr
| Arrow of expr * string
| BlockExpr of stmt
and expr_exp = expr_e with_type
and expr = expr_exp node
[@@deriving show {with_path = false}]

let no_type e = { e = e; ty = None }

