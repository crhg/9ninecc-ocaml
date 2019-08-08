type 't node = {
    exp: 't;
    loc: Lexing.position [@opaque]
}

and decl_exp =
| FunctionDecl of Type.t * string * (Type.t * string) list * stmt
| GlobalVarDecl of Type.t * declarator * init option
and decl = decl_exp node

and declarator_exp =
| DeclIdent of string
| PointerOf of declarator
| Array of declarator * expr option
| Func of declarator * (Type.t * string) list
and declarator = declarator_exp node

and init_exp =
| ExprInitializer of expr
| ListInitializer of init list
and init = init_exp node

and stmt_exp = 
| Empty
| Var of Type.t * declarator * init option
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
}

and expr_e =
| Num of string
| Str of string * string (* 文字列そのものとラベル *)
| Ident of { name : string; mutable entry : Env.entry option }
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
and expr_exp = expr_e with_type
and expr = expr_exp node
[@@deriving show]

let no_type e = { e = e; ty = None }

