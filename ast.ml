type 't node = {
    exp: 't;
    loc: Lexing.position [@opaque]
}

and decl_exp =
| Function of Type.t * string * (Type.t * string) list * stmt
| GlobalVarDef of Type.t * string
and decl = decl_exp node

and declarator_exp =
| DeclIdent of string
| PointerOf of declarator
| Array of declarator * int
| Func of declarator * (Type.t * string) list
and declarator = declarator_exp node

and stmt_exp = 
| Var of Type.t * string
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
| Str of string
| Ident of string * Env.entry ref
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

let rec type_and_var t d = match d.exp with
| DeclIdent var -> (t, var)
| PointerOf d -> type_and_var (Type.Ptr t) d
| Array (d,n) -> type_and_var (Type.Array(t, n)) d 
| Func (d, params) ->
    let param_type_list = List.map (fun (t, _)->t) params in
    type_and_var (Type.Function(t, param_type_list)) d
