type 't node = { exp : 't; loc : Lexing.position; }
and decl_exp =
    FunctionDecl of Type.t * string * (Type.t * string) list * stmt
  | GlobalVarDecl of Type.t * declarator * init option
and decl = decl_exp node
and declarator_exp =
    DeclIdent of string
  | PointerOf of declarator
  | Array of declarator * expr option
  | Func of declarator * (Type.t * string) list
and declarator = declarator_exp node
and init_exp = ExprInitializer of expr | ListInitializer of init list
and init = init_exp node
and stmt_exp =
    Var of Type.t * declarator * init option
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | For of expr option * expr option * expr option * stmt
  | Block of stmt list
and stmt = stmt_exp node
and 't with_type = { e : 't; mutable ty : Type.t option; }
and expr_e =
    Num of string
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
val pp_node :
  (Ppx_deriving_runtime.Format.formatter -> 't -> Ppx_deriving_runtime.unit) ->
  Ppx_deriving_runtime.Format.formatter ->
  't node -> Ppx_deriving_runtime.unit
val show_node :
  (Ppx_deriving_runtime.Format.formatter -> 't -> Ppx_deriving_runtime.unit) ->
  't node -> Ppx_deriving_runtime.string
val pp_decl_exp :
  Ppx_deriving_runtime.Format.formatter ->
  decl_exp -> Ppx_deriving_runtime.unit
val show_decl_exp : decl_exp -> Ppx_deriving_runtime.string
val pp_decl :
  Ppx_deriving_runtime.Format.formatter -> decl -> Ppx_deriving_runtime.unit
val show_decl : decl -> Ppx_deriving_runtime.string
val pp_declarator_exp :
  Ppx_deriving_runtime.Format.formatter ->
  declarator_exp -> Ppx_deriving_runtime.unit
val show_declarator_exp : declarator_exp -> Ppx_deriving_runtime.string
val pp_declarator :
  Ppx_deriving_runtime.Format.formatter ->
  declarator -> Ppx_deriving_runtime.unit
val show_declarator : declarator -> Ppx_deriving_runtime.string
val pp_init_exp :
  Ppx_deriving_runtime.Format.formatter ->
  init_exp -> Ppx_deriving_runtime.unit
val show_init_exp : init_exp -> Ppx_deriving_runtime.string
val pp_init :
  Ppx_deriving_runtime.Format.formatter -> init -> Ppx_deriving_runtime.unit
val show_init : init -> Ppx_deriving_runtime.string
val pp_stmt_exp :
  Ppx_deriving_runtime.Format.formatter ->
  stmt_exp -> Ppx_deriving_runtime.unit
val show_stmt_exp : stmt_exp -> Ppx_deriving_runtime.string
val pp_stmt :
  Ppx_deriving_runtime.Format.formatter -> stmt -> Ppx_deriving_runtime.unit
val show_stmt : stmt -> Ppx_deriving_runtime.string
val pp_with_type :
  (Ppx_deriving_runtime.Format.formatter -> 't -> Ppx_deriving_runtime.unit) ->
  Ppx_deriving_runtime.Format.formatter ->
  't with_type -> Ppx_deriving_runtime.unit
val show_with_type :
  (Ppx_deriving_runtime.Format.formatter -> 't -> Ppx_deriving_runtime.unit) ->
  't with_type -> Ppx_deriving_runtime.string
val pp_expr_e :
  Ppx_deriving_runtime.Format.formatter ->
  expr_e -> Ppx_deriving_runtime.unit
val show_expr_e : expr_e -> Ppx_deriving_runtime.string
val pp_expr_exp :
  Ppx_deriving_runtime.Format.formatter ->
  expr_exp -> Ppx_deriving_runtime.unit
val show_expr_exp : expr_exp -> Ppx_deriving_runtime.string
val pp_expr :
  Ppx_deriving_runtime.Format.formatter -> expr -> Ppx_deriving_runtime.unit
val show_expr : expr -> Ppx_deriving_runtime.string
val no_type : 'a -> 'a with_type
