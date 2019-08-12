type 't node = { exp : 't; loc : Lexing.position; }
and param = {
  param_ty : Type.t;
  param_name : string;
  mutable param_entry : Env.entry option;
  param_loc : Lexing.position;
}
and st_un = {
  su_tag : string option;
  su_fields : (type_spec * declarator) list option;
}
and type_spec_exp =
    Long
  | Int
  | Short
  | Char
  | Struct of st_un
  | Union of st_un
and type_spec = type_spec_exp node
and decl_exp =
    FunctionDecl of { func_ts : type_spec; func_decl : declarator;
      func_body : stmt; mutable func_ty : Type.t option;
      mutable func_name : string option;
      mutable func_params : param list option;
      mutable func_frame_size : int option;
    }
  | GlobalVarDecl of { gv_ts : type_spec; gv_decl_inits : decl_init list; }
and decl = decl_exp node
and decl_init = {
  di_decl : declarator;
  di_init : init option;
  mutable di_entry : Env.entry option;
  mutable di_init_assign : expr list;
}
and declarator_exp =
    DeclIdent of string
  | PointerOf of declarator
  | Array of declarator * expr option
  | Func of declarator * (type_spec * declarator) list
and declarator = declarator_exp node
and init_exp = ExprInitializer of expr | ListInitializer of init list
and init = init_exp node
and stmt_exp =
    Empty
  | Var of { var_ts : type_spec; var_decl_inits : decl_init list; }
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | For of expr option * expr option * expr option * stmt
  | Block of stmt list
and stmt = stmt_exp node
and 't with_type = { e : 't; mutable with_type_ty : Type.t option; }
and binop =
    Add
  | Sub
  | PtrAdd of int
  | PtrSub of int
  | PtrDiff of int
  | Mul
  | Div
  | Lt
  | Le
  | Eq
  | Ne
and binop_r = { mutable op : binop; mutable lhs : expr; mutable rhs : expr; }
and ident_r = { name : string; mutable entry : Env.entry option; }
and assign_r = {
  assign_lhs : expr;
  assign_rhs : expr;
  mutable assign_lhs_type : Type.t option;
}
and deref_r = { deref_expr : expr; mutable deref_type : Type.t option; }
and sizeof_r = { sizeof_expr : expr; mutable sizeof_size : int; }
and arrow_r = {
  arrow_expr : expr;
  arrow_field : string;
  mutable arrow_field_type : Type.t option;
  mutable arrow_field_offset : int;
}
and expr_e =
    Num of string
  | Str of string * string
  | Ident of ident_r
  | Binop of binop_r
  | Assign of assign_r
  | Call of string * expr list
  | Deref of deref_r
  | Addr of expr
  | Sizeof of sizeof_r
  | Arrow of arrow_r
  | BlockExpr of stmt
and expr_exp = expr_e with_type
and expr = expr_exp node
val pp_node :
  (Ppx_deriving_runtime.Format.formatter -> 't -> Ppx_deriving_runtime.unit) ->
  Ppx_deriving_runtime.Format.formatter ->
  't node -> Ppx_deriving_runtime.unit
val show_node :
  (Ppx_deriving_runtime.Format.formatter -> 't -> Ppx_deriving_runtime.unit) ->
  't node -> Ppx_deriving_runtime.string
val pp_param :
  Ppx_deriving_runtime.Format.formatter -> param -> Ppx_deriving_runtime.unit
val show_param : param -> Ppx_deriving_runtime.string
val pp_st_un :
  Ppx_deriving_runtime.Format.formatter -> st_un -> Ppx_deriving_runtime.unit
val show_st_un : st_un -> Ppx_deriving_runtime.string
val pp_type_spec_exp :
  Ppx_deriving_runtime.Format.formatter ->
  type_spec_exp -> Ppx_deriving_runtime.unit
val show_type_spec_exp : type_spec_exp -> Ppx_deriving_runtime.string
val pp_type_spec :
  Ppx_deriving_runtime.Format.formatter ->
  type_spec -> Ppx_deriving_runtime.unit
val show_type_spec : type_spec -> Ppx_deriving_runtime.string
val pp_decl_exp :
  Ppx_deriving_runtime.Format.formatter ->
  decl_exp -> Ppx_deriving_runtime.unit
val show_decl_exp : decl_exp -> Ppx_deriving_runtime.string
val pp_decl :
  Ppx_deriving_runtime.Format.formatter -> decl -> Ppx_deriving_runtime.unit
val show_decl : decl -> Ppx_deriving_runtime.string
val pp_decl_init :
  Ppx_deriving_runtime.Format.formatter ->
  decl_init -> Ppx_deriving_runtime.unit
val show_decl_init : decl_init -> Ppx_deriving_runtime.string
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
val pp_binop :
  Ppx_deriving_runtime.Format.formatter -> binop -> Ppx_deriving_runtime.unit
val show_binop : binop -> Ppx_deriving_runtime.string
val pp_binop_r :
  Ppx_deriving_runtime.Format.formatter ->
  binop_r -> Ppx_deriving_runtime.unit
val show_binop_r : binop_r -> Ppx_deriving_runtime.string
val pp_ident_r :
  Ppx_deriving_runtime.Format.formatter ->
  ident_r -> Ppx_deriving_runtime.unit
val show_ident_r : ident_r -> Ppx_deriving_runtime.string
val pp_assign_r :
  Ppx_deriving_runtime.Format.formatter ->
  assign_r -> Ppx_deriving_runtime.unit
val show_assign_r : assign_r -> Ppx_deriving_runtime.string
val pp_deref_r :
  Ppx_deriving_runtime.Format.formatter ->
  deref_r -> Ppx_deriving_runtime.unit
val show_deref_r : deref_r -> Ppx_deriving_runtime.string
val pp_sizeof_r :
  Ppx_deriving_runtime.Format.formatter ->
  sizeof_r -> Ppx_deriving_runtime.unit
val show_sizeof_r : sizeof_r -> Ppx_deriving_runtime.string
val pp_arrow_r :
  Ppx_deriving_runtime.Format.formatter ->
  arrow_r -> Ppx_deriving_runtime.unit
val show_arrow_r : arrow_r -> Ppx_deriving_runtime.string
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
val show_expr_short : expr -> string
