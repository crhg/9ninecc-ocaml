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
and enum = { enum_tag : string option; enum_list : enumarator list option; }
and enumerator_exp = { en_name : string; en_expr : expr_s option; }
and enumarator = enumerator_exp node
and type_name_exp = {
  type_name_ts : type_spec;
  type_name_decl : declarator;
}
and type_name = type_name_exp node
and decl_spec = {
  ds_type_spec : type_spec option;
  ds_storage_class_spec : storage_class_spec option;
}
and storage_class_spec_exp = Typedef | Extern
and storage_class_spec = storage_class_spec_exp node
and type_spec_exp =
    Long
  | Int
  | Short
  | Char
  | Struct of st_un
  | Union of st_un
  | Enum of enum
  | Type of string
and type_spec = type_spec_exp node
and function_decl_r = {
  func_ds : decl_spec;
  func_decl : declarator;
  func_body : stmt;
  mutable func_ty : Type.t option;
  mutable func_name : string option;
  mutable func_params : param list option;
  mutable func_frame_size : int option;
}
and decl_exp =
    FunctionDecl of function_decl_r
  | GlobalVarDecl of { gv_ds : decl_spec; gv_decl_inits : decl_init list; }
  | TypedefDecl of type_spec * declarator list
  | DummyDecl
and decl = decl_exp node
and decl_init = {
  di_decl : declarator;
  di_init : init option;
  mutable di_entry : Env.entry option;
  mutable di_init_assign : i_expr list;
}
and declarator_exp =
    DeclIdent of string
  | PointerOf of declarator
  | Array of declarator * expr option
  | Func of declarator * (type_spec * declarator) list
and declarator = declarator_exp node
and init_exp = ExprInitializer of expr_s | ListInitializer of init list
and init = init_exp node
and stmt_exp =
    Empty
  | Var of { var_ds : decl_spec; var_decl_inits : decl_init list; }
  | TypedefStmt of type_spec * declarator list
  | Expr of expr_s
  | Return of expr_s
  | If of expr_s * stmt * stmt option
  | While of expr_s * stmt
  | For of expr_s option * expr_s option * expr_s option * stmt
  | Block of stmt list
and stmt = stmt_exp node
and binop = Add | Sub | Mul | Div | Lt | Le | Eq | Ne | Store of Type.t
and binop_r = { op : binop; lhs : expr; rhs : expr; }
and ident_r = { name : string; }
and assign_r = { assign_lhs : expr; assign_rhs : expr; }
and deref_r = { deref_expr : expr; }
and sizeof_r = { sizeof_expr : expr; }
and arrow_r = { arrow_expr : expr; arrow_field : string; }
and expr_exp =
    Num of string
  | Str of string * string
  | Ident of ident_r
  | Binop of binop_r
  | Assign of assign_r
  | Call of expr * expr list
  | Deref of deref_r
  | Addr of expr
  | Sizeof of sizeof_r
  | Arrow of arrow_r
  | Cast of type_name * expr
  | BlockExpr of stmt
and expr = expr_exp node
and i_expr =
    Const of int
  | Label of string
  | LVar of int
  | Load of Type.t * i_expr
  | ICall of i_expr * i_expr list
  | I_binop of binop * i_expr * i_expr
  | I_block of stmt
and expr_s = { expr : expr; mutable i_expr : i_expr option; }
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
val pp_enum :
  Ppx_deriving_runtime.Format.formatter -> enum -> Ppx_deriving_runtime.unit
val show_enum : enum -> Ppx_deriving_runtime.string
val pp_enumerator_exp :
  Ppx_deriving_runtime.Format.formatter ->
  enumerator_exp -> Ppx_deriving_runtime.unit
val show_enumerator_exp : enumerator_exp -> Ppx_deriving_runtime.string
val pp_enumarator :
  Ppx_deriving_runtime.Format.formatter ->
  enumarator -> Ppx_deriving_runtime.unit
val show_enumarator : enumarator -> Ppx_deriving_runtime.string
val pp_type_name_exp :
  Ppx_deriving_runtime.Format.formatter ->
  type_name_exp -> Ppx_deriving_runtime.unit
val show_type_name_exp : type_name_exp -> Ppx_deriving_runtime.string
val pp_type_name :
  Ppx_deriving_runtime.Format.formatter ->
  type_name -> Ppx_deriving_runtime.unit
val show_type_name : type_name -> Ppx_deriving_runtime.string
val pp_decl_spec :
  Ppx_deriving_runtime.Format.formatter ->
  decl_spec -> Ppx_deriving_runtime.unit
val show_decl_spec : decl_spec -> Ppx_deriving_runtime.string
val pp_storage_class_spec_exp :
  Ppx_deriving_runtime.Format.formatter ->
  storage_class_spec_exp -> Ppx_deriving_runtime.unit
val show_storage_class_spec_exp :
  storage_class_spec_exp -> Ppx_deriving_runtime.string
val pp_storage_class_spec :
  Ppx_deriving_runtime.Format.formatter ->
  storage_class_spec -> Ppx_deriving_runtime.unit
val show_storage_class_spec :
  storage_class_spec -> Ppx_deriving_runtime.string
val pp_type_spec_exp :
  Ppx_deriving_runtime.Format.formatter ->
  type_spec_exp -> Ppx_deriving_runtime.unit
val show_type_spec_exp : type_spec_exp -> Ppx_deriving_runtime.string
val pp_type_spec :
  Ppx_deriving_runtime.Format.formatter ->
  type_spec -> Ppx_deriving_runtime.unit
val show_type_spec : type_spec -> Ppx_deriving_runtime.string
val pp_function_decl_r :
  Ppx_deriving_runtime.Format.formatter ->
  function_decl_r -> Ppx_deriving_runtime.unit
val show_function_decl_r : function_decl_r -> Ppx_deriving_runtime.string
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
val pp_expr_exp :
  Ppx_deriving_runtime.Format.formatter ->
  expr_exp -> Ppx_deriving_runtime.unit
val show_expr_exp : expr_exp -> Ppx_deriving_runtime.string
val pp_expr :
  Ppx_deriving_runtime.Format.formatter -> expr -> Ppx_deriving_runtime.unit
val show_expr : expr -> Ppx_deriving_runtime.string
val pp_i_expr :
  Ppx_deriving_runtime.Format.formatter ->
  i_expr -> Ppx_deriving_runtime.unit
val show_i_expr : i_expr -> Ppx_deriving_runtime.string
val pp_expr_s :
  Ppx_deriving_runtime.Format.formatter ->
  expr_s -> Ppx_deriving_runtime.unit
val show_expr_s : expr_s -> Ppx_deriving_runtime.string
val show_expr_short : expr -> string
val show_i_expr_short : i_expr -> string
val show_binop_short : binop -> string
val make_expr_s : expr -> expr_s
val is_extern : decl_spec -> bool
