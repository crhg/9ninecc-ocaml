val eval_int : Ast.expr -> int
val eval_pointer : Ast.expr -> string * int
val add_pointer : Ast.expr -> Ast.expr -> string * int
val sub_pointer : Ast.expr -> Ast.expr -> string * int
val is_global_array_entry : Env.entry -> bool
val get_label_from_entry : Env.entry -> string
val eval_lval : Ast.expr -> string * int
