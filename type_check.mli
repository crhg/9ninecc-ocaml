val prepare_func : (Type.t * string) list -> Ast.stmt -> int
val allocate_stmt : Ast.stmt -> unit
val prepare_init : Ast.init -> unit
val assign_type : Ast.expr -> Type.t
val type_and_var : Type.t -> Ast.declarator -> Type.t * string
