val prepare_func : (Type.t * Env.Env.key) list -> Ast.stmt -> unit
val allocate_stmt : Ast.stmt -> unit
val prepare_init : Ast.init -> unit
val assign_type_plane : Ast.expr -> Type.t
val normalize_type : Type.t -> Type.t
val assign_type : Ast.expr -> Type.t
val find_type : Ast.expr -> Type.t
val type_and_var : Type.t -> Ast.declarator -> Type.t * Env.Env.key
