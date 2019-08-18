val check : Ast.decl list -> unit
val type_and_var_ty: Type.t -> Ast.declarator -> Type.t * string
val convert : Ast.expr -> Type.t * Ast.i_expr
