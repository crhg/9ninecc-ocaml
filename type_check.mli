val check : Ast.decl list -> unit
val var_of_d : Ast.declarator -> string
val type_and_var_ty: Type.t -> Ast.declarator -> Type.t * string
val convert : Ast.expr -> Type.t * Ast.i_expr
