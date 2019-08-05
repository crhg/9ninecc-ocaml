val init_global : Type.t -> string -> Ast.init -> unit
val init_data : Type.t -> Ast.init -> unit
val init_data_scalar : (Ast.expr -> unit) -> Ast.init -> unit
val out_char : Ast.expr -> unit
val out_int : Ast.expr -> unit
val out_pointer : Ast.expr -> unit
val init_data_char : Ast.init -> unit
val init_data_int : Ast.init -> unit
val init_data_pointer : Ast.init -> unit
val init_data_array : Type.t -> int -> Ast.init -> unit
val init_data_by_list : Type.t -> int -> Ast.init list -> unit
val init_str : int -> string -> unit
val init_zero : int -> unit
