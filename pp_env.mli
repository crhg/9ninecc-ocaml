type entry = 
| ObjectMacro of Pp_ast.pp_token list
| FunctionMacro of string list * Pp_ast.pp_token list
val pp_entry :
  Ppx_deriving_runtime.Format.formatter -> entry -> Ppx_deriving_runtime.unit
val show_entry : entry -> Ppx_deriving_runtime.string
val mem : string -> bool
val find : string -> entry
val find_opt : string -> entry option
val add : string -> entry -> unit
val remove : string -> unit
