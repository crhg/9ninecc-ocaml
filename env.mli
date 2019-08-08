type entry = LocalVar of Type.t * int | GlobalVar of Type.t * string
val pp_entry :
  Ppx_deriving_runtime.Format.formatter -> entry -> Ppx_deriving_runtime.unit
val show_entry : entry -> Ppx_deriving_runtime.string
val with_new_local_frame : (unit -> 'a) -> int
val with_new_scope : (unit -> 'a) -> unit
val register_local_var : Type.t -> string -> unit
val register_global_var : Type.t -> string -> unit
val get_entry : string -> entry
val entry_type : entry -> Type.t
