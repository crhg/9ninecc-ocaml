type entry =
    LocalVar of Type.t * int
  | GlobalVar of Type.t * string
  | TypeDef of Type.t
val pp_entry :
  Ppx_deriving_runtime.Format.formatter -> entry -> Ppx_deriving_runtime.unit
val show_entry : entry -> Ppx_deriving_runtime.string
val with_new_local_frame : (unit -> 'a) -> int
val with_new_scope : (unit -> 'a) -> unit
val register_local_var : Type.t -> string -> unit
val register_global_var : Type.t -> string -> unit
val register_typedef : Type.t -> string -> unit
val get_entry : string -> entry
val entry_type : entry -> Type.t
val register_tag : string -> Type.t -> unit
val get_tag : string -> Type.t
val get_tag_opt : string -> Type.t option
val defined_tag_in_current_scope : string -> bool
