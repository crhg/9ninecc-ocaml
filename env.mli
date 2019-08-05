type entry =
    DummyEntry
  | LocalVar of Type.t * int
  | GlobalVar of Type.t * string
val pp_entry :
  Ppx_deriving_runtime.Format.formatter -> entry -> Ppx_deriving_runtime.unit
val show_entry : entry -> Ppx_deriving_runtime.string
val reset : 'a -> unit
exception DuplicatedLocal of Type.t * string
val register_local_var : Type.t -> string -> unit
exception DuplicatedGlobal of Type.t * string
val register_global_var : Type.t -> string -> unit
val local_var_size : 'a -> int
val get_entry : string -> entry
val entry_type : entry -> Type.t
