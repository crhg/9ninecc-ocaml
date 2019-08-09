type t =
    Int
  | Char
  | Ptr of t
  | Array of t * int option
  | Function of t * (string * t) list
  | Struct of st_un
and field = { field_name : string; field_type : t; field_offset : int; }
and st_un_exp = { fields : field list; size : int; alignment : int; }
and st_un = { mutable exp : st_un_exp option; }
val pp :
  Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit
val show : t -> Ppx_deriving_runtime.string
val pp_field :
  Ppx_deriving_runtime.Format.formatter -> field -> Ppx_deriving_runtime.unit
val show_field : field -> Ppx_deriving_runtime.string
val pp_st_un_exp :
  Ppx_deriving_runtime.Format.formatter ->
  st_un_exp -> Ppx_deriving_runtime.unit
val show_st_un_exp : st_un_exp -> Ppx_deriving_runtime.string
val pp_st_un :
  Ppx_deriving_runtime.Format.formatter -> st_un -> Ppx_deriving_runtime.unit
val show_st_un : st_un -> Ppx_deriving_runtime.string
val get_size : t -> int
val get_alignment : t -> int
val is_complete_type : t -> bool
val get_field : t -> string -> field
