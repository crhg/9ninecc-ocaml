type t =
    Void
  | Long
  | Int
  | Short
  | Char
  | Bool
  | Ptr of t
  | Array of t * int option
  | Function of t * (string * t) list
  | Struct of aggregate
  | Union of aggregate
and aggregate = {
  id : string;
  tag : string option;
  mutable body : body option;
}
and field = { field_name : string; field_type : t; field_offset : int; }
and body = { fields : field list; size : int; alignment : int; }
val pp :
  Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit
val show : t -> Ppx_deriving_runtime.string
val pp_aggregate :
  Ppx_deriving_runtime.Format.formatter ->
  aggregate -> Ppx_deriving_runtime.unit
val show_aggregate : aggregate -> Ppx_deriving_runtime.string
val pp_field :
  Ppx_deriving_runtime.Format.formatter -> field -> Ppx_deriving_runtime.unit
val show_field : field -> Ppx_deriving_runtime.string
val pp_body :
  Ppx_deriving_runtime.Format.formatter -> body -> Ppx_deriving_runtime.unit
val show_body : body -> Ppx_deriving_runtime.string
val show_type : t -> Ppx_deriving_runtime.string
exception Incomplete
val get_size : t -> int
val get_alignment : t -> int
val is_complete_type : t -> bool
val is_simple : t -> bool
val get_field : t -> string -> field
val is_function : t -> bool
