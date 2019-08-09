type t =
    Int
  | Char
  | Ptr of t
  | Array of t * int option
  | Function of t * (string * t) list
val pp :
  Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit
val show : t -> Ppx_deriving_runtime.string
val get_size : t -> int
val get_alignment : t -> int
val is_complete_type : t -> bool
