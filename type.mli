module SS :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
val shown : SS.t ref
type t =
    Int
  | Char
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
val get_field : t -> string -> field
