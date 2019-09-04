type t = {
  ty : Type.t;
  label : string;
  init : Ast.init option
}
val pp :
  Ppx_deriving_runtime.Format.formatter -> t -> Ppx_deriving_runtime.unit
val show : t -> Ppx_deriving_runtime.string
val register : t -> unit
val iter : (t -> unit) -> unit
