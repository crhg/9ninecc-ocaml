type t = {
    ty: Type.t;
    label: string;
    init: Ast.init option;
}
[@@deriving show]

let global_vars =
    Queue.create()

let register global_var =
    Queue.push global_var global_vars

let iter f =
    Queue.iter f global_vars
