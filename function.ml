(* 関数定義に必要な情報を保存する *)

type t = {
    ty: Type.t;
    label: string;
    params: param list;
    frame_size: int;
    has_varargs: bool;
    body: Ast.stmt
}

and param = {
    param_ty: Type.t;
    param_name: string;
    param_offset: int
}
[@@deriving show]

let funcs =
    Queue.create()

let register (func:t) =
    Queue.push func funcs

let iter f =
    Queue.iter f funcs
