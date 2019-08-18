open Ast
open Misc

exception Not_Constant of string

let rec eval_int i_expr = match eval i_expr with
| None, n -> n
| _ -> raise(Not_Constant("not int " ^(Ast.show_i_expr i_expr)))

and eval i_expr = match i_expr with
| Const n -> (None, n)
| Label label -> (Some label, 0)
| I_binop(op, l, r) ->
    let l = eval l in
    let r = eval r in
    (match op, l, r with
    | Add, (label, x), (None, y) -> (label, x + y)
    | Add, (None, x), (label, y) -> (label, x + y)
    | Sub, (label, x), (None, y) -> (label, x - y)
    | Mul, (None, x), (None, y) -> (None, x * y)
    | Div, (None, x), (None, y) -> (None, x / y)
    | Lt, (None, x), (None, y) -> (None, if x < y then 1 else 0)
    | Le, (None, x), (None, y) -> (None, if x <= y then 1 else 0)
    | Eq, (None, x), (None, y) -> (None, if x = y then 1 else 0)
    | Ne, (None, x), (None, y) -> (None, if x <> y then 1 else 0)
    | _ -> raise(Not_Constant(Ast.show_i_expr i_expr))
    )

