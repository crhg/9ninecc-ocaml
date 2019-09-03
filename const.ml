exception Not_Constant of string

let rec eval_int i_expr = match eval i_expr with
| None, n -> n
| _ -> raise(Not_Constant("not int " ^(Ast.show_i_expr i_expr)))

and eval i_expr =
    let open Ast in
    match i_expr with
    | Const n -> (None, n)
    | Label label -> (Some label, 0)
    | ICond (c, t, e) ->
        if eval c <> (None, 0) then eval t else eval e
    | IBitComplement e ->
        (match eval e with
        | None, e -> (None, lnot e)
        | _ -> raise(Not_Constant(Ast.show_i_expr i_expr))
        )
    | I_binop(LAnd, l, r) ->
        (None, if eval l <> (None, 0) && eval r <> (None, 0) then 1 else 0)
    | I_binop(LOr, l, r) ->
        (None, if eval l <> (None, 0) || eval r <> (None, 0) then 1 else 0)
    | I_binop(op, l, r) ->
        let l = eval l in
        let r = eval r in
        (match op, l, r with
        | Add, (label, x), (None, y) -> (label, x + y)
        | Add, (None, x), (label, y) -> (label, x + y)
        | Sub, (label, x), (None, y) -> (label, x - y)
        | Mul, (None, x), (None, y) -> (None, x * y)
        | Div, (None, x), (None, y) -> (None, x / y)
        | Mod, (None, x), (None, y) -> (None, x mod y)
        | Lt, (None, x), (None, y) -> (None, if x < y then 1 else 0)
        | Le, (None, x), (None, y) -> (None, if x <= y then 1 else 0)
        | Eq, (None, x), (None, y) -> (None, if x = y then 1 else 0)
        | Ne, (None, x), (None, y) -> (None, if x <> y then 1 else 0)
        | BitAnd, (None, x), (None, y) -> (None, x land y)
        | BitOr, (None, x), (None, y) -> (None, x lor y)
        | BitXor, (None, x), (None, y) -> (None, x lxor y)
        | LShift, (None, x), (None, y) -> (None, x lsl y)
        | RShift, (None, x), (None, y) -> (None, x asr y)
        | _ -> raise(Not_Constant(Ast.show_i_expr i_expr))
        )
    | _ -> failwith "?"

