open Ast
open Misc

let rec eval_int expr =
    match expr.exp.e with
    | Num n -> int_of_string n
    | Binop{op=Add; lhs=lhs; rhs=rhs} -> eval_int lhs + eval_int rhs
    | Binop{op=Sub; lhs=lhs; rhs=rhs} -> eval_int lhs - eval_int rhs
    | _ -> raise(Error_at("not int or not implemented", expr.loc))

and eval_pointer expr = match expr.exp.e with
| Str (_, label) -> (label, 0)
| Ident { entry = Some entry } when is_global_array_entry entry ->
    (get_label_from_entry entry, 0)
| Binop { op=PtrAdd size; lhs=lhs; rhs=rhs } ->
    add_pointer lhs size rhs 
| Binop { op=PtrSub size; lhs=lhs; rhs=rhs } ->
    sub_pointer lhs size rhs
| Addr e ->
    eval_lval e
| _ -> raise(Error_at("not pointer expression or not implemented: " ^ (show_expr expr), expr.loc))

and add_pointer lhs size rhs =
    let (label, x) = eval_pointer lhs in
    let y = eval_int rhs in
    (label, x + y * size)

and sub_pointer lhs size rhs =
    let (label, x) = eval_pointer lhs in
    let y = eval_int rhs in
    (label, x - y * size)

and is_global_array_entry entry = match entry with
| Env.GlobalVar (Array _, _) -> true
| _ -> false

and get_label_from_entry entry = match entry with
| Env.GlobalVar (_, label) -> label
| _ -> failwith ("not GlobalVar " ^ Env.show_entry entry)

and eval_lval expr = match expr.exp.e with
| Str (_, label) -> (label, 0)
| Ident { entry = Some entry_ref } -> (get_label_from_entry entry_ref, 0)
| Deref { deref_expr=e } -> eval_pointer e
