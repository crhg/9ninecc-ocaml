open Ast
open Misc
open Printf
open Type

let rec eval_int expr =
    match expr.exp.e with
    | Num n -> int_of_string n
    | Add (lhs, rhs) -> eval_int lhs + eval_int rhs
    | Sub (lhs, rhs) -> eval_int lhs - eval_int rhs
    | _ -> raise(Error_at("not int or not implemented", expr.loc))

and eval_pointer expr = match expr.exp.e with
| Str label -> (label, 0)
| Ident (_, entry_ref) when is_global_array_entry !entry_ref ->
    (get_label_from_entry !entry_ref, 0)
| Add (lhs, rhs) ->
    add_pointer lhs rhs
| Sub (lhs, rhs) ->
    sub_pointer lhs rhs
| Addr e ->
    eval_lval e
| _ -> raise(Error_at("not pointer expression or not implemented: " ^ (show_expr expr), expr.loc))

and add_pointer lhs rhs = match lhs.exp.ty, rhs.exp.ty with
| Some Type.Ptr ty, Some Type.Int ->
    let (label, x) = eval_pointer lhs in
    let y = eval_int rhs in
    (label, x + y * Type.get_size(ty))
| Some Type.Int, Some Type.Ptr _ -> add_pointer rhs lhs
| _ -> failwith "cannot add"

and sub_pointer lhs rhs = match lhs.exp.ty, rhs.exp.ty with
| Some Type.Ptr ty, Some Type.Int ->
    let (label, x) = eval_pointer lhs in
    let y = eval_int rhs in
    (label, x - y * Type.get_size(ty))
| _ -> failwith "cannot sub"

and is_global_array_entry entry = match entry with
| Env.GlobalVar (Array _, _) -> true
| _ -> false

and get_label_from_entry entry = match entry with
| Env.GlobalVar (_, label) -> label
| _ -> failwith ("not GlobalVar " ^ Env.show_entry entry)

and eval_lval expr = match expr.exp.e with
| Str label -> (label, 0)
| Ident (_, entry_ref) -> (get_label_from_entry !entry_ref, 0)
| Deref e -> eval_pointer e
