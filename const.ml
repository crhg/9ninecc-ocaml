open Ast
open Printf
open Type

let rec get_value expr =
    match eval expr with
    | (None, n) -> sprintf "%d" n
    | (Some label, 0) -> label
    | (Some label, n) -> sprintf "%s + %d\n" label n

and is_global_array_entry entry = match entry with
| Env.GlobalVar (Array _, _) -> true
| _ -> false

and get_label_from_entry entry = match entry with
| Env.GlobalVar (_, label) -> label
| _ -> failwith ("not GlobalVar " ^ Env.show_entry entry)

and eval expr = match expr.exp.e with
| Num n -> (None, int_of_string n)
| Str label -> (Some label, 0)
| Ident (_, entry_ref) when is_global_array_entry !entry_ref ->
    (Some (get_label_from_entry !entry_ref), 0)
