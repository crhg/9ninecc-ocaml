(* ローカル変数の環境 *)
open Ast
open Misc

module Env = Map.Make(String)

type entry = {
    ty: Type.t;
    offset: int
}

let local_env = ref Env.empty

let offset = ref 0

exception Duplicated

let allocate ty_name =
    let (ty, name) = ty_name in
    if Env.mem name !local_env then
        raise Duplicated
    else
        let size = Type.get_size ty in
        let alignment = Type.get_alignment ty in
        offset := round_up !offset alignment + size;
        local_env := Env.add name { ty = ty; offset = !offset } !local_env

let size _ = !offset

let rec prepare params stmt =
    local_env := Env.empty;
    offset := 0;
    List.iter allocate params;
    allocate_stmt stmt

and allocate_stmt stmt = match stmt.exp with
| Var (ty, name) ->
    begin
        try allocate (ty, name) with
        | Duplicated ->
            raise (Error_at("duplicated: " ^ name, stmt.loc))
    end
| If (expr, then_stmt, else_stmt_opt) ->
    allocate_stmt then_stmt;
    may allocate_stmt else_stmt_opt
| While (expr, stmt) ->
    allocate_stmt stmt
| For (init, cond, next, stmt) ->
    allocate_stmt stmt;
| Block stmt_list ->
    List.iter allocate_stmt stmt_list
| _ -> ()

let lvar_offset name = (Env.find name !local_env).offset
let lvar_type name = (Env.find name !local_env).ty
