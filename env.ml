(* ローカル変数の環境 *)
open Ast
open Misc

module Env = Map.Make(String)

let local_env = ref Env.empty

let allocate name =
    if Env.mem name !local_env then ()
    else
        let new_offset = (Env.cardinal !local_env + 1) * 8 in
        local_env := Env.add name new_offset !local_env

let size _ = Env.cardinal !local_env

let rec prepare params stmt =
    List.iter allocate params;
    allocate_stmt stmt

and allocate_stmt stmt = match stmt with
| Expr expr ->
    allocate_expr expr
| Return expr ->
    allocate_expr expr
| If (expr, then_stmt, else_stmt_opt) ->
    allocate_expr expr;
    allocate_stmt then_stmt;
    may allocate_stmt else_stmt_opt
| While (expr, stmt) ->
    allocate_expr expr;
    allocate_stmt stmt
| For (init, cond, next, stmt) ->
    may allocate_expr init;
    may allocate_expr cond;
    may allocate_expr next;
    allocate_stmt stmt;
| Block stmt_list ->
    List.iter allocate_stmt stmt_list

and allocate_expr expr = match expr with
| Num _ -> ()
| Ident name -> allocate name
| Add (l, r) -> allocate_expr l; allocate_expr r
| Sub (l, r) -> allocate_expr l; allocate_expr r
| Mul (l, r) -> allocate_expr l; allocate_expr r
| Div (l, r) -> allocate_expr l; allocate_expr r
| Lt (l, r) -> allocate_expr l; allocate_expr r
| Le (l, r) -> allocate_expr l; allocate_expr r
| Eq (l, r) -> allocate_expr l; allocate_expr r
| Ne (l, r) -> allocate_expr l; allocate_expr r
| Assign (l, r) -> allocate_expr l; allocate_expr r
| Call (_, params) -> List.iter allocate_expr params
| Addr e -> allocate_expr e
| Deref e -> allocate_expr e

let lvar_offset name = Env.find name !local_env
