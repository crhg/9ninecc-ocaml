open Ast
open Printf

module Env = Map.Make(String)

let local_env = ref Env.empty

let lvar_offset name =
    try Env.find name !local_env
    with
    | Not_found ->
        let new_offset = (Env.cardinal !local_env + 1) * 8 in
        local_env := Env.add name new_offset !local_env;
        new_offset

let rec gen stmt_list =
    local_env := Env.empty;

    printf ".intel_syntax noprefix\n";
    printf ".global main\n";
    printf "main:\n";

    (* 変数26個分の領域を確保 *)
    printf "    push rbp\n";
    printf "    mov rbp, rsp\n";
    printf "    sub rsp, %d\n" (8 * 26);

    List.iter gen_stmt stmt_list;

    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n"

and gen_stmt stmt = match stmt with
| Expr expr ->
    gen_expr expr;
    printf "    pop rax\n";

and gen_lval expr = match expr with
| Ident name ->
    printf "    mov rax, rbp\n";
    printf "    sub rax, %d\n" (lvar_offset name);
    printf "    push rax\n"
| _ -> failwith("not lval: " ^ show_expr expr)

and binop op l r = 
    gen_expr l;
    gen_expr r;
    printf "    pop rdi\n";
    printf "    pop rax\n";
    op ();
    printf "    push rax\n"

and gen_expr expr = match expr with
| Num n ->
    printf "    push %d\n" (int_of_string n)
| Ident name ->
    gen_lval expr;
    printf "    pop rax\n";
    printf "    mov rax, [rax]\n";
    printf "    push rax\n"
| Assign (l, r) ->
    gen_lval l;
    gen_expr r;
    printf "    pop rdi\n";
    printf "    pop rax\n";
    printf "    mov [rax], rdi\n";
    printf "    push rdi\n";
| Add (l, r) ->
    let op _ =
        printf "    add rax, rdi\n"
    in
    binop op l r
| Sub (l, r) ->
    let op _ =
        printf "    sub rax, rdi\n"
    in
    binop op l r
| Mul (l, r) ->
    let op _ =
        printf "    imul rax, rdi\n"
    in
    binop op l r
| Div (l, r) ->
    let op _ =
        printf "    cqo\n";
        printf "    idiv rdi\n"
    in
    binop op l r
| Eq (l, r) ->
    let op _ =
        printf "    cmp rax, rdi\n";
        printf "    sete al\n";
        printf "    movzb rax, al\n"
    in
    binop op l r
| Ne (l, r) ->
    let op _ =
        printf "    cmp rax, rdi\n";
        printf "    setne al\n";
        printf "    movzb rax, al\n"
    in
    binop op l r
| Lt (l, r) ->
    let op _ =
        printf "    cmp rax, rdi\n";
        printf "    setl al\n";
        printf "    movzb rax, al\n"
    in
    binop op l r
| Le (l, r) ->
    let op _ =
        printf "    cmp rax, rdi\n";
        printf "    setle al\n";
        printf "    movzb rax, al\n"
    in
    binop op l r
