open Ast
open Printf

let rec gen expr =
    printf ".intel_syntax noprefix\n";
    printf ".global main\n";
    printf "main:\n";
    gen_expr expr;
    printf "    pop rax\n";
    printf "    ret\n"

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
