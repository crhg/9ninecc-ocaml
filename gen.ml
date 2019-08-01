open Ast
open Misc
open Printf

(* ラベル用のシーケンス *)
let seq = ref 0

let new_seq _ =
    let r = !seq in
    seq := !seq + 1;
    r

(* ローカル変数の環境 *)
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
    Stack.reset();

    printf ".intel_syntax noprefix\n";
    printf ".global main\n";
    printf "main:\n";

    (* 変数26個分の領域を確保 *)
    printf "    push rbp\n";
    printf "    mov rbp, rsp\n";
    printf "    sub rsp, %d\n" (8 * 26);
    Stack.add (8 * 26);

    List.iter gen_stmt stmt_list;

    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n"

and gen_stmt stmt =
let gen_expr expr =
    Stack.with_save (fun _ -> gen_expr expr)
in
match stmt with
| Expr expr ->
    gen_expr expr;
    printf "    pop rax\n"
| Return expr ->
    gen_expr expr;
    printf "    pop rax\n";
    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n"
| If (expr, then_stmt, None) ->
    let seq=new_seq() in
    gen_expr expr;
    printf "    pop rax\n";
    printf "    cmp rax, 0\n";
    printf "    je .Lend%d\n" seq;
    gen_stmt then_stmt;
    printf ".Lend%d:\n" seq
| If (expr, then_stmt, Some(else_stmt)) ->
    let seq=new_seq() in
    gen_expr expr;
    printf "    pop rax\n";
    printf "    cmp rax, 0\n";
    printf "    je .Lelse%d\n" seq;
    gen_stmt then_stmt;
    printf "    jmp .Lend%d\n" seq;
    printf ".Lelse%d:\n" seq;
    gen_stmt else_stmt;
    printf ".Lend%d:\n" seq
| While (expr, stmt) ->
    let seq=new_seq() in
    printf ".Lbegin%d:\n" seq;
    gen_expr expr;
    printf "    pop rax\n";
    printf "    cmp rax, 0\n";
    printf "    je .Lend%d\n" seq;
    gen_stmt stmt;
    printf "    jmp .Lbegin%d\n" seq;
    printf ".Lend%d:\n" seq
| For (init, cond, next, stmt) ->
    let seq=new_seq() in
    let gen_expr' expr =
        gen_expr expr;
        printf "    pop rax\n"
    in
    let gen_cond expr =
        gen_expr expr;
        printf "    pop rax\n";
        printf "    cmp rax, 0\n";
        printf "    je .Lend%d\n" seq
    in
    may gen_expr' init;
    printf ".Lbegin%d:\n" seq;
    may gen_cond cond;
    gen_stmt stmt;
    may gen_expr' next;
    printf "    jmp .Lbegin%d\n" seq;
    printf ".Lend%d:\n" seq
| Block stmt_list ->
    List.iter gen_stmt stmt_list

and gen_lval expr = match expr with
| Ident name ->
    printf "    mov rax, rbp\n";
    printf "    sub rax, %d\n" (lvar_offset name);
    Stack.push "rax"
| _ -> failwith("not lval: " ^ show_expr expr)

and binop op l r = 
    gen_expr l;
    gen_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    op ();
    Stack.push "rax"

and gen_expr expr = match expr with
| Num n ->
    Stack.push n
| Ident name ->
    gen_lval expr;
    Stack.pop "rax";
    printf "    mov rax, [rax]\n";
    Stack.push "rax";
| Assign (l, r) ->
    gen_lval l;
    gen_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    printf "    mov [rax], rdi\n";
    Stack.push "rdi"
| Call func ->
    Stack.with_adjust 0 (fun _ ->
        printf "    call %s\n" func;
        Stack.push "rax"
    )
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
