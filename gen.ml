open Ast
open Misc
open Printf

(* ラベル用のシーケンス *)
let seq = ref 0

let new_seq _ =
    let r = !seq in
    seq := !seq + 1;
    r


let rec gen decl_list =
    printf ".intel_syntax noprefix\n";
    printf ".global main\n";

    List.iter gen_decl decl_list

and gen_decl decl = match decl with
| Function (func, params, body) ->
    Env.prepare params body;
    Stack.reset();

    printf "%s:\n" func;

    printf "    push rbp\n";
    printf "    mov rbp, rsp\n";
    printf "    sub rsp, %d\n" (8 * Env.size());
    Stack.add (8 * Env.size());

    let copy_param i name =
        gen_lval_lvar name;
        Stack.pop "rax";
        match i with
        | 0 -> printf "    mov [rax], rdi\n"
        | 1 -> printf "    mov [rax], rsi\n"
        | 2 -> printf "    mov [rax], rdx\n"
        | 3 -> printf "    mov [rax], rcx\n"
        | 4 -> printf "    mov [rax], r8\n"
        | 5 -> printf "    mov [rax], r9\n"
        | _ ->
            let offset = (i - 6) * 8 + 16 in
            printf "    mov r10, rbp\n";
            printf "    add r10, %d # %s\n" offset name;
            printf "    mov r10, [r10]\n";
            printf "    mov [rax], r10\n"
    in
        Array.iteri copy_param (Array.of_list params);

    gen_stmt body;

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

and gen_lval_lvar name = 
    printf "    mov rax, rbp\n";
    printf "    sub rax, %d\n" (Env.lvar_offset name);
    Stack.push "rax"

and gen_lval expr = match expr with
| Ident name -> gen_lval_lvar name
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
| Call (func, expr_list) ->
    let n_param = List.length expr_list in
    let n_stack_param = if n_param > 6 then n_param - 6 else 0 in
    Stack.with_adjust (n_stack_param * 8) (fun _ ->
        List.iter gen_expr (List.rev expr_list);
        (if n_param >= 1 then Stack.pop "rdi");
        (if n_param >= 2 then Stack.pop "rsi");
        (if n_param >= 3 then Stack.pop "rdx");
        (if n_param >= 4 then Stack.pop "rcx");
        (if n_param >= 5 then Stack.pop "r8");
        (if n_param >= 6 then Stack.pop "r9");
        printf "    mov rax, %d\n" n_param;
        printf "    call %s\n" func;
    );
    Stack.push "rax"
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
