open Ast
open Misc
open Printf

(* ラベル用のシーケンス *)
let seq = ref 0

let new_seq _ =
    let r = !seq in
    seq := !seq + 1;
    r

(* typeのサイズを考慮したloadとstore *)
(* レジスタ名は64bitのもので指定 *)
(* 指定したタイプに応じて変化する *)
exception Invalid_size

let select_size ty = match ty with
| Type.Ptr _ -> 64
| Type.Int -> 32

let select_reg ty reg = match (select_size ty, reg) with
| (64, _) -> reg
| (32, "rax") -> "eax"
| (32, "rdi") -> "edi"
| (32, "rsi") -> "esi"
| (32, "rdx") -> "edx"
| (32, "rcx") -> "ecx"
| (32, "rbx") -> "ebx"
| (32, "r8")  -> "r8d"
| (32, "r9")  -> "r9d"
| (32, "r10") -> "r10d"

let load ty dst src =
    printf "    mov %s, %s\n" (select_reg ty dst) src

let store ty dst src =
    printf "    mov %s, %s\n" dst (select_reg ty src)

(* コード生成 本体 *)
let rec gen decl_list =
    printf ".intel_syntax noprefix\n";
    printf ".global main\n";

    List.iter gen_decl decl_list

and gen_decl decl = match decl.exp with
| Function (func, params, body) ->
    Env.prepare params body;
    Stack.reset();

    printf "%s:\n" func;

    printf "    push rbp\n";
    printf "    mov rbp, rsp\n";
    printf "    sub rsp, %d\n" (Env.size());
    Stack.add (Env.size());

    let copy_param i ty_name =
        let (ty, name) = ty_name in
        gen_lval_lvar name;
        Stack.pop "rax";
        match i with
        | 0 ->
            store ty "[rax]"  "rdi"
        | 1 ->
            store ty "[rax]"  "rsi"
        | 2 ->
            store ty "[rax]"  "rdx"
        | 3 ->
            store ty "[rax]"  "rcx"
        | 4 ->
            store ty "[rax]"  "r8"
        | 5 ->
            store ty "[rax]"  "r9"
        | _ ->
            let offset = (i - 6) * 8 + 16 in
            printf "    mov r10, rbp\n";
            printf "    add r10, %d # %s\n" offset name;
            printf "    mov r10, [r10]\n";
            store ty "[rax]" "r10"
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
match stmt.exp with
| Var _ -> ()
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

and gen_lval expr = match expr.exp.e with
| Ident name ->
    begin
        try gen_lval_lvar name
        with Not_found ->
            raise (Error_at("undefined: " ^ name, expr.loc))
    end
| _ -> raise (Error_at("not lval: " ^ show_expr expr, expr.loc))

and binop op l r = 
    gen_expr l;
    gen_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    op ();
    Stack.push "rax"

and assign_type expr = 
    let ty = find_type expr in
    expr.exp.ty <- Some ty;
    ty

and get_type expr = Option.get(expr.exp.ty)

and find_type expr = match expr.exp.e with
| Num _ -> Type.Int
| Ident name -> Env.lvar_type name
| Assign (l, r) ->
    let lty = assign_type l in
    let _ = assign_type r in
    lty
| Call (_, expr_list) ->
    let _ = List.map assign_type expr_list in
    Type.Int
| Addr e ->
    let ty = assign_type e in
    Type.Ptr ty
| Deref e ->
    let ty = assign_type e in
    begin
        match ty with
        | Type.Ptr t -> t
        | _ -> raise (Error_at("deref of non pointer", expr.loc))
    end
| Add (l, r) ->
    let lty = assign_type l in
    let _ = assign_type r in
    lty
| Sub (l, r) ->
    let lty = assign_type l in
    let rty = assign_type r in
    begin
        match (lty, rty) with
        | (Type.Ptr _, Type.Ptr _) -> Type.Int
        | (_, _) -> lty
    end
| Mul (l, r) ->
    let lty = assign_type l in
    let _ = assign_type r in
    lty
| Div (l, r) ->
    let lty = assign_type l in
    let _ = assign_type r in
    lty
| Eq (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int
| Ne (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int
| Lt (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int
| Le (l, r) ->
    let _ = assign_type l in
    let _ = assign_type r in
    Type.Int

and gen_expr expr =
let _ = assign_type expr in
match expr.exp.e with
| Num n ->
    Stack.push n
| Ident name ->
    let ty = get_type expr in
    gen_lval expr;
    Stack.pop "rax";
    load ty "rax" "[rax]";
    Stack.push "rax";
| Assign (l, r) ->
    let ty = get_type expr in
    gen_lval l;
    gen_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    store ty "[rax]" "rdi";
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
| Addr e ->
    gen_lval e
| Deref e ->
    let ty = get_type expr in
    gen_expr e;
    Stack.pop "rax";
    load ty "rax" "[rax]";
    Stack.push "rax";
| Add (l, r) ->
    let op _ =
        let lty = get_type l in
        let rty = get_type r in
        match (lty, rty) with
        | (Type.Ptr ty, Type.Int) ->
            let size = Type.get_size ty in
            printf "    mov rbx, %d\n" size;
            printf "    imul rdi, rbx\n";
            printf "    add rax, rdi\n"
        | (Type.Int, Type.Int) ->
            printf "    add rax, rdi\n"
    in
    binop op l r
| Sub (l, r) ->
    let op _ =
        let lty = get_type l in
        let rty = get_type r in
        match (lty, rty) with
        | (Type.Ptr lty', Type.Ptr rty') ->
            if lty' == rty' then
                let size = Type.get_size lty' in
                printf "    sub rax, rdi\n";
                printf "    mov rdi, %d\n" size;
                printf "    cqo\n";
                printf "    idiv rdi\n"
            else
                raise(Error_at(
                    "different pointer type: lhs=" ^ (Type.show lty) ^ ", rhs=" ^ (Type.show rty),
                    expr.loc
                ))
        | (Type.Ptr ty, Type.Int) ->
            let size = Type.get_size ty in
            printf "    mov rbx, %d\n" size;
            printf "    imul rdi, rbx\n";
            printf "    sub rax, rdi\n"
        | (Type.Int, Type.Int) ->
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
