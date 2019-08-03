open Ast
open Env
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
| Type.Char -> 8

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
| (8, "rax") -> "al"
| (8, "rdi") -> "dil"
| (8, "rsi") -> "sil"
| (8, "rdx") -> "dl"
| (8, "rcx") -> "cl"
| (8, "rbx") -> "bl"
| (8, "r8")  -> "r8b"
| (8, "r9")  -> "r9b"
| (8, "r10") -> "r10b"

let load ty dst src = match ty with
| Type.Char ->
    printf "    movsx %s, BYTE PTR [%s]\n" dst src
| _ ->
    printf "    mov %s, %s\n" (select_reg ty dst) src

let store ty dst src =
    printf "    mov %s, %s\n" dst (select_reg ty src)

(* コード生成 本体 *)
let rec gen decl_list =
    printf ".intel_syntax noprefix\n";

    String_literal.gen();

    List.iter gen_decl decl_list

and gen_decl decl = match decl.exp with
| GlobalVarDecl (ty, name, None) ->
    begin
        try register_global_var ty name with
        | DuplicatedGlobal _ -> raise(Error_at("duplicated global: "^name, decl.loc))
    end;

    printf "    .globl %s\n" name;
    printf "    .bss\n";
    printf "    .align %d\n" (Type.get_alignment ty);
    printf "    .type %s, @object\n" name;
    printf "    .size %s, %d\n" name (Type.get_size ty);
    printf "%s:\n" name;
    printf "    .zero %d\n" (Type.get_size ty)

| GlobalVarDecl (ty, name, Some(init)) ->
    begin
        try register_global_var ty name with
        | DuplicatedGlobal _ -> raise(Error_at("duplicated global: "^name, decl.loc))
    end;

    Type_check.prepare_init init;
    Init.init_global ty name init

| FunctionDecl (_, func, params, body) ->
    Type_check.prepare_func params body;
    Stack.reset();

    let size = local_var_size() in

    printf "    .text\n";
    printf "    .globl %s\n" func;
    printf "    .type %s, @function\n" func;
    printf "%s:\n" func;

    printf "    push rbp\n";
    printf "    mov rbp, rsp\n";
    printf "    sub rsp, %d\n" size;
    Stack.add size;

    let copy_param i (ty, name) =
        gen_lval_name name;
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

and gen_lval_entry entry = match entry with
| LocalVar (_, offset) ->
    printf "    mov rax, rbp\n";
    printf "    sub rax, %d\n" offset;
    Stack.push "rax"
| GlobalVar (_, name) ->
    printf "    mov rax, OFFSET FLAT:%s\n" name;
    Stack.push "rax"

and gen_lval_name name = gen_lval_entry (get_entry name)

and gen_lval expr = match expr.exp.e with
| Ident (_, entry_ref) ->
    gen_lval_entry !entry_ref
| Deref e ->
    gen_expr e
| _ -> raise (Error_at("not lval: " ^ show_expr expr, expr.loc))

and binop op l r = 
    gen_expr l;
    gen_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    op ();
    Stack.push "rax"


and get_type expr = Option.get(expr.exp.ty)


and gen_expr expr =
let _ = Type_check.assign_type expr in
match expr.exp.e with
| Num n ->
    Stack.push n
| Str label ->
    printf "    mov rax, OFFSET FLAT:%s\n" label;
    Stack.push "rax"
| Ident (_, entry_ref) ->
    let ty = entry_type !entry_ref in
    begin
        match ty with
        | Array (_, _) -> 
            gen_lval expr
        | _ ->
            gen_lval expr;
            Stack.pop "rax";
            load ty "rax" "[rax]";
            Stack.push "rax"
    end
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
        printf "    mov al, 0\n";
        printf "    call %s\n" func
    );
    Stack.push "rax"
| Addr e ->
    gen_lval e
| Deref e ->
    let ty = get_type expr in
    gen_expr e;
    Stack.pop "rax";
    load ty "rax" "[rax]";
    Stack.push "rax"
| Sizeof e ->
    printf "    mov rax, %d\n" (Type.get_size (get_type e));
    Stack.push "rax"
| Add (l, r) ->
    let add l r =
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
    in
    begin
        match get_type r with
        | Type.Ptr _ -> add r l
        | _          -> add l r
    end
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
