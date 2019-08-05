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

(* コード生成 本体 *)
let rec gen decl_list =
    printf ".intel_syntax noprefix\n";

    String_literal.gen();

    List.iter gen_decl decl_list

and gen_decl decl = match decl.exp with
| GlobalVarDecl (ty, d, None) ->
    let ty, name = Type_check.type_and_var ty d in
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

    | GlobalVarDecl (ty, decl, Some init) ->
    let ty, name = Type_check.type_and_var ty decl in
    let ty = match ty , init.exp with
        | Type.Array (t, None), ListInitializer l ->
            Type.Array (t, Some(List.length l))
        | Type.Array (Type.Char, None), ExprInitializer { exp = { e = Str (s, _) } }->
            Type.Array (Type.Char, Some(String.length s + 1))
        | _ -> ty in
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
        Gen_expr.gen_lval_name name;
        Stack.pop "rax";
        Gen_misc.(match i with
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
        )
    in
        Array.iteri copy_param (Array.of_list params);

    gen_stmt body;

    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n"

and gen_stmt stmt =
let gen_expr expr =
    Stack.with_save (fun _ -> Gen_expr.gen_expr expr)
in
match stmt.exp with
| Empty -> ()
| Var (ty, d, None) -> ()
| Var (ty, d, Some init) ->
    Init_local.gen ty d init
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
