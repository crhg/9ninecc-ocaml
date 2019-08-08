open Ast
open Env
open Misc
open Printf

(* コード生成 本体 *)
let rec gen decl_list =
    printf ".intel_syntax noprefix\n";

    String_literal.gen();

    List.iter gen_decl decl_list

and gen_decl decl = match decl.exp with
| GlobalVarDecl (ty, d, None) ->
    let ty, name = Type_check.type_and_var ty d in
    register_global_var ty name;

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
    register_global_var ty name;

    Type_check.prepare_init init;
    Init.init_global ty name init

| FunctionDecl (_, func, params, body) ->
    Env.with_new_scope @@ fun _ ->
    let size = Type_check.prepare_func params body in

    printf "    .text\n";
    printf "    .globl %s\n" func;
    printf "    .type %s, @function\n" func;
    printf "%s:\n" func;

    (* call命令の実行時にrspを16バイト境界にするので、呼び出されたときは戻り番地が積まれた分ズレている *)
    Stack.set(-8);

    Stack.push("rbp");
    printf "    mov rbp, rsp\n";
    Stack.sub(size);

    (* 文の中で式を実行した場合は必ず生成された値をpopする決まりとするので *)
    (* この時点でのスタック位置が式のコード生成開始時のスタック位置になる *)

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
        List.iteri copy_param params;

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
    let end_label = Unique_id.new_id ".Lend" in
    gen_expr expr;
    printf "    pop rax\n";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" end_label;
    gen_stmt then_stmt;
    printf "%s:\n" end_label
| If (expr, then_stmt, Some(else_stmt)) ->
    let else_label = Unique_id.new_id ".Lelse" in
    let end_label = Unique_id.new_id ".Lend" in
    gen_expr expr;
    printf "    pop rax\n";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" else_label;
    gen_stmt then_stmt;
    printf "    jmp %s\n" end_label;
    printf "%s:\n" else_label;
    gen_stmt else_stmt;
    printf "%s:\n" end_label
| While (expr, stmt) ->
    let begin_label = Unique_id.new_id ".Lbegin" in
    let end_label = Unique_id.new_id ".Lend" in
    printf "%s:\n" begin_label;
    gen_expr expr;
    printf "    pop rax\n";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" end_label;
    gen_stmt stmt;
    printf "    jmp %s\n" begin_label;
    printf "%s:\n" end_label
| For (init, cond, next, stmt) ->
    let begin_label = Unique_id.new_id ".Lbegin" in
    let end_label = Unique_id.new_id ".Lend" in
    let gen_expr' expr =
        gen_expr expr;
        printf "    pop rax\n"
    in
    let gen_cond expr =
        gen_expr expr;
        printf "    pop rax\n";
        printf "    cmp rax, 0\n";
        printf "    je %s\n" end_label
    in
    may gen_expr' init;
    printf "%s:\n" begin_label;
    may gen_cond cond;
    gen_stmt stmt;
    may gen_expr' next;
    printf "    jmp %s\n" begin_label;
    printf "%s:\n" end_label
| Block stmt_list ->
    List.iter gen_stmt stmt_list
