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
| GlobalVarDecl { gv_ds = { ds_storage_class_spec = Some { exp = Extern; _ }; _ }; _ } ->
    ()
| GlobalVarDecl { gv_decl_inits = decl_inits; _ } ->
    decl_inits |> List.iter (fun di -> match di with
        | { di_entry = Some (GlobalVar (ty, label)); di_init = None; _ } ->
            if Type.is_function ty then ()
            else (
                printf "    .globl %s\n" label;
                printf "    .bss\n";
                printf "    .align %d\n" (Type.get_alignment ty);
                printf "    .type %s, @object\n" label;
                printf "    .size %s, %d\n" label (Type.get_size ty);
                printf "%s:\n" label;
                printf "    .zero %d\n" (Type.get_size ty);
                printf "\n"
            )
        | { di_entry = Some (GlobalVar (ty, label)); di_init = Some init; _ } ->
                Init_global.gen ty label init;
            printf "\n"
        | _ -> failwith "?"
    )

| FunctionDecl { func_name=Some func; func_params=Some params; func_body=body; func_frame_size=Some size; _ } ->

    printf "    .text\n";
    printf "    .globl %s\n" func;
    printf "    .type %s, @function\n" func;
    printf "%s:\n" func;

    (* call命令の実行時にrspを16バイト境界にするので、呼び出されたときは戻り番地が積まれた分ズレている *)
    Stack.set (-8);

    Stack.push "rbp";
    printf "    mov rbp, rsp\n";

    (* ローカル変数領域を確保, rspが8バイト境界になるように切り上げる *)
    Stack.sub (Misc.round_up size 8);

    (* この時点ではスタックはこうなっているはず *)
    (* rsp = rbp - ローカル変数領域のサイズ: ここから先にローカル変数が割り付けられている *)
    (* ...ローカル変数領域... *)
    (* rbp + 0 : 以前のrbp *)
    (* rbp + 8 : call命令が積んだ戻り番地 *)
    (* rbp + 16 : もしあれば7番目の引数 *)
    (* rbp + 24 : もしあれば8番目の引数 *)
    (* ... *)

    (* 文の中で式を実行した場合は必ず生成された値をpopする決まりとするので *)
    (* この時点でのスタック位置が式のコード生成開始時のスタック位置になる *)

    let copy_param i param =
        match param with
        | {
            param_ty = ty;
            param_name = name;
            param_entry = Some (LocalVar (_, lvar_offset));
            _
        } ->
            printf "    mov rax, rbp\n";
            printf "    sub rax, %d\n" lvar_offset;
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
                let param_offset = (i - 6) * 8 + 16 in
                printf "    mov r10, rbp\n";
                printf "    add r10, %d # %s\n" param_offset name;
                printf "    mov r10, [r10]\n";
            store ty "[rax]" "r10"
            )
        | _ -> failwith "?"
    in
        List.iteri copy_param params;

    gen_stmt body;

    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n";
    printf "\n"
| TypedefDecl _
| DummyDecl ->
    ()
| _ -> failwith("gen_decl: "^(Ast.show_decl decl))

and gen_stmt stmt =
try Stack.check_no_change @@ fun _ -> gen_stmt' stmt with
| Stack.Stack_changed ->
    raise(Error_at("stack changed: " ^ (Ast.show_stmt stmt), stmt.loc))

and gen_stmt' stmt =
let gen_expr expr = gen_i_expr @@ Option.get expr.i_expr in
match stmt.exp with
| Empty
| TmpVar _
| TypedefStmt _ ->
    ()
| Var { var_decl_inits = decl_inits; _ } ->
    decl_inits |> List.iter (fun decl_init ->
        decl_init.di_init_assign |> List.iter (fun assign ->
            gen_i_expr assign;
            Stack.pop "rax"
        )
    )
| Expr expr ->
    (* fprintf stderr "# expr start %s\n" (Ast.show_expr_short expr.expr); *)
    printf "# expr start %s\n" (Ast.show_expr_short expr.expr);
    gen_expr expr;
    Stack.pop "rax";
    (* fprintf stderr "# expr end\n"; *)
    printf "# expr end\n"
| Return expr ->
    printf "# return start%s\n" (Ast.show_expr_short expr.expr);
    gen_expr expr;
    Stack.pop "rax";
    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n";
    printf "# return end\n"
| If (expr, then_stmt, None) ->
    let end_label = Unique_id.new_id ".Lend" in
    gen_expr expr;
    Stack.pop "rax";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" end_label;
    gen_stmt then_stmt;
    printf "%s:\n" end_label
| If (expr, then_stmt, Some(else_stmt)) ->
    let else_label = Unique_id.new_id ".Lelse" in
    let end_label = Unique_id.new_id ".Lend" in
    gen_expr expr;
    Stack.pop "rax";
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
    Stack.pop "rax";
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
        Stack.pop "rax";
    in
    let gen_cond expr =
        gen_expr expr;
        Stack.pop "rax";
        printf "    cmp rax, 0\n";
        printf "    je %s\n" end_label
    in
    Option.may gen_expr' init;
    printf "%s:\n" begin_label;
    Option.may gen_cond cond;
    gen_stmt stmt;
    Option.may gen_expr' next;
    printf "    jmp %s\n" begin_label;
    printf "%s:\n" end_label
| Block stmt_list ->
    List.iter gen_stmt stmt_list

and gen_i_expr i_expr =
    try gen_i_expr' i_expr with
    e ->
        Printf.fprintf stderr "During gen_i_expr: %s\n" (Ast.show_i_expr i_expr);
        raise e

and gen_i_expr' i_expr = match i_expr with
| Const n ->
    Stack.push @@ string_of_int n
| Label label ->
    printf "    mov rax, OFFSET FLAT:%s\n" label;
    Stack.push "rax"
| LVar offset ->
    printf "    mov rax, rbp\n";
    printf "    sub rax, %d\n" offset;
    Stack.push "rax"
| Load (ty, e) ->
    gen_i_expr e;
    Stack.pop "rax";
    Gen_misc.load ty "rax" "[rax]";
    Stack.push "rax"
(* 単純な関数呼び出しは直接callする *)
| ICall (Label label, i_expr_list) ->
    let n = List.length i_expr_list in
    let n_stack = if n > 6 then n - 6 else 0 in
    let stack_param_size = n_stack * 8 in
    Stack.with_adjust stack_param_size (fun _ ->
        List.iter gen_i_expr (List.rev i_expr_list);
        (if n >= 1 then Stack.pop "rdi");
        (if n >= 2 then Stack.pop "rsi");
        (if n >= 3 then Stack.pop "rdx");
        (if n >= 4 then Stack.pop "rcx");
        (if n >= 5 then Stack.pop "r8");
        (if n >= 6 then Stack.pop "r9");
        printf "    mov al, 0\n";
        printf "    call %s\n" label;
        if stack_param_size > 0 then Stack.add stack_param_size
    );
    Stack.push "rax"
| ICall (func, i_expr_list) ->
    let n = List.length i_expr_list in
    let n_stack = if n > 6 then n - 6 else 0 in
    let stack_param_size = n_stack * 8 in
    Stack.with_adjust stack_param_size (fun _ ->
        List.iter gen_i_expr (List.rev i_expr_list);
        gen_i_expr func;
        Stack.pop "r10";
        (if n >= 1 then Stack.pop "rdi");
        (if n >= 2 then Stack.pop "rsi");
        (if n >= 3 then Stack.pop "rdx");
        (if n >= 4 then Stack.pop "rcx");
        (if n >= 5 then Stack.pop "r8");
        (if n >= 6 then Stack.pop "r9");
        printf "    mov al, 0\n";
        printf "    call r10\n";
        if stack_param_size > 0 then Stack.add stack_param_size
    );
    Stack.push "rax"
| I_block block ->
    gen_stmt block;
    Stack.push "rax"
| I_binop (LAnd, l, r) ->
    let label = Unique_id.new_id ".Land" in
    gen_i_expr l;
    printf "    mov rax, [rsp]\n";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" label;
    Stack.pop "rax";
    gen_i_expr r;
    printf "%s:\n" label
| I_binop (LOr, l, r) ->
    let label = Unique_id.new_id ".Lor" in
    gen_i_expr l;
    printf "    mov rax, [rsp]\n";
    printf "    cmp rax, 0\n";
    printf "    jne %s\n" label;
    Stack.pop "rax";
    gen_i_expr r;
    printf "%s:\n" label
| I_binop (op, l, r) ->
    gen_i_expr l;
    gen_i_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    gen_op op;
    Stack.push "rax"

and gen_op op = match op with
    | Add ->
        printf "    add rax, rdi\n";
    | Sub ->
        printf "    sub rax, rdi\n"
    | Mul ->
        printf "    imul rax, rdi\n"
    | Div ->
        printf "    cqo\n";
        printf "    idiv rdi\n"
    | Mod ->
        printf "    cqo\n";
        printf "    idiv rdi\n";
        printf "    mov rax, rdx\n"
    | Eq ->
        printf "    cmp rax, rdi\n";
        printf "    sete al\n";
        printf "    movzb rax, al\n"
    | Ne ->
        printf "    cmp rax, rdi\n";
        printf "    setne al\n";
        printf "    movzb rax, al\n"
    | Lt ->
        printf "    cmp rax, rdi\n";
        printf "    setl al\n";
        printf "    movzb rax, al\n"
    | Le ->
        printf "    cmp rax, rdi\n";
        printf "    setle al\n";
        printf "    movzb rax, al\n"
    | BitAnd ->
        printf "    and rax, rdi\n"
    | BitXor ->
        printf "    xor rax, rdi\n"
    | BitOr ->
        printf "    or rax, rdi\n"
    | Store ty ->
        Gen_misc.store ty "[rax]" "rdi";
        printf "    mov rax, rdi\n"
    (* 以下の演算子は別扱いなのでここには来ない *)
    | LAnd
    | LOr ->
        failwith "op?"
