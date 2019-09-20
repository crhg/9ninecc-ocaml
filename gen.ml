open Ast
open Extension

let printf = Printf.printf

(* コード生成 本体 *)
let rec gen _ =
    printf ".intel_syntax noprefix\n";

    String_literal.gen();
    Global_var.iter gen_global_var;
    Function.iter gen_function

and gen_global_var Global_var.{ ty = ty; label = label; init = init; _ } =
    match init with
    | None ->
        printf "    .globl %s\n" label;
        printf "    .bss\n";
        printf "    .align %d\n" (Type.get_alignment ty);
        printf "    .type %s, @object\n" label;
        printf "    .size %s, %d\n" label (Type.get_size ty);
        printf "%s:\n" label;
        printf "    .zero %d\n" (Type.get_size ty);
        printf "\n"
    | Some init ->
        Init_global.gen ty label init;
        printf "\n"

and gen_function Function.{ label = label; params = params; frame_size = frame_size; has_varargs = has_varargs; body = body; _ } =
    printf "    .text\n";
    printf "    .globl %s\n" label;
    printf "    .type %s, @function\n" label;
    printf "%s:\n" label;

    (* call命令の実行時にrspを16バイト境界にするので、呼び出されたときは戻り番地が積まれた分ズレている *)
    Stack.set (-8);

    Stack.push "rbp";
    printf "    mov rbp, rsp\n";

    (* ローカル変数領域を確保, rspが8バイト境界になるように切り上げる *)
    Stack.sub (Misc.round_up frame_size 8);

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
        | Function.{
            param_ty = ty;
            param_name = name;
            param_offset = lvar_offset;
            _
        } ->
            printf "# copy param %d\n" i;
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
    in
    List.iteri copy_param params;

    (* 可変引数があるときはレジスタ渡しのパラメタをメモリに保存 *)
    (if has_varargs then (
        let n = List.length params in
        Varargs.gen_register_save n
    ));

    gen_stmt body;

    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n";
    printf "\n"

and gen_stmt stmt =
try Stack.check_no_change @@ fun _ -> gen_stmt' stmt with
| Stack.Stack_changed ->
    raise(Misc.Error_at("stack changed: " ^ (Ast.show_stmt stmt), stmt.loc))

and gen_stmt' stmt =
let gen_expr expr = gen_i_expr @@ Option.get expr.i_expr in
match stmt.exp with
| LabeledStmt (_, label, stmt) ->
    printf "%s:\n" label;
    gen_stmt stmt
| Empty
| TmpVar _
| TypedefStmt _ ->
    ()
| Var { var_decl_inits = decl_inits; _ } ->
    decl_inits |> List.iter (fun decl_init ->
        decl_init.di_init_assign |> List.iter (fun assign ->
            printf "# Var\n";
            gen_i_expr assign;
            Stack.pop "rax";
            printf "\n"
        )
    )
| Expr expr ->
    printf "# expr start %s\n" (Ast.show_expr_short expr.expr);
    gen_expr expr;
    Stack.pop "rax";
    printf "# expr end\n";
    printf "\n"
| Return expr ->
    (match expr with
    | Some expr ->
        printf "# return start %s\n" (Ast.show_expr_short expr.expr);
        gen_expr expr;
        Stack.pop "rax";
    | None ->
        printf "# return start\n";
        ()
    );
    printf "    mov rsp, rbp\n";
    printf "    pop rbp\n";
    printf "    ret\n";
    printf "# return end\n";
    printf "\n"
| If (expr, then_stmt, None) ->
    printf "# if\n";
    let end_label = Unique_id.new_id ".Lend" in
    gen_expr expr;
    Stack.pop "rax";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" end_label;
    gen_stmt then_stmt;
    printf "%s:\n" end_label;
    printf "# if end\n";
    printf "\n"
| If (expr, then_stmt, Some(else_stmt)) ->
    printf "# if else\n";
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
    printf "%s:\n" end_label;
    printf "# if else end\n";
    printf "\n"
| While (expr, stmt) ->
    printf "# while\n";
    Env.with_new_break_label (fun break_label ->
    Env.with_new_continue_label (fun continue_label ->
    printf "%s:\n" continue_label;
    gen_expr expr;
    Stack.pop "rax";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" break_label;
    gen_stmt stmt;
    printf "    jmp %s\n" continue_label;
    printf "%s:\n" break_label
    ));
    printf "# while end\n";
    printf "\n"
| Do (stmt, expr) ->
    printf "# do\n";
    Env.with_new_break_label (fun break_label ->
    Env.with_new_continue_label (fun continue_label ->
    let do_label = Unique_id.new_id ".Ldo" in
    printf "%s:\n" do_label;
    gen_stmt stmt;
    printf "%s:\n" continue_label;
    gen_expr expr;
    Stack.pop "rax";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" break_label;
    printf "    jmp %s\n" do_label;
    printf "%s:\n" break_label
    ));
    printf "# do end\n";
    printf "\n"
| For (init, cond, next, stmt) ->
    printf "# for\n";
    let begin_label = Unique_id.new_id ".Lbegin" in
    Env.with_new_break_label (fun break_label ->
    Env.with_new_continue_label (fun continue_label ->
    let gen_expr' expr =
        gen_expr expr;
        Stack.pop "rax";
    in
    let gen_cond expr =
        gen_expr expr;
        Stack.pop "rax";
        printf "    cmp rax, 0\n";
        printf "    je %s\n" break_label
    in
    Option.may gen_expr' init;
    printf "%s:\n" begin_label;
    Option.may gen_cond cond;
    gen_stmt stmt;
    printf "%s:\n" continue_label;
    Option.may gen_expr' next;
    printf "    jmp %s\n" begin_label;
    printf "%s:\n" break_label
    ));
    printf "# for end\n";
    printf "\n"
| Break ->
    let label = Env.get_break_label() in
    printf "    jmp %s\n" label
| Continue ->
    let label = Env.get_continue_label() in
    printf "    jmp %s\n" label
| Switch (expr, stmt) ->
    printf "# switch\n";
    gen_expr expr;
    Stack.pop "rax";
    (Env.with_new_break_label (fun break_label ->
    let rec gen_switch_branch stmt = match stmt.exp with
        | LabeledStmt(kind, label, stmt) ->
            (match kind with
            | Case {i_expr = Some i_expr; _} ->
                let c = Const.eval_int i_expr in
                printf "    cmp rax, %d\n" c;
                printf "    je %s\n" label;
            | Case _ ->
                failwith "case?" (* 型検査でi_exprが設定されているはず *)
            | Default ->
                printf "    jmp %s\n" label
            );
            gen_switch_branch stmt
        | If (_, stmt1, stmt2) ->
            gen_switch_branch stmt1;
            Option.may gen_switch_branch stmt2
        | While (_, stmt)
        | Do (stmt, _)
        | For (_, _, _, stmt)
        | Block stmt ->
            gen_switch_branch stmt
        | StmtList stmts ->
            List.iter gen_switch_branch stmts
        | Empty | Var _ | TmpVar _ | TypedefStmt _ | Expr _ | Return _ | Switch _ | Break | Continue ->
            ()
    in 
    gen_switch_branch stmt;
    printf "    jmp %s\n" break_label;
    gen_stmt stmt;
    printf "%s:\n" break_label
    ));
    printf "# switch end\n";
    printf "\n"
| Block stmt ->
    printf "# block\n";
    gen_stmt stmt;
    printf "# block end\n";
    printf "\n"
| StmtList stmt_list ->
    printf "# stmt list\n";
    List.iter gen_stmt stmt_list;
    printf "# stmt list end\n";
    printf "\n"

and gen_i_expr i_expr =
    try gen_i_expr' i_expr with
    e ->
        Printf.fprintf stderr "During gen_i_expr: %s\n" (Ast.show_i_expr i_expr);
        raise e

and gen_i_expr' i_expr = match i_expr with
| Const n ->
    if n <= 0xffffffff && n >= -0x80000000 then
        Stack.push @@ string_of_int n
    else (
        printf "mov rax, %d\n" n;
        Stack.push "rax"
    )
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
| ICall (func, i_expr_list) ->
    let gen_call prepare call =
        let n = List.length i_expr_list in
        let n_register = min n 6 in
        let n_stack = max (n-6) 0 in
        let stack_param_size = n_stack * 8 in
        let pop_register_params n =
            let regs = ["rdi";"rsi";"rdx";"rcx";"r8";"r9"] in
            List.take n regs |> List.iter Stack.pop in
        Stack.with_adjust stack_param_size (fun _ ->
            List.rev i_expr_list |> List.iter gen_i_expr;
            prepare();
            pop_register_params n_register;
            printf "    mov al, 0\n";
            call();
            if stack_param_size > 0 then Stack.add stack_param_size
        );
        Stack.push "rax" in
    (match func with
    | Label label when Builtin.is_builtin label ->
        List.iter gen_i_expr (List.rev i_expr_list);
        Builtin.gen label
    | Label label ->
        let prepare _ = () in
        let call _ = printf "    call %s\n" label in
        gen_call prepare call
    | _ ->
        let prepare _ = Stack.pop "r10" in
        let call _ = printf "    call r10\n" in
        gen_call prepare call
    )
| I_block block ->
    gen_stmt block;
    Stack.push "rax"
| ICond (cond,then_expr,else_expr) ->
    let else_label = Unique_id.new_id ".Lelse" in
    let end_label = Unique_id.new_id ".Lend" in
    gen_i_expr cond;
    Stack.pop "rax";
    printf "    cmp rax, 0\n";
    printf "    je %s\n" else_label;
    Stack.with_save (fun _ -> gen_i_expr then_expr);
    printf "    jmp %s\n" end_label;
    printf "%s:\n" else_label;
    gen_i_expr else_expr;
    printf "%s:\n" end_label
| IBitComplement e ->
    gen_i_expr e;
    Stack.pop "rax";
    printf "    not rax\n";
    Stack.push "rax";
| IBoolOfRetval e ->
    gen_i_expr e;
    Stack.pop "rax";
    printf "    movzx eax, al\n";
    Stack.push "rax"
| IBoolOfInt e ->
    gen_i_expr e;
    Stack.pop "rax";
    printf "    cmp rax, 0\n";
    printf "    setne al\n";
    Stack.push "rax"
| ICharOfInt e ->
    gen_i_expr e;
    Stack.pop "rax";
    printf "    movsx rax,al\n";
    Stack.push "rax"
| IShortOfInt e ->
    gen_i_expr e;
    Stack.pop "rax";
    printf "    movsx rax,ax\n";
    Stack.push "rax"
| IIntOfInt e ->
    gen_i_expr e;
    Stack.pop "rax";
    printf "    cdqe\n";
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
| Error { error_exn = e } ->
    raise e

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
        printf "    movzx rax, al\n"
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
    | LShift ->
        printf "    mov cl, dil\n";
        printf "    shl rax, cl\n"
    | RShift ->
        printf "    mov cl, dil\n";
        printf "    sar rax, cl\n"
    | Comma ->
        printf "    mov rax, rdi\n"
    | Store ty ->
        Gen_misc.store ty "[rax]" "rdi";
        printf "    mov rax, rdi\n"
    (* 以下の演算子は別扱いなのでここには来ない *)
    | LAnd
    | LOr ->
        failwith "op?"
