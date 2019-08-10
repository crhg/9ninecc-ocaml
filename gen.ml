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
| GlobalVarDecl { gv_decl_inits = decl_inits } ->
    decl_inits |> List.iter (fun di -> match di with
        | { di_entry = Some (GlobalVar (ty, label)); di_init = None } ->
            printf "    .globl %s\n" label;
            printf "    .bss\n";
            printf "    .align %d\n" (Type.get_alignment ty);
            printf "    .type %s, @object\n" label;
            printf "    .size %s, %d\n" label (Type.get_size ty);
            printf "%s:\n" label;
            printf "    .zero %d\n" (Type.get_size ty)
        | { di_entry = Some (GlobalVar (ty, label)); di_init = Some init } ->
            Init_global.gen ty label init
    )

| FunctionDecl { func_name=Some func; func_params=Some params; func_body=body; func_frame_size=Some size } ->

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

    (* 文の中で式を実行した場合は必ず生成された値をpopする決まりとするので *)
    (* この時点でのスタック位置が式のコード生成開始時のスタック位置になる *)

    let copy_param i {param_ty = ty; param_name = name; param_entry = Some entry } =
        gen_lval_entry entry;
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
| _ -> failwith("gen_decl: "^(Ast.show_decl decl))

and gen_stmt stmt =
try Stack.check_no_change @@ fun _ -> gen_stmt' stmt with
| Stack.Stack_changed ->
    raise(Error_at("stack changed: " ^ (Ast.show_stmt stmt), stmt.loc))

and gen_stmt' stmt =
(* let gen_expr expr = *)
(*     Stack.with_save (fun _ -> gen_expr expr) *)
(* in *)
match stmt.exp with
| Empty -> ()
| Var { var_decl_inits = decl_inits } ->
    decl_inits |> List.iter (fun decl_init ->
        decl_init.di_init_assign |> List.iter (fun assign ->
            gen_expr assign;
            Stack.pop "rax"
        )
    )
| Expr expr ->
    printf "# expr start\n";
    gen_expr expr;
    Stack.pop "rax";
    printf "# expr end\n"
| Return expr ->
    printf "# return start\n";
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
| Ident { entry = Some entry} ->
    gen_lval_entry entry
| Deref e ->
    gen_expr e
| Arrow (e, name) ->
    let Some (Ptr ty) = e.exp.ty in
    let field = Type.get_field ty name in
    gen_expr e;
    Stack.pop("rax");
    printf "    add rax, %d\n" field.field_offset;
    Stack.push("rax");
| _ -> raise (Error_at("not lval: " ^ show_expr expr, expr.loc))

and binop op l r = 
    gen_expr l;
    gen_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    op ();
    Stack.push "rax"

and get_type expr =
    try Option.get(expr.exp.ty) with
    | Option.No_value ->
        raise(Error_at("get_type: type?: " ^ (Ast.show_expr expr), expr.loc))

and gen_expr expr =
match expr.exp.e with
| Num n ->
    Stack.push n
| Str (_, label) ->
    printf "    mov rax, OFFSET FLAT:%s\n" label;
    Stack.push "rax"
| Ident { entry = Some entry } ->
    let ty = entry_type entry in
    begin
        match ty with
        | Array (_, _) -> 
            gen_lval expr
        | _ ->
            gen_lval expr;
            Stack.pop "rax";
            Gen_misc.load ty "rax" "[rax]";
            Stack.push "rax"
    end
| Assign (l, r) ->
    let ty = get_type expr in
    gen_lval l;
    gen_expr r;
    Stack.pop "rdi";
    Stack.pop "rax";
    Gen_misc.store ty "[rax]" "rdi";
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
        printf "    call %s\n" func;
        (if n_stack_param > 0 then Stack.add (n_stack_param * 8));
    );
    Stack.push "rax"
| BlockExpr block ->
    gen_stmt block;
    Stack.push "rax"
| Addr e ->
    gen_lval e
| Deref e ->
    let ty = get_type expr in
    gen_expr e;
    Stack.pop "rax";
    Gen_misc.load ty "rax" "[rax]";
    Stack.push "rax"
| Arrow ({exp = { ty = Some (Ptr (e_ty))}} as e, name) ->
    let field = Type.get_field e_ty name in
    gen_expr e;
    Stack.pop "rax";
    printf "    add rax, %d\n" field.field_offset;
    Gen_misc.load field.field_type "rax" "[rax]";
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
            | _ -> raise(Error_at("invalid operand type combination: " ^ (Ast.show_expr expr), expr.loc))
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
        | _ -> raise(Error_at("invalid operand type combination: " ^ (Ast.show_expr expr), expr.loc))
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
| _ ->
    raise(Error_at("gen_expr invalid expr: " ^ (Ast.show_expr expr), expr.loc))
