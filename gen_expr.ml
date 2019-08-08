open Ast
open Env
open Misc
open Printf

let rec gen_lval_entry entry = match entry with
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
| Addr e ->
    gen_lval e
| Deref e ->
    let ty = get_type expr in
    gen_expr e;
    Stack.pop "rax";
    Gen_misc.load ty "rax" "[rax]";
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
