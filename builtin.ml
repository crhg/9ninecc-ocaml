let register _ =
    let ty = Type.(Function (Void, [])) in
    let name = "__builtin_va_start" in
    Env.register_global_var ty name name

and is_builtin label =
    match label with
    | "__builtin_va_start" -> true
    | _ -> false

and gen label =
    match label with
    | "__builtin_va_start" ->
        let open Printf in
        (* ADM64 ABIの3.5.7 Variable Argument Lists参照 *)
        (* https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf *)
        Stack.pop "rax";
        printf "  mov edi, dword ptr [rbp-8]\n";
        printf "  mov dword ptr [rax], edi\n";
        printf "  mov dword ptr [rax+4], 0\n";
        printf "  lea rdi, [rbp + 16]\n";
        printf "  mov qword ptr [rax+8], rdi\n";
        printf "  lea rdi, [rbp - 56]\n";
        printf "  mov qword ptr [rax+16], rdi\n";
        Stack.push "rax"

    | _ -> failwith("unknown: " ^ label)
