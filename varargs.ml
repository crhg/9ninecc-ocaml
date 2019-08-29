let save_area_size = Misc.round_up (
    6 * 8 (* 64bitレジスタ×6個 *)
    + 8 (* overflow_arg_areaに設定する値を保存する(ポインタ) *)
    + 4 (* gp_offsetに設定する値を保存する(int) *)
) 8

let overflow_arg  = save_area_size - 0
let reg_save_area = save_area_size - 8
let gp            = save_area_size - 8 - 6 * 8

let gen_va_start _ =
    let open Printf in
    (* ADM64 ABIの3.5.7 Variable Argument Lists参照 *)
    (* https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf *)
    printf "# va_start\n";
    Stack.pop "rax";
    printf "    mov edi, [rbp - %d]\n" gp;
    printf "    mov [rax], edi # gp_offset\n";
    printf "    mov dword ptr [rax+4], 0 # fp_offset\n"; (* 浮動小数点は未対応 *)
    printf "    mov rdi, [rbp - %d]\n" overflow_arg;
    printf "    mov [rax+8], rdi # overflow_arg_area\n";
    printf "    lea rdi, [rbp - %d]\n" reg_save_area;
    printf "    mov [rax+16], rdi # reg_save_area\n";
    Stack.push "rax";
    printf "\n"

and gen_register_save n =
    let open Printf in

    printf "# register_save\n";
    ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] |> List.iteri (fun i reg ->
        printf "    mov [rbp-%d], %s\n" (reg_save_area - i * 8) reg
    );

    let n_stack = max (n - 6) 0 in
    printf "    lea r10, [rbp + %d]\n" (16 + 8 * n_stack);
    printf "    mov [rbp - %d], r10\n" overflow_arg;

    let n_register = min n 6 in
    printf "    mov dword ptr [rbp - %d], %d\n" gp (n_register * 8);

    printf "\n"
