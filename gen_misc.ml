(* typeのサイズを考慮したloadとstore *)
(* レジスタ名は64bitのもので指定 *)
(* 指定したタイプに応じて変化する *)
exception Invalid_size

let select_size ty = match ty with
| Type.Ptr _ -> 64
| Type.Long -> 64
| Type.Int -> 32
| Type.Short -> 16
| Type.Char -> 8
| _ -> raise(Misc.Error("gen_misc.ml:select_size: " ^ (Type.show_type ty)))

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
| (16, "rax") -> "ax"
| (16, "rdi") -> "di"
| (16, "rsi") -> "si"
| (16, "rdx") -> "dx"
| (16, "rcx") -> "cx"
| (16, "rbx") -> "bx"
| (16, "r8")  -> "r8w"
| (16, "r9")  -> "r9w"
| (16, "r10") -> "r10w"
| (8, "rax") -> "al"
| (8, "rdi") -> "dil"
| (8, "rsi") -> "sil"
| (8, "rdx") -> "dl"
| (8, "rcx") -> "cl"
| (8, "rbx") -> "bl"
| (8, "r8")  -> "r8b"
| (8, "r9")  -> "r9b"
| (8, "r10") -> "r10b"
| _ -> failwith "?"

let load ty dst src = Printf.(match ty with
| Type.Char ->
    printf "    movsx %s, BYTE PTR [%s]\n" dst src
| Type.Short ->
    printf "    movsx %s, WORD PTR [%s]\n" dst src
| Type.Int ->
    printf "    movsx %s, DWORD PTR [%s]\n" dst src
| _ ->
    printf "    mov %s, %s\n" (select_reg ty dst) src
)

let store ty dst src =
    Printf.printf "    mov %s, %s\n" dst (select_reg ty src)

