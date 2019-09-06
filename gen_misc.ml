type size = S8 | S16 | S32 | S64

(* typeのサイズを考慮したloadとstore *)
(* レジスタ名は64bitのもので指定 *)
(* 指定したタイプに応じて変化する *)
exception Invalid_size

let select_size ty = match ty with
| Type.Ptr _ -> S64
| Type.Long -> S64
| Type.Int -> S32
| Type.Short -> S16
| Type.Char -> S8
| _ -> raise(Misc.Error("gen_misc.ml:select_size: " ^ (Type.show_type ty)))

let select_reg ty reg = match (select_size ty, reg) with
| (S64, _) -> reg
| (S32, "rax") -> "eax"
| (S32, "rdi") -> "edi"
| (S32, "rsi") -> "esi"
| (S32, "rdx") -> "edx"
| (S32, "rcx") -> "ecx"
| (S32, "rbx") -> "ebx"
| (S32, "r8")  -> "r8d"
| (S32, "r9")  -> "r9d"
| (S32, "r10") -> "r10d"
| (S16, "rax") -> "ax"
| (S16, "rdi") -> "di"
| (S16, "rsi") -> "si"
| (S16, "rdx") -> "dx"
| (S16, "rcx") -> "cx"
| (S16, "rbx") -> "bx"
| (S16, "r8")  -> "r8w"
| (S16, "r9")  -> "r9w"
| (S16, "r10") -> "r10w"
| (S8, "rax") -> "al"
| (S8, "rdi") -> "dil"
| (S8, "rsi") -> "sil"
| (S8, "rdx") -> "dl"
| (S8, "rcx") -> "cl"
| (S8, "rbx") -> "bl"
| (S8, "r8")  -> "r8b"
| (S8, "r9")  -> "r9b"
| (S8, "r10") -> "r10b"
| _ -> failwith "?"

let load ty dst src = Printf.(match select_size ty with
| S8 ->
    printf "    movsx %s, BYTE PTR %s\n" dst src
| S16 ->
    printf "    movsx %s, WORD PTR %s\n" dst src
| S32 ->
    printf "    movsxd %s, DWORD PTR %s\n" dst src
| S64 ->
    printf "    mov %s, %s\n" (select_reg ty dst) src
)

let store ty dst src =
    Printf.printf "    mov %s, %s\n" dst (select_reg ty src)

