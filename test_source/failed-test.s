.intel_syntax noprefix
    .section .rodata
.LC0:
    .string "hoge"
.LC1:
    .string "hoge"
    .text
    .globl test0
    .type test0, @function
test0:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 0
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test1
    .type test1, @function
test1:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 42
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test2
    .type test2, @function
test2:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 5
    push 20
    pop rdi
    pop rax
    add rax, rdi
    push rax
    push 4
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test3
    .type test3, @function
test3:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 12
    push 34
    pop rdi
    pop rax
    add rax, rdi
    push rax
    push 5
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test4
    .type test4, @function
test4:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 5
    push 6
    push 7
    pop rdi
    pop rax
    imul rax, rdi
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test5
    .type test5, @function
test5:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    push 10
    push 2
    pop rdi
    pop rax
    cqo
    idiv rdi
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test6
    .type test6, @function
test6:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 5
    push 9
    push 6
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rdi
    pop rax
    imul rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test7
    .type test7, @function
test7:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 3
    push 5
    pop rdi
    pop rax
    add rax, rdi
    push rax
    push 2
    pop rdi
    pop rax
    cqo
    idiv rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test8
    .type test8, @function
test8:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 0
    push 10
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    push 15
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test9
    .type test9, @function
test9:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 0
    push 2
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    push 0
    push 2
    push 3
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rdi
    pop rax
    imul rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test10
    .type test10, @function
test10:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    push 1
    pop rdi
    pop rax
    cmp rax, rdi
    sete al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test11
    .type test11, @function
test11:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    push 0
    pop rdi
    pop rax
    cmp rax, rdi
    sete al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test12
    .type test12, @function
test12:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 3
    push 2
    pop rdi
    pop rax
    imul rax, rdi
    push rax
    push 6
    pop rdi
    pop rax
    cmp rax, rdi
    setne al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test13
    .type test13, @function
test13:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    push 10
    push 2
    pop rdi
    pop rax
    cqo
    idiv rdi
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    push 5
    pop rdi
    pop rax
    cmp rax, rdi
    setne al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test14
    .type test14, @function
test14:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test15
    .type test15, @function
test15:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test16
    .type test16, @function
test16:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 3
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test17
    .type test17, @function
test17:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setle al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test18
    .type test18, @function
test18:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setle al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test19
    .type test19, @function
test19:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 3
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setle al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test20
    .type test20, @function
test20:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 1
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test21
    .type test21, @function
test21:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test22
    .type test22, @function
test22:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 3
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test23
    .type test23, @function
test23:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 1
    pop rdi
    pop rax
    cmp rax, rdi
    setle al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test24
    .type test24, @function
test24:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setle al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test25
    .type test25, @function
test25:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    push 3
    pop rdi
    pop rax
    cmp rax, rdi
    setle al
    movzb rax, al
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test26
    .type test26, @function
test26:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    push 2
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    imul rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test27
    .type test27, @function
test27:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 4
    push rax
    push 1
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    push 2
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test28
    .type test28, @function
test28:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 2
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    push 3
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test29
    .type test29, @function
test29:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 4
    push rax
    push 1
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    push 2
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test30
    .type test30, @function
test30:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    pop rax
    cmp rax, 0
    je .Lend0
    push 2
    pop rax
    mov rsp, rbp
    pop rbp
    ret
.Lend0:
    push 3
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test31
    .type test31, @function
test31:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 0
    pop rax
    cmp rax, 0
    je .Lend1
    push 2
    pop rax
    mov rsp, rbp
    pop rbp
    ret
.Lend1:
    push 3
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test32
    .type test32, @function
test32:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    pop rax
    cmp rax, 0
    je .Lelse2
    push 2
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    jmp .Lend2
.Lelse2:
    push 3
    pop rax
    mov rsp, rbp
    pop rbp
    ret
.Lend2:
    push 4
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test33
    .type test33, @function
test33:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 0
    pop rax
    cmp rax, 0
    je .Lelse3
    push 2
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    jmp .Lend3
.Lelse3:
    push 3
    pop rax
    mov rsp, rbp
    pop rbp
    ret
.Lend3:
    push 4
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test34
    .type test34, @function
test34:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    push 0
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
.Lbegin4:
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 5
    pop rdi
    pop rax
    cmp rax, rdi
    setne al
    movzb rax, al
    push rax
    pop rax
    cmp rax, 0
    je .Lend4
    mov rax, rbp
    sub rax, 4
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    jmp .Lbegin4
.Lend4:
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test35
    .type test35, @function
test35:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    push 0
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
.Lbegin5:
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 9999999
    pop rdi
    pop rax
    cmp rax, rdi
    setne al
    movzb rax, al
    push rax
    pop rax
    cmp rax, 0
    je .Lend5
    mov rax, rbp
    sub rax, 4
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    jmp .Lbegin5
.Lend5:
    push 0
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test36
    .type test36, @function
test36:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    push 0
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    push 0
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
.Lbegin6:
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 5
    pop rdi
    pop rax
    cmp rax, rdi
    setl al
    movzb rax, al
    push rax
    pop rax
    cmp rax, 0
    je .Lend6
    mov rax, rbp
    sub rax, 8
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 10
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    jmp .Lbegin6
.Lend6:
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test37
    .type test37, @function
test37:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    push 0
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
.Lbegin7:
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 9999999
    pop rdi
    pop rax
    cmp rax, rdi
    setne al
    movzb rax, al
    push rax
    pop rax
    cmp rax, 0
    je .Lend7
    push 0
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rax, rbp
    sub rax, 4
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    jmp .Lbegin7
.Lend7:
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test38
    .type test38, @function
test38:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    push 1
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test39
    .type test39, @function
test39:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    mov al, 0
    call foo1
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test40
    .type test40, @function
test40:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 1
    sub rsp, 8
    mov al, 0
    call foo2
    add rsp, 8
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    push 2
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test41
    .type test41, @function
test41:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 8
    push 7
    push 6
    push 5
    push 4
    push 3
    push 2
    push 1
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop r8
    pop r9
    mov al, 0
    call foo3
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test42
    .type test42, @function
test42:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 3
    push 2
    pop rdi
    pop rsi
    mov al, 0
    call add
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl add
    .type add, @function
add:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov [rax], edi
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov [rax], esi
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test43
    .type test43, @function
test43:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 20
    pop rdi
    mov al, 0
    call fib
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl fib
    .type fib, @function
fib:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov [rax], edi
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    cmp rax, rdi
    setle al
    movzb rax, al
    push rax
    pop rax
    cmp rax, 0
    je .Lelse8
    push 1
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    jmp .Lend8
.Lelse8:
    sub rsp, 12
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rdi
    mov al, 0
    call fib
    add rsp, 12
    push rax
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rdi
    mov al, 0
    call fib
    add rsp, 4
    push rax
    pop rdi
    pop rax
    add rax, rdi
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
.Lend8:
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test44
    .type test44, @function
test44:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, rbp
    sub rax, 4
    push rax
    push 3
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    push 3
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test45
    .type test45, @function
test45:
    push rbp
    mov rbp, rsp
    sub rsp, 20
    mov rax, rbp
    sub rax, 4
    push rax
    push 3
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 16
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 20
    push rax
    mov rax, rbp
    sub rax, 16
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    sub rsp, 12
    mov rax, rbp
    sub rax, 20
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 12
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test46
    .type test46, @function
test46:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, rbp
    sub rax, 4
    push rax
    push 3
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 16
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 16
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test47
    .type test47, @function
test47:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test48
    .type test48, @function
test48:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test49
    .type test49, @function
test49:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test50
    .type test50, @function
test50:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test51
    .type test51, @function
test51:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test52
    .type test52, @function
test52:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    sub rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test53
    .type test53, @function
test53:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test54
    .type test54, @function
test54:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    sub rax, rdi
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test55
    .type test55, @function
test55:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, rbp
    sub rax, 8
    push rax
    sub rsp, 8
    push 1
    pop rdi
    mov al, 0
    call at
    add rsp, 8
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 16
    push rax
    sub rsp, 8
    push 3
    pop rdi
    mov al, 0
    call at
    add rsp, 8
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 16
    push rax
    pop rax
    mov rax, [rax]
    push rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rdi
    pop rax
    sub rax, rdi
    mov rdi, 4
    cqo
    idiv rdi
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test56
    .type test56, @function
test56:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, rbp
    sub rax, 8
    push rax
    sub rsp, 8
    push 1
    pop rdi
    mov al, 0
    call at
    add rsp, 8
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 16
    push rax
    sub rsp, 8
    push 3
    pop rdi
    mov al, 0
    call at
    add rsp, 8
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    mov rax, rbp
    sub rax, 16
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rdi
    pop rax
    sub rax, rdi
    mov rdi, 4
    cqo
    idiv rdi
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test57
    .type test57, @function
test57:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    mov rax, 4
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test58
    .type test58, @function
test58:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, 4
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test59
    .type test59, @function
test59:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test60
    .type test60, @function
test60:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test61
    .type test61, @function
test61:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, 4
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test62
    .type test62, @function
test62:
    push rbp
    mov rbp, rsp
    sub rsp, 40
    push 0
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test63
    .type test63, @function
test63:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test64
    .type test64, @function
test64:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test65
    .type test65, @function
test65:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test66
    .type test66, @function
test66:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test67
    .type test67, @function
test67:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test68
    .type test68, @function
test68:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test69
    .type test69, @function
test69:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test70
    .type test70, @function
test70:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    mov al, 0
    call getdata2
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 8
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 2
    pop rdi
    pop rax
    mov rbx, 8
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test71
    .type test71, @function
test71:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, rbp
    sub rax, 16
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 16
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test72
    .type test72, @function
test72:
    push rbp
    mov rbp, rsp
    sub rsp, 40
    mov rax, rbp
    sub rax, 40
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    push 1
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    sub rsp, 8
    mov rax, rbp
    sub rax, 40
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test73
    .type test73, @function
test73:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov rax, rbp
    sub rax, 48
    push rax
    mov rax, rbp
    sub rax, 40
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 48
    push rax
    pop rax
    mov rax, [rax]
    push rax
    push 1
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 40
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test74
    .type test74, @function
test74:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov rax, rbp
    sub rax, 40
    push rax
    push 5
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    push 1
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 48
    push rax
    mov rax, rbp
    sub rax, 40
    push rax
    push 5
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], rdi
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 48
    push rax
    pop rax
    mov rax, [rax]
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test75
    .type test75, @function
test75:
    push rbp
    mov rbp, rsp
    sub rsp, 40
    sub rsp, 8
    mov rax, rbp
    sub rax, 40
    push rax
    push 5
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    mov rax, rbp
    sub rax, 40
    push rax
    pop rdi
    pop rax
    sub rax, rdi
    mov rdi, 4
    cqo
    idiv rdi
    push rax
    pop rdi
    mov al, 0
    call pr_int
    add rsp, 8
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .bss
    .align 4
    .type a76, @object
    .size a76, 4
a76:
    .zero 4
    .text
    .globl f76
    .type f76, @function
f76:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov [rax], edi
    mov rax, OFFSET FLAT:a76
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test76
    .type test76, @function
test76:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 10
    pop rdi
    mov al, 0
    call f76
    push rax
    pop rax
    mov rax, OFFSET FLAT:a76
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl g77
    .type g77, @function
g77:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 100
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
    .bss
    .align 4
    .type a77, @object
    .size a77, 4
a77:
    .zero 4
    .text
    .globl f77
    .type f77, @function
f77:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov [rax], edi
    mov rax, OFFSET FLAT:a77
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test77
    .type test77, @function
test77:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    mov al, 0
    call g77
    push rax
    pop rdi
    mov al, 0
    call f77
    push rax
    pop rax
    mov rax, OFFSET FLAT:a77
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .bss
    .align 4
    .type a78, @object
    .size a78, 40
a78:
    .zero 40
    .text
    .globl f78
    .type f78, @function
f78:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov [rax], edi
    mov rax, OFFSET FLAT:a78
    push rax
    push 3
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    mov rax, rbp
    sub rax, 4
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test78
    .type test78, @function
test78:
    push rbp
    mov rbp, rsp
    sub rsp, 0
    push 10
    pop rdi
    mov al, 0
    call f78
    push rax
    pop rax
    mov rax, OFFSET FLAT:a78
    push rax
    push 3
    pop rdi
    pop rax
    mov rbx, 4
    imul rdi, rbx
    add rax, rdi
    push rax
    pop rax
    mov eax, [rax]
    push rax
    pop rdi
    mov al, 0
    call pr_int
    push rax
    pop rax
    mov rsp, rbp
    pop rbp
    ret
    .text
    .globl test79
    .type test79, @function
test79:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov rax, rbp
    sub rax, 3
    push rax
    push 0
    pop rdi
    pop rax
    mov rbx, 1
    imul rdi, rbx
    add rax, rdi
    push rax
    push 0
    push 1
    pop rdi
    pop rax
    sub rax, rdi
    push rax
    pop rdi
    pop rax
    mov [rax], dil
    push rdi
    pop rax
    mov rax, rbp
    sub rax, 8
    push rax
    push 4
    pop rdi
    pop rax
    mov [rax], edi
    push rdi
    pop rax
