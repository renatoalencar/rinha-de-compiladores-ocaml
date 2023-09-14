// Replace crt0 for now, the idea is to replace
// parts of libc and probably the whole thing
// Since we won't need a lot of it anyway.

.text
.global _start
_start:
    xorq %rbp, %rbp
    movq (%rsp), %rdi // argc
    movq 8(%rsp), %rsi // argv
    movq 16(%rsp), %rcx // envp
    call main
    movq %rax, %rdi
    call _exit