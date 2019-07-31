%{
    open Printf
%}

%token <string> NUM         // 整数トークン

%token EOF

%type <unit> translation_unit

%start translation_unit

%%

translation_unit:
| n=NUM EOF {
    printf ".intel_syntax noprefix\n";
    printf ".global main\n";
    printf "main:\n";
    printf "    mov rax, %d\n" (int_of_string n);
    printf "    ret\n"
}
