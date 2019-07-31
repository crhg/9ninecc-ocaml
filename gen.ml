open Ast
open Printf

let rec gen expr =
    printf ".intel_syntax noprefix\n";
    printf ".global main\n";
    printf "main:\n";
    gen_expr expr;
    printf "    ret\n"

and gen_expr expr = match expr with
| Num n ->
    printf "    mov rax, %d\n" (int_of_string n)
| Add (l, Num n) ->
    gen_expr l;
    printf "    add rax, %d\n" (int_of_string n)
| Sub (l, Num n) ->
    gen_expr l;
    printf "    sub rax, %d\n" (int_of_string n)
| _ -> failwith "gen_expr error"
