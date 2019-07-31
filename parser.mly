%{
    open Ast
    open Printf
%}

%token PLUS
%token MINUS
%token AST
%token SLASH

%token LPAR
%token RPAR

%token <string> NUM         // 整数トークン

%token EOF

%type <Ast.expr> translation_unit

%start translation_unit

%%

translation_unit:
| e=expr EOF { e }

expr:
| m=mul { m }
| e=expr PLUS m=mul { Add (e, m) }
| e=expr MINUS m=mul { Sub (e, m) }

mul:
| u=unary { u }
| m=mul AST u=unary { Mul (m, u) }
| m=mul SLASH u=unary { Div (m, u) }

unary:
| t=term { t }
| PLUS t=term { t }
| MINUS t=term { Sub (Num "0", t) }

term:
| n=NUM { Num n }
| LPAR e=expr RPAR { e }
