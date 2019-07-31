%{
    open Ast
    open Printf
%}

%token PLUS
%token MINUS

%token <string> NUM         // 整数トークン

%token EOF

%type <Ast.expr> translation_unit

%start translation_unit

%%

translation_unit:
| e=expr EOF { e }

expr:
| t=term { t }
| e=expr PLUS t=term { Add (e, t) }
| e=expr MINUS t=term { Sub (e, t) }

term:
| n=NUM { Num n }
