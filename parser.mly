%{
    open Ast
    open Printf
%}

%token PLUS MINUS AST SLASH

%token LT LE GT GE EQ NE

%token LPAR RPAR

%token <string> NUM         // 整数トークン

%token EOF

%type <Ast.expr> translation_unit

%start translation_unit

%%

translation_unit:
| e=expr EOF { e }

expr:
| e=equality { e }

equality:
| e=relational { e }
| l=equality EQ r=relational { Eq (l, r) }
| l=equality NE r=relational { Ne (l, r) }

relational:
| e=add { e }
| l=relational LT r=add { Lt (l, r) }
| l=relational LE r=add { Le (l, r) }
| l=relational GT r=add { Lt (r, l) }
| l=relational GE r=add { Le (r, l) }

add:
| e=mul { e }
| e=add PLUS m=mul { Add (e, m) }
| e=add MINUS m=mul { Sub (e, m) }

mul:
| e=unary { e }
| l=mul AST r=unary { Mul (l, r) }
| l=mul SLASH r=unary { Div (l, r) }

unary:
| e=term { e }
| PLUS e=term { e }
| MINUS e=term { Sub (Num "0", e) }

term:
| n=NUM { Num n }
| LPAR e=expr RPAR { e }
