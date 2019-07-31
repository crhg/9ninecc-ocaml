%{
    open Ast
%}

%token PLUS MINUS AST SLASH

%token LT LE GT GE EQ NE

%token ASSIGN

%token SEMI

%token LPAR RPAR

%token RETURN IF ELSE

%token <string> NUM         // 整数トークン
%token <string> IDENT

%token EOF

%type <Ast.stmt list> translation_unit

%start translation_unit

%%

translation_unit:
| l=stmt* EOF { l }

stmt:
| e=expr SEMI { Expr e }
| RETURN e=expr SEMI { Return e }
| IF LPAR e=expr RPAR then_stmt=stmt else_stmt=option(ELSE s=stmt {s}) {
    If (e, then_stmt, else_stmt)
}

expr:
| e=assign { e }

assign:
| e=equality { e }
| l=assign ASSIGN r=equality { Assign (l, r) }

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
| id=IDENT { Ident id }
| LPAR e=expr RPAR { e }
