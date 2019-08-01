%{
    open Ast
%}

%token PLUS MINUS AST SLASH AMP

%token LT LE GT GE EQ NE

%token ASSIGN

%token SEMI COMMA

%token LPAR RPAR LBRACE RBRACE

%token RETURN IF ELSE WHILE FOR

%token INT

%token <string> NUM         // 整数トークン
%token <string> IDENT

%token EOF

%type <Ast.decl list> translation_unit

%start translation_unit

%%

translation_unit:
| l=decl* EOF { l }

decl:
| INT func=IDENT LPAR params=separated_list(COMMA, t=type_spec d=declarator { type_and_var t d }) RPAR body=block {
    { exp = Function (func, params, body); loc = $startpos(func) }
}

type_spec:
| INT { Type.Int }

declarator:
| var=IDENT { { exp = DeclIdent var; loc = $startpos(var) } }
| token=AST d=declarator { { exp = PointerOf d; loc = $startpos(token) } }

stmt:
| t=type_spec d=declarator SEMI {
    let (ty, name) = type_and_var t d in
    { exp = Var (ty, name); loc = d.loc }
}
| e=expr SEMI { { exp = Expr e; loc = e.loc } }
| token=RETURN e=expr SEMI { { exp = Return e; loc = $startpos(token) } }
| token=IF LPAR e=expr RPAR then_stmt=stmt else_stmt=option(ELSE s=stmt {s}) {
    { exp = If (e, then_stmt, else_stmt); loc = $startpos(token) }
}
| token=WHILE LPAR e=expr RPAR s=stmt { { exp = While (e, s); loc = $startpos(token) } }
| token=FOR LPAR init=expr? SEMI cond=expr? SEMI next=expr? RPAR s=stmt { { exp = For (init, cond, next, s); loc = $startpos(token) } }
| b=block { b }

block:
| token=LBRACE l=stmt* RBRACE { { exp = Block l; loc = $startpos(token) } }

expr:
| e=assign { e }

assign:
| e=equality { e }
| l=assign token=ASSIGN r=equality { { exp = no_type (Assign (l, r)); loc = $startpos(token) } }

equality:
| e=relational { e }
| l=equality token=EQ r=relational { { exp = no_type (Eq (l, r)); loc = $startpos(token) } }
| l=equality token=NE r=relational { { exp = no_type (Ne (l, r)); loc = $startpos(token) } }

relational:
| e=add { e }
| l=relational token=LT r=add { { exp = no_type (Lt (l, r)); loc = $startpos(token) } }
| l=relational token=LE r=add { { exp = no_type (Le (l, r)); loc = $startpos(token) } }
| l=relational token=GT r=add { { exp = no_type (Lt (r, l)); loc = $startpos(token) } }
| l=relational token=GE r=add { { exp = no_type (Le (r, l)); loc = $startpos(token) } }

add:
| e=mul { e }
| e=add token=PLUS m=mul { { exp = no_type (Add (e, m)); loc = $startpos(token) } }
| e=add token=MINUS m=mul { { exp = no_type (Sub (e, m)); loc = $startpos(token) } }

mul:
| e=unary { e }
| l=mul token=AST r=unary { { exp = no_type (Mul (l, r)); loc = $startpos(token) } }
| l=mul token=SLASH r=unary { { exp = no_type (Div (l, r)); loc = $startpos(token) } }

unary:
| e=term { e }
| PLUS e=term { e }
| token=MINUS e=term { { exp = no_type (Sub ({ exp = no_type (Num "0"); loc = $startpos(token) }, e)); loc = $startpos(token) } }
| token=AST e=unary { { exp = no_type (Deref e); loc = $startpos(token) } }
| token=AMP e=unary { { exp = no_type (Addr e); loc = $startpos(token) } }

term:
| n=NUM { { exp = no_type (Num n); loc = $startpos(n) } }
| id=IDENT { { exp = no_type (Ident id); loc = $startpos(id) } }
| func=IDENT LPAR l=separated_list(COMMA, expr) RPAR { { exp = no_type (Call (func, l)); loc = $startpos(func) } }
| LPAR e=expr RPAR { e }
