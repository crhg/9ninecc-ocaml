%{
    open Ast
    open Misc
%}

%token PLUS MINUS AST SLASH AMP

%token LT LE GT GE EQ NE

%token ASSIGN

%token DOT ARROW

%token SEMI COMMA

%token LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET

%token RETURN IF ELSE WHILE FOR

%token SIZEOF

%token LONG INT SHORT CHAR STRUCT UNION

%token <string> NUM  // 整数トークン
%token <string> STR  // 文字列リテラル
%token <string> IDENT

%token EOF

%type <Ast.decl list> translation_unit

%start translation_unit

%%

translation_unit:
| l=decl* EOF { l }

decl:
| t=type_spec decl_inits = separated_list(COMMA, decl_init) SEMI {
    { 
        exp = GlobalVarDecl {
            gv_ts = t;
            gv_decl_inits = decl_inits
        };
        loc = t.loc
    }
}
| t=type_spec d=declarator body=block {
    {
        exp = FunctionDecl {
            func_ts = t;
            func_decl = d;
            func_body = body;
            func_ty = None;
            func_name = None;
            func_params = None;
            func_frame_size = None
        };
        loc = d.loc
    }
}

type_spec:
| token=LONG  { ignore token; { exp = Ast.Long;  loc = $startpos(token) } }
| token=INT   { ignore token; { exp = Ast.Int;   loc = $startpos(token) } }
| token=SHORT { ignore token; { exp = Ast.Short; loc = $startpos(token) } }
| token=CHAR  { ignore token; { exp = Ast.Char;  loc = $startpos(token) } }
| token=STRUCT fields=su_body {
    ignore token; 
    { exp = Struct { su_tag = None; su_fields = Some fields }; loc = $startpos(token) }
}
| token=STRUCT tag=IDENT fields=option(su_body) {
    ignore token; 
    { exp = Struct { su_tag = Some tag; su_fields = fields }; loc = $startpos(token) }
}
| token=UNION fields=su_body {
    ignore token; 
    { exp = Union { su_tag = None; su_fields = Some fields }; loc = $startpos(token) }
}
| token=UNION tag=IDENT fields=option(su_body) {
    ignore token; 
    { exp = Union { su_tag = Some tag; su_fields = fields }; loc = $startpos(token) }
}

su_body:
| LBRACE l=su_field* RBRACE { l }

su_field:
| ts=type_spec d=declarator SEMI { (ts, d) }

decl_init:
| decl=declarator init=option(ASSIGN i=init {i}) {
    { di_decl = decl; di_init = init; di_entry = None; di_init_assign = [] }
}

declarator:
| d=direct_declarator { d }
| token=AST d=declarator { ignore token; { exp = PointerOf d; loc = d.loc } }

direct_declarator:
| var=IDENT { { exp = DeclIdent var; loc = $startpos(var) } }
| LPAR d=declarator RPAR { d }
| d=direct_declarator LBRACKET e=option(expr) RBRACKET {
    {
        exp = Array (d, e);
        loc = d.loc
    }
}
| d=direct_declarator LPAR params=separated_list(COMMA, ts=type_spec d=declarator{ (ts, d) }) RPAR {
    {
        exp = Func (d, params);
        loc = d.loc
    }
}

param:
| t=type_spec d=declarator {
    { param_ts = t; param_decl = d; param_name = None; param_entry = None }
}


init:
| e=expr { { exp=ExprInitializer e; loc=e.loc } }
| token=LBRACE l=separated_list(COMMA, init) RBRACE {
    ignore token;
    { exp=ListInitializer l; loc=$startpos(token) }
}

stmt:
| token = SEMI { ignore token; { exp = Empty; loc = $startpos(token) } }
| t=type_spec decl_inits=decl_init* SEMI {
    { exp = Var {var_ts=t; var_decl_inits=decl_inits }; loc = t.loc }
}
| e=expr SEMI { { exp = Expr e; loc = e.loc } }
| token=RETURN e=expr SEMI { ignore token; { exp = Return e; loc = $startpos(token) } }
| token=IF LPAR e=expr RPAR then_stmt=stmt else_stmt=option(ELSE s=stmt {s}) {
    ignore token;
    { exp = If (e, then_stmt, else_stmt); loc = $startpos(token) }
}
| token=WHILE LPAR e=expr RPAR s=stmt {
    ignore token;
    { exp = While (e, s); loc = $startpos(token) }
}
| token=FOR LPAR init=expr? SEMI cond=expr? SEMI next=expr? RPAR s=stmt {
    ignore token;
    { exp = For (init, cond, next, s); loc = $startpos(token) }
}
| b=block { b }

block:
| token=LBRACE l=stmt* RBRACE { ignore token; { exp = Block l; loc = $startpos(token) } }

expr:
| e=assign { e }

assign:
| e=equality { e }
| l=assign token=ASSIGN r=equality { ignore token; { exp = no_type (Assign (l, r)); loc = $startpos(token) } }

equality:
| e=relational { e }
| l=equality token=EQ r=relational { ignore token; { exp = no_type (Binop{op=Eq; lhs=l; rhs=r}); loc = $startpos(token) } }
| l=equality token=NE r=relational { ignore token; { exp = no_type (Binop{op=Ne; lhs=l; rhs=r}); loc = $startpos(token) } }

relational:
| e=add { e }
| l=relational token=LT r=add { ignore token; { exp = no_type (Binop{op=Lt; lhs=l; rhs=r}); loc = $startpos(token) } }
| l=relational token=LE r=add { ignore token; { exp = no_type (Binop{op=Le; lhs=l; rhs=r}); loc = $startpos(token) } }
| l=relational token=GT r=add { ignore token; { exp = no_type (Binop{op=Lt; lhs=r; rhs=l}); loc = $startpos(token) } }
| l=relational token=GE r=add { ignore token; { exp = no_type (Binop{op=Le; lhs=r; rhs=l}); loc = $startpos(token) } }

add:
| e=mul { e }
| e=add token=PLUS m=mul { ignore token; { exp = no_type (Binop{op=Add; lhs=e; rhs=m}); loc = $startpos(token) } }
| e=add token=MINUS m=mul { ignore token; { exp = no_type (Binop{op=Sub; lhs=e; rhs=m}); loc = $startpos(token) } }

mul:
| e=unary { e }
| l=mul token=AST r=unary { ignore token; { exp = no_type (Binop{op=Mul; lhs=l; rhs=r}); loc = $startpos(token) } }
| l=mul token=SLASH r=unary { ignore token; { exp = no_type (Binop{op=Div; lhs=l; rhs=r}); loc = $startpos(token) } }

unary:
| e=term { e }
| PLUS e=term { e }
| token=MINUS e=unary {
    ignore token;
    {
        exp = no_type @@ Binop{op=Sub; lhs={ exp = no_type (Num "0"); loc = $startpos(token) }; rhs=e};
        loc = $startpos(token)
    }
}
| token=AST e=unary { ignore token; { exp = no_type (Deref e); loc = $startpos(token) } }
| token=AMP e=unary { ignore token; { exp = no_type (Addr e); loc = $startpos(token) } }
| token=SIZEOF e=unary { ignore token; { exp = no_type (Sizeof{sizeof_expr=e; sizeof_size=0}); loc = $startpos(token) } }

term:
| n=NUM { { exp = no_type (Num n); loc = $startpos(n) } }
| str=STR {
    { exp = no_type (Str (str, String_literal.add str)); loc = $startpos(str) }
}
| id=IDENT { { exp = no_type (Ident { name = id; entry = None }); loc = $startpos(id) } }
| func=IDENT LPAR l=separated_list(COMMA, expr) RPAR {
    { exp = no_type (Call (func, l)); loc = $startpos(func) }
}
| arr=term token=LBRACKET offset=expr RBRACKET {
    ignore token;
    let pointer = { exp = no_type (Binop{op=Add; lhs=arr; rhs=offset}); loc = $startpos(token) } in
    { exp = no_type (Deref pointer); loc = $startpos(token) }
}
| term=term token=ARROW field=IDENT {
    ignore token;
    { exp = no_type (Arrow (term, field)); loc = $startpos(token) }
}
| term=term token=DOT field=IDENT {
    ignore token;
    let pointer = { exp = no_type (Addr term); loc = $startpos(token) } in
    { exp = no_type (Arrow (pointer, field)); loc = $startpos(token) }
}
| LPAR e=expr RPAR { e }
| LPAR b=block RPAR { { exp = no_type (BlockExpr b); loc = b.loc } }
