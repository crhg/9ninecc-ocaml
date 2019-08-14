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

%token LONG INT SHORT CHAR STRUCT UNION ENUM TYPEDEF

%token DUMMY // typedefで使うダミーのトークン

%token <string> NUM  // 整数トークン
%token <string> STR  // 文字列リテラル
%token <string> IDENT
%token <string> TYPEDEF_ID

%token EOF

%type <Ast.decl list> translation_unit

%start translation_unit

%%

ident:
| DUMMY ident=IDENT { (ident, $startpos(ident)) }

typedef_id:
| DUMMY tid=TYPEDEF_ID { (tid, $startpos(tid)) }

id:
| DUMMY id=IDENT
| DUMMY id=TYPEDEF_ID { (id, $startpos(id)) }

translation_unit:
| l=decl* EOF { l }

decl_type_spec:
| ts = type_spec {
    (* Printf.fprintf stderr "function_type_spec\n"; *)
    Typedef_env.new_scope();
    ts
}

decl:
| t=decl_type_spec d=declarator body=block
{
    Typedef_env.overwrite_scope();

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
| t=decl_type_spec decl_inits = separated_list(COMMA, decl_init) SEMI {
    Typedef_env.restore_scope();
    { 
        exp = GlobalVarDecl {
            gv_ts = t;
            gv_decl_inits = decl_inits
        };
        loc = t.loc
    }
}
| typedef=typedef {
    let (ts, decl, loc) = typedef in
    { exp = TypedefDecl (ts, decl); loc = loc }
}

typedef:
| token=TYPEDEF ts=type_spec decl=declarator SEMI {
    ignore token;
    let _, name = Type_check.type_and_var_ty Type.Int decl in
    Typedef_env.add name;
    (ts, decl, $startpos(token))
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
| token=STRUCT tag=id fields=option(su_body) {
    ignore token; 
    let tag, _ = tag in
    { exp = Struct { su_tag = Some tag; su_fields = fields }; loc = $startpos(token) }
}
| token=UNION fields=su_body {
    ignore token; 
    { exp = Union { su_tag = None; su_fields = Some fields }; loc = $startpos(token) }
}
| token=UNION tag=id fields=option(su_body) {
    ignore token; 
    let tag, _ = tag in
    { exp = Union { su_tag = Some tag; su_fields = fields }; loc = $startpos(token) }
}
| token=ENUM enum_list=enum_list {
    ignore token;
    { exp = Enum { enum_tag = None; enum_list = Some enum_list }; loc = $startpos(token) }
}
| token=ENUM tag=id enum_list=option(enum_list) {
    ignore token;
    let tag, _ = tag in
    { exp = Enum { enum_tag = Some tag; enum_list = enum_list }; loc = $startpos(token) }
}
| typedef_id=typedef_id {
    let name, loc = typedef_id in
    { exp = Type name; loc = loc }
}

su_body:
| LBRACE l=su_field* RBRACE { l }

su_field:
| ts=type_spec d=declarator SEMI { (ts, d) }

enum_list:
| LBRACE l=separated_list(COMMA, enumarator) COMMA? RBRACE { l } 

enumarator:
| id=id expr=option(ASSIGN e=expr{e}) {
    let name, loc = id in
    let expr = Option.map make_expr_s expr in
    {
        exp = { en_name = name; en_expr = expr };
        loc = loc
    }
}


decl_init:
| decl=declarator init=option(ASSIGN i=init {i}) {
    { di_decl = decl; di_init = init; di_entry = None; di_init_assign = [] }
}

declarator:
| d=direct_declarator { d }
| token=AST d=declarator { ignore token; { exp = PointerOf d; loc = d.loc } }

direct_declarator:
| id=id { 
    let name, loc = id in

    (* declaratorに出現したらとりあえずtypedef名でない方に倒す *)
    (* typedef名だった場合はあとで登録される *)
    (* 同一スコープの再定義禁止チェックは型チェックのときに行うのでここでは気にしない *)
    Typedef_env.remove name;

    { exp = DeclIdent name; loc = loc }
}
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
| e=expr {
    let es = make_expr_s e in
    { exp=ExprInitializer es; loc=e.loc }
}
| token=LBRACE l=separated_list(COMMA, init) RBRACE {
    ignore token;
    { exp=ListInitializer l; loc=$startpos(token) }
}

stmt:
| token = SEMI { ignore token; { exp = Empty; loc = $startpos(token) } }
| typedef=typedef {
    let (ts, decl, loc) = typedef in
    { exp = Typedef (ts, decl); loc = loc }
}
| t=type_spec decl_inits=decl_init* SEMI {
    { exp = Var {var_ts=t; var_decl_inits=decl_inits }; loc = t.loc }
}
| e=expr SEMI {
    let es = make_expr_s e in
    { exp = Expr es; loc = e.loc }
}
| token=RETURN e=expr SEMI {
    ignore token;
    let e = make_expr_s e in
    { exp = Return e; loc = $startpos(token) } }
| token=IF LPAR e=expr RPAR then_stmt=stmt else_stmt=option(ELSE s=stmt {s}) {
    ignore token;
    let e = make_expr_s e in
    { exp = If (e, then_stmt, else_stmt); loc = $startpos(token) }
}
| token=WHILE LPAR e=expr RPAR s=stmt {
    ignore token;
    let e = make_expr_s e in
    { exp = While (e, s); loc = $startpos(token) }
}
| token=FOR LPAR init=expr? SEMI cond=expr? SEMI next=expr? RPAR s=stmt {
    ignore token;
    let init = Option.map make_expr_s init in
    let cond = Option.map make_expr_s cond in
    let next = Option.map make_expr_s next in
    { exp = For (init, cond, next, s); loc = $startpos(token) }
}
| b=block { b }

block:
| token=LBRACE 
  midrule({
      (* Printf.fprintf stderr "block new_scope\n"; *)
      Typedef_env.new_scope()
  })
  l=stmt*
  RBRACE
{
    ignore token;
    Typedef_env.restore_scope();
    { exp = Block l; loc = $startpos(token) }
}

expr:
| e=assign { e }

assign:
| e=equality { e }
| l=assign token=ASSIGN r=equality { ignore token; { exp = Assign { assign_lhs = l; assign_rhs = r }; loc = $startpos(token) } }

equality:
| e=relational { e }
| l=equality token=EQ r=relational { ignore token; { exp = Binop{op=Eq; lhs=l; rhs=r}; loc = $startpos(token) } }
| l=equality token=NE r=relational { ignore token; { exp = Binop{op=Ne; lhs=l; rhs=r}; loc = $startpos(token) } }

relational:
| e=add { e }
| l=relational token=LT r=add { ignore token; { exp = Binop{op=Lt; lhs=l; rhs=r}; loc = $startpos(token) } }
| l=relational token=LE r=add { ignore token; { exp = Binop{op=Le; lhs=l; rhs=r}; loc = $startpos(token) } }
| l=relational token=GT r=add { ignore token; { exp = Binop{op=Lt; lhs=r; rhs=l}; loc = $startpos(token) } }
| l=relational token=GE r=add { ignore token; { exp = Binop{op=Le; lhs=r; rhs=l}; loc = $startpos(token) } }

add:
| e=mul { e }
| e=add token=PLUS m=mul { ignore token; { exp = Binop{op=Add; lhs=e; rhs=m}; loc = $startpos(token) } }
| e=add token=MINUS m=mul { ignore token; { exp = Binop{op=Sub; lhs=e; rhs=m}; loc = $startpos(token) } }

mul:
| e=unary { e }
| l=mul token=AST r=unary { ignore token; { exp = Binop{op=Mul; lhs=l; rhs=r}; loc = $startpos(token) } }
| l=mul token=SLASH r=unary { ignore token; { exp = Binop{op=Div; lhs=l; rhs=r}; loc = $startpos(token) } }

unary:
| e=term { e }
| PLUS e=term { e }
| token=MINUS e=unary {
    ignore token;
    {
        exp = Binop{op=Sub; lhs={ exp = Num "0"; loc = $startpos(token) }; rhs=e};
        loc = $startpos(token)
    }
}
| token=AST e=unary { ignore token; { exp = Deref { deref_expr = e }; loc = $startpos(token) } }
| token=AMP e=unary { ignore token; { exp = Addr e; loc = $startpos(token) } }
| token=SIZEOF e=unary { ignore token; { exp = Sizeof{sizeof_expr=e; sizeof_size=0}; loc = $startpos(token) } }

term:
| n=NUM { { exp = Num n; loc = $startpos(n) } }
| str=STR {
    { exp = Str (str, String_literal.add str); loc = $startpos(str) }
}
| id=ident { 
    let name, loc = id in
    { exp = Ident { name = name }; loc = loc }
}
| func=ident LPAR l=separated_list(COMMA, expr) RPAR {
    let func, loc = func in
    { exp = Call (func, l); loc = loc } 
}
| arr=term token=LBRACKET offset=expr RBRACKET {
    ignore token;
    let pointer = { exp = Binop{op=Add; lhs=arr; rhs=offset}; loc = $startpos(token) } in
    { exp = Deref { deref_expr=pointer }; loc = $startpos(token) }
}
| term=term token=ARROW field=id {
    ignore token;
    let field, _ = field in
    { exp = Arrow { arrow_expr = term; arrow_field = field }; loc = $startpos(token) }
}
| term=term token=DOT field=id {
    ignore token;
    let field, _ = field in
    let pointer = { exp = Addr term; loc = $startpos(token) } in
    { exp = Arrow { arrow_expr = pointer; arrow_field = field }; loc = $startpos(token) }
}
| LPAR e=expr RPAR { e }
| LPAR b=block RPAR { { exp = BlockExpr b; loc = b.loc } }
