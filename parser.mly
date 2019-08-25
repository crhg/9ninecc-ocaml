%{
    open Ast
%}

%token PLUS MINUS AST SLASH MOD AMP XOR OR LAND LOR LSHIFT RSHIFT NOT

%token PLUSPLUS MINUSMINUS

%token LT LE GT GE EQ NE

%token ASSIGN
%token PLUS_ASSIGN MINUS_ASSIGN AST_ASSIGN SLASH_ASSIGN MOD_ASSIGN AMP_ASSIGN XOR_ASSIGN OR_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN

%token DOT ARROW

%token QUESTION COLON

%token SEMI COMMA

%token LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET

%token RETURN IF ELSE WHILE FOR

%token SIZEOF

%token LONG INT SHORT CHAR STRUCT UNION ENUM TYPEDEF EXTERN

%token DUMMY // typedefで使うダミーのトークン

%token <string> NUM  // 整数トークン
%token <string> STR  // 文字列リテラル
%token <string> IDENT
%token <string> TYPEDEF_ID

%token EOF

%nonassoc IF

%type <Ast.decl list> translation_unit
%type <Ast.expr> expr_eof

%start translation_unit
%start expr_eof

%%

ident:
| ident=IDENT { (ident, $startpos(ident)) }

typedef_id:
| tid=TYPEDEF_ID { (tid, $startpos(tid)) }

id:
| id=IDENT
| id=TYPEDEF_ID { (id, $startpos(id)) }

translation_unit:
| l=decl* EOF { l }


decl:
| h=function_decl_head l=function_decl_tail {
    let (ds, d, loc) = h in
    let body = { exp = Block l; loc = loc } in

    (match ds with
    | { ds_storage_class_spec = Some { exp=Typedef; _ }; _ } ->
        raise(Misc.Error_at("typedef with body??", loc))
    | _ -> ()
    );

    {
        exp = FunctionDecl {
            func_ds = ds;
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

| ds=decl_spec decl_inits = separated_list(COMMA, decl_init) semi=SEMI {
    ignore semi;
    match ds with
    (* typedefの場合 *)
    | { ds_storage_class_spec = Some {exp=Typedef; _}; ds_type_spec = Some ts; _ } ->
        let decls = decl_inits |> List.map (fun di -> match di with
            | { di_decl = decl; di_init = None; _ } ->
                let var = Type_check.var_of_d decl in
                Typedef_env.add var;
                decl
            | { di_init = Some init; _ } ->
                raise(Misc.Error_at("typedef with init?", init.loc))
        ) in
        {
            exp = TypedefDecl(ts, decls);
            loc = $startpos(semi)
        }
    (* 通常の宣言 *)
    | _ ->
        { 
            exp = GlobalVarDecl {
                gv_ds = ds;
                gv_decl_inits = decl_inits
            };
            loc = $startpos(semi)
        }
}

decl_spec:
| ts=type_spec {
    { ds_type_spec = Some ts; ds_storage_class_spec = None }
}
| ts=type_spec ss=storage_class_spec
| ss=storage_class_spec ts=type_spec {
    { ds_type_spec = Some ts; ds_storage_class_spec = Some ss }
}

storage_class_spec:
| token=TYPEDEF { ignore token; { exp = Typedef; loc = $startpos(token) } }
| token=EXTERN { ignore token; { exp = Extern; loc = $startpos(token) } }

decl_type_spec:
| ts = type_spec {
    (* Printf.fprintf stderr "function_type_spec\n"; *)
    Typedef_env.new_scope();
    ts
}

(*  *)
function_decl_head:
| ds=decl_spec d=declarator token=LBRACE {
    ignore token;
    match d.exp with
    | Func(_, params) ->
        Typedef_env.new_scope();
        params |> List.iter (fun (_, d) ->
                    Typedef_env.remove (Type_check.var_of_d d)
                );

        (ds, d, $startpos(token))
    | _ ->
        raise(Misc.Error_at("declarator is not function", d.loc))
}

function_decl_tail:
| DUMMY l=stmt*
  midrule({
      Typedef_env.restore_scope();
  })
  DUMMY RBRACE {
    l
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
| LBRACE DUMMY l=su_field* DUMMY RBRACE { l }

su_field:
| ts=type_spec d=declarator SEMI { (ts, d) }

(* enum_list: *)
(* | LBRACE l=separated_nonempty_list(COMMA, enumarator) COMMA? RBRACE { l }  *)
enum_list:
| LBRACE DUMMY e=enumarator l=enum_list_rest { e::l }

enum_list_rest:
| DUMMY RBRACE { [] }
| COMMA DUMMY RBRACE { [] }
| COMMA e=enumarator l=enum_list_rest { e::l }

enumarator:
| id=id expr=option(ASSIGN e=constant_expression{e}) {
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
| d=direct_declarator LBRACKET e=option(assignment_expression) RBRACKET {
    {
        exp = Array (d, e);
        loc = d.loc
    }
}
(* | d=direct_declarator LPAR params=separated_list(COMMA, ts=type_spec d=declarator{ (ts, d) }) RPAR { *)
| d=function_declarator_head params=function_declarator_rest {
    {
        exp = Func (d, params);
        loc = d.loc
    }
}

function_declarator_head:
| d=direct_declarator LPAR {
    Typedef_env.new_scope();
    d
}

function_declarator_rest:
| params=separated_list(COMMA, ts=type_spec d=declarator{ (ts, d) }) RPAR {
    Typedef_env.restore_scope();
    params
}

init:
| e=assignment_expression {
    let es = make_expr_s e in
    { exp=ExprInitializer es; loc=e.loc }
}
| token=LBRACE DUMMY l=separated_list(COMMA, init) DUMMY RBRACE {
    ignore token;
    { exp=ListInitializer l; loc=$startpos(token) }
}

stmt:
| token = SEMI { ignore token; { exp = Empty; loc = $startpos(token) } }
| ds=decl_spec decl_inits=decl_init* token=SEMI {
    ignore token;
    match ds with
    (* typedefの場合 *)
    | { ds_storage_class_spec = Some {exp=Typedef; _}; ds_type_spec = Some ts; _ } ->
        let decls = decl_inits |> List.map (fun di -> match di with
            | { di_decl = decl; di_init = None; _ } ->
                let var = Type_check.var_of_d decl in
                Typedef_env.add var;
                decl
            | { di_init = Some init; _ } ->
                raise(Misc.Error_at("typedef with init?", init.loc))
        ) in
        {
            exp = TypedefStmt(ts, decls);
            loc = $startpos(token)
        }
    (* 通常の宣言 *)
    | _ ->
        {
            exp = Var {var_ds=ds; var_decl_inits=decl_inits };
            loc = $startpos(token)
        }
}
| e=expression SEMI {
    let es = make_expr_s e in
    { exp = Expr es; loc = e.loc }
}
| token=RETURN e=expression SEMI {
    ignore token;
    let e = make_expr_s e in
    { exp = Return e; loc = $startpos(token) } }
| token=IF LPAR e=expression RPAR then_stmt=stmt else_stmt=option(ELSE s=stmt {s}) {
    ignore token;
    let e = make_expr_s e in
    { exp = If (e, then_stmt, else_stmt); loc = $startpos(token) }
}
| token=WHILE LPAR e=expression RPAR s=stmt {
    ignore token;
    let e = make_expr_s e in
    { exp = While (e, s); loc = $startpos(token) }
}
| token=FOR LPAR init=expression? SEMI cond=expression? SEMI next=expression? RPAR s=stmt {
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
  DUMMY
  l=stmt*
  midrule({
      Typedef_env.restore_scope();
  })
  DUMMY
  RBRACE
{
    ignore token;
    { exp = Block l; loc = $startpos(token) }
}

expr_eof:
| e=expression EOF { e }

identifier:
| id=ident { 
    let name, loc = id in
    { exp = Ident name; loc = loc }
}

constant:
| n=NUM { { exp = Num n; loc = $startpos(n) } }

string_literal:
| str=STR {
    { exp = Str (str, String_literal.add str); loc = $startpos(str) }
}

primary_expression:
| i=identifier { i }
| c=constant { c }
| s=string_literal { s }
| LPAR e=expression RPAR { e }
| LPAR b=block RPAR { { exp = BlockExpr b; loc = b.loc } }
(* 未実装: generic_selection *)

postfix_expression:
| e=primary_expression { e }
| arr=postfix_expression token=LBRACKET index=expression RBRACKET {
    (* arr[index]は*(arr+index)に変換する *)
    ignore token;
    let pointer = { exp = Binop(Add, arr, index); loc = $startpos(token) } in
    { exp = Deref pointer; loc = $startpos(token) }
}
| func=postfix_expression LPAR params=separated_list(COMMA, p=assignment_expression { p }) RPAR {
    { exp = Call (func, params); loc = func.loc } 
}
| e=postfix_expression token=DOT field=id {
    (* e.f は (&e)->f に変換する *)
    ignore token;
    let field, _ = field in
    let pointer = { exp = Addr e; loc = $startpos(token) } in
    { exp = Arrow (pointer, field); loc = $startpos(token) }
}
| e=postfix_expression token=ARROW field=id {
    ignore token;
    let field, _ = field in
    { exp = Arrow (e, field); loc = $startpos(token) }
}
| e=postfix_expression token=PLUSPLUS {
    ignore token;
    let loc = $startpos(token) in
    let one = { exp = Num "1"; loc=loc } in
    save_and_return_l_with e (fun tmp ->
        {
            exp = Assign(
                tmp,
                { exp = Binop(Add, tmp, one); loc=loc }
            );
            loc = loc
        }
    )
}
| e=postfix_expression token=MINUSMINUS {
    ignore token;
    let loc = $startpos(token) in
    let one = { exp = Num "1"; loc=loc } in
    save_and_return_l_with e (fun tmp ->
        {
            exp = Assign(
                tmp,
                { exp = Binop(Sub, tmp, one); loc=loc }
            );
            loc = loc
        }
    )
}
(* 未実装: ( type-name ) { initializer-list } *)
(* 未実装: ( type-name ) { initializer-list , } *)

unary_expression:
| e=postfix_expression { e }
| token=PLUSPLUS e=unary_expression {
    ignore token;
    let loc = $startpos(token) in
    let one = { exp = Num "1"; loc=loc } in
    op_assign Add loc e one
}
| token=MINUSMINUS e=unary_expression {
    ignore token;
    let loc = $startpos(token) in
    let one = { exp = Num "1"; loc=loc } in
    op_assign Sub loc e one
}
| token=AMP e=cast_expression { ignore token; { exp = Addr e; loc = $startpos(token) } }
| token=AST e=cast_expression { ignore token; { exp = Deref e; loc = $startpos(token) } }
| PLUS e=cast_expression { e }
| token=MINUS e=cast_expression {
    (* -e は 0-e に変換する *)
    ignore token;
    {
        exp = Binop(Sub, { exp = Num "0"; loc = $startpos(token) }, e);
        loc = $startpos(token)
    }
}
| token=NOT e=cast_expression {
    (* !e は 0 == e に変換する *)
    ignore token;
    {
        exp = Binop(Eq, { exp = Num "0"; loc = $startpos(token) }, e);
        loc = $startpos(token)
    }
}
| token=SIZEOF e=unary_expression { ignore token; { exp = Sizeof e; loc = $startpos(token) } }
(* | token=SIZEOF LPAR t=type_name RPAR { ignore token; { exp = SizeofType t; loc = $startpos(token) } } *)
(* 未実装: _Alignof ( type-name ) *)

cast_expression:
| e=unary_expression { e }
| token=LPAR t=type_name RPAR e=cast_expression { ignore token; { exp = Cast(t, e); loc=$startpos(token) } }

multiplicative_expression:
| e=cast_expression { e }
| l=multiplicative_expression token=AST r=cast_expression {
    ignore token;
    { exp = Binop(Mul, l, r); loc = $startpos(token) }
}
| l=multiplicative_expression token=SLASH r=cast_expression {
    ignore token;
    { exp = Binop(Div, l, r); loc = $startpos(token) }
}
| l=multiplicative_expression token=MOD r=cast_expression {
    ignore token;
    { exp = Binop(Mod, l, r); loc = $startpos(token) }
}

additive_expression:
| e=multiplicative_expression { e } 
| l=additive_expression token=PLUS r=multiplicative_expression {
    ignore token;
    { exp = Binop(Add, l, r); loc = $startpos(token) }
}
| l=additive_expression token=MINUS r=multiplicative_expression {
    ignore token;
    { exp = Binop(Sub, l, r); loc = $startpos(token) }
}

shift_expression:
| e=additive_expression { e }
| l=shift_expression token=LSHIFT r=additive_expression {
    ignore token;
    { exp = Binop(LShift, l, r); loc = $startpos(token) }
}
| l=shift_expression token=RSHIFT r=additive_expression {
    ignore token;
    { exp = Binop(RShift, l, r); loc = $startpos(token) }
}

relational_expression:
| e=shift_expression { e }
| l=relational_expression token=LT r=shift_expression {
    ignore token;
    { exp = Binop(Lt, l, r); loc = $startpos(token) }
}
| l=relational_expression token=GT r=shift_expression {
    ignore token;
    { exp = Binop(Lt, r, l); loc = $startpos(token) }
}
| l=relational_expression token=LE r=shift_expression {
    ignore token;
    { exp = Binop(Le, l, r); loc = $startpos(token) }
}
| l=relational_expression token=GE r=shift_expression {
    ignore token;
    { exp = Binop(Le, r, l); loc = $startpos(token) }
}

equality_expression:
| e=relational_expression { e }
| l=equality_expression token=EQ r=relational_expression {
    ignore token;
    { exp = Binop(Eq, l, r); loc = $startpos(token) }
}
| l=equality_expression token=NE r=relational_expression {
    ignore token;
    { exp = Binop(Ne, l, r); loc = $startpos(token) }
}

and_expression:
| e=equality_expression { e }
| l=and_expression token=AMP r=equality_expression {
    ignore token;
    { exp = Binop(BitAnd, l, r); loc = $startpos(token) }
}

exclusive_or_expression:
| e=and_expression { e }
| l=exclusive_or_expression token=XOR r=and_expression {
    ignore token;
    { exp = Binop(BitXor, l, r); loc = $startpos(token) }
}

inclusive_or_expression:
| e=exclusive_or_expression { e }
| l=inclusive_or_expression token=OR r=exclusive_or_expression {
    ignore token;
    { exp = Binop(BitOr, l, r); loc = $startpos(token) }
}

logical_and_expression:
| e=inclusive_or_expression { e }
| l=logical_and_expression token=LAND r=inclusive_or_expression {
    ignore token;
    { exp = Binop(LAnd, l, r); loc = $startpos(token) }
}

logical_or_expression:
| e=logical_and_expression { e }
| l=logical_or_expression token=LOR r=logical_and_expression {
    ignore token;
    { exp = Binop(LOr, l, r); loc = $startpos(token) }
}

conditional_expression:
| e=logical_or_expression { e }
| cond=logical_or_expression token=QUESTION then_expr=expression COLON else_expr=conditional_expression {
    ignore token;
    { exp = Cond(cond, then_expr, else_expr); loc=$startpos(token) }
}

assignment_expression:
| e=conditional_expression { e }
| l=unary_expression token=ASSIGN r=assignment_expression {
    ignore token;
    { exp = Assign (l, r); loc = $startpos(token) }
}
| l=unary_expression op=binop_assign r=assignment_expression {
    let op, loc = op in
    Ast.op_assign op loc l r
}

binop_assign:
| token=AST_ASSIGN { ignore token; Mul, $startpos(token) } 
| token=SLASH_ASSIGN { ignore token; Div, $startpos(token) } 
| token=PLUS_ASSIGN { ignore token; Add, $startpos(token) } 
| token=MINUS_ASSIGN { ignore token; Sub, $startpos(token) } 
| token=MOD_ASSIGN { ignore token; Mod, $startpos(token) } 
| token=AMP_ASSIGN { ignore token; BitAnd, $startpos(token) } 
| token=XOR_ASSIGN { ignore token; BitXor, $startpos(token) } 
| token=OR_ASSIGN { ignore token; BitOr, $startpos(token) } 
| token=LSHIFT_ASSIGN { ignore token; LShift, $startpos(token) } 
| token=RSHIFT_ASSIGN { ignore token; RShift, $startpos(token) } 

expression:
| e=assignment_expression { e }
(* 未実装: expression , assignment-expression *)

constant_expression:
| e=conditional_expression { e }

type_name:
| ts=specifier_qualifier_list d=abstract_declarator? {
    let dummy = { exp = DeclIdent "**DUMMY**"; loc = ts.loc } in
    {
        exp = {
            type_name_ts = ts;
            type_name_decl = Option.may_apply d dummy
        };
        loc = ts.loc
    }
}

(* abstract_declaratorの値はdeclarator->declaratorで表す *)
abstract_declarator:
| d=pointer { d }
| d=direct_abstract_declarator { d }
| p=pointer d=direct_abstract_declarator  {
    Misc.compose p d
}

pointer:
| token=AST d=pointer? { 
    ignore token;
    let loc = $startpos(token) in
    let f x = { exp = PointerOf x; loc = loc } in
    Option.compose (Some f) d
}

direct_abstract_declarator:
| LPAR d=abstract_declarator RPAR { d }
| d=direct_abstract_declarator? token=LBRACKET size=assignment_expression? RBRACKET {
    ignore token;
    let loc = $startpos(token) in
    let f x = { exp = Array(x, size); loc=loc } in
    Option.compose (Some f) d
}
| d=direct_abstract_declarator? token=LPAR
  midrule({ Typedef_env.new_scope(); })
  params=separated_list(COMMA, ts=type_spec d=declarator{ (ts, d) })
  midrule({ Typedef_env.restore_scope(); })
  RPAR {
    ignore token;
    let f x = { exp = Func(x, params); loc=x.loc } in
    Option.compose (Some f) d
}

specifier_qualifier_list:
| ts=type_spec { ts }
(* XXX: type_specがリストになっているのは unsigned short int のようなものを任意の順序で書けるからだが今のところサポートしない *)
(* | type_spec specifier_qualifier_list? *)
(* XXX: 今のところ type_qualifierはサポートしていない *)
(* | type_qualifier specifier_qualifier_list? *)


