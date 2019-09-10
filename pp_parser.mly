%{
    open Pp_ast
%}

%token SHARP
%token SHARP_DEFINE SHARP_INCLUDE
%token SHARP_IF SHARP_IFDEF SHARP_IFNDEF SHARP_ELIF SHARP_ELSE SHARP_ENDIF
%token DEFINE INCLUDE IF IFDEF IFNDEF ELIF ELSE ENDIF
%token <Pp_token.token> SHARP_NON_DIRECTIVE

%token LPAR RPAR COMMA

%token LINE NL

%token <string> ID
%token <string> NUM
%token <string> STR
%token <string> CHAR
%token <string> PUNCT
%token <string> WSP

%token EOF

%type <Pp_ast.group_part list> preprocessing_file
%type <unit> dummy

%start preprocessing_file
%start dummy

%%

preprocessing_file:
| l=list(group_part) EOF { l }

group_part:
| p=define_object { p }
| p=define_function { p }
| p=include_file { p }
| p=non_directive { p }
| p=if_part { If p }
| p=ifdef_part { If p }
| p=ifndef_part { If p }
| p=line { p }

group_parts:
| { [] }
| p=group_part l=group_parts {
    (* Printf.fprintf stderr "group_parts\n"; *)
    p::l
}

if_part:
| p=if_line l=group_parts rest=elif_part {
    let cond = { cond_expr = p; cond_groups = l } in
    cond :: rest 
}

ifdef_part:
| p=ifdef_line l=group_parts rest=elif_part {
    let cond = { cond_expr = p; cond_groups = l } in
    cond :: rest 
}

ifndef_part:
| p=ifndef_line l=group_parts rest=elif_part {
    let cond = { cond_expr = p; cond_groups = l } in
    cond :: rest 
}

elif_part:
| p=else_part { p }
| p=elif_line l=group_parts rest=elif_part {
    let cond = { cond_expr = p; cond_groups = l } in
    cond :: rest
}

else_part:
| p=endif_part { p }
| p=else_line l=group_parts rest=endif_part {
    let cond = { cond_expr = p; cond_groups = l } in
    cond :: rest
}

endif_part:
| endif_line { [] }

if_line:
| SHARP_IF l=pp_tokens NL { l }

ifdef_line:
| SHARP_IFDEF id=ID NL { [Punct "("; Id "defined"; Id id; Punct ")"] }

ifndef_line:
| SHARP_IFNDEF id=ID NL { [Punct "("; Num "0"; Punct "=="; Id "defined"; Id id; Punct ")"] }

elif_line:
| SHARP_ELIF l=pp_tokens NL { l }

else_line:
| SHARP_ELSE NL { [Num "1"] }

endif_line:
| SHARP_ENDIF NL {
    (* Printf.fprintf stderr "endif_line\n" *)
}

define_object:
| SHARP_DEFINE id=ID WSP+ l=pp_tokens NL { DefineObject(id, l) }
| SHARP_DEFINE id=ID WSP* NL { DefineObject(id, []) }

define_function:
| SHARP_DEFINE id=ID LPAR params=separated_list(COMMA, p=param {p}) RPAR WSP* l=pp_tokens NL {
    DefineFunction(id, params, l)
}

include_file:
| token=SHARP_INCLUDE WSP* l=pp_tokens NL {
    ignore token;
    Include { pp_tokens = l; loc = $startpos(token)}
}

non_directive:
| SHARP_NON_DIRECTIVE l=pp_tokens NL { NonDirective(l) }

line:
| LINE l=wsp_or_pp_tokens NL { Line(l @ [NewLine]) }

param:
| WSP* id=ID WSP* { id }

wsp_or_pp_tokens:
| { [] }
| wsp=wsp l=wsp_or_pp_tokens { wsp :: l }
| t=pp_token l=wsp_or_pp_tokens { t :: l }

pp_tokens:
| { [] }
| t=pp_token l=pp_tokens_rest { t::l }

pp_tokens_rest:
| { [] }
| wsp=WSP l=pp_tokens_rest { (Wsp wsp)::l }
| t=pp_token l=pp_tokens_rest { t::l }

pp_token:
| t=not_sharp { t }
| SHARP { Punct "#" }

not_sharp:
| p=PUNCT { Punct p }
| id=ID { Id id }
| str=STR { Str str }
| c=CHAR { Char c }
| num=NUM { Num num }
| LPAR { Punct "(" }
| RPAR { Punct ")" }
| COMMA { Punct "," }

wsp:
| wsp=WSP { Wsp wsp }

dummy:
|DEFINE|INCLUDE|IF|IFDEF|IFNDEF|ELIF|ELSE|ENDIF {()}
