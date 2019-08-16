%{
    open Pp_ast
%}

%token DEFINE

%token SHARP LPAR RPAR COMMA

%token NL

%token <string> ID
%token <string> NUM
%token <string> STR
%token <string> PUNCT
%token <string> WSP

%token EOF

%type <Pp_ast.group_part list> preprocessing_file

%start preprocessing_file

%%

preprocessing_file:
| l=list(group_part) EOF { l }

group_part:
| wsp* SHARP WSP* DEFINE WSP+ id=ID WSP+ l=pp_tokens NL { DefineObject(id, l) }
| wsp* SHARP WSP* DEFINE WSP+ id=ID LPAR params=separated_list(COMMA, p=param {p}) RPAR WSP* l=pp_tokens NL {
    DefineFunction(id, params, l)
}
| wsp* SHARP WSP* l=pp_tokens NL { NonDirective(l) }
| wsps1=wsp* not_sharp=not_sharp wsps2=wsp* l=pp_tokens NL { Line(wsps1 @ [not_sharp] @ wsps2 @ l @ [NewLine]) }
| wsps=wsp* NL { Line(wsps) } (* 空白だけの行 *)

param:
| WSP* id=ID WSP* { id }

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
| num=NUM { Num num }
| LPAR { Punct "(" }
| RPAR { Punct ")" }
| COMMA { Punct "," }

wsp:
| wsp=WSP { Wsp wsp }
