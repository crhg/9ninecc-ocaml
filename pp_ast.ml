(** プリプロセッサ用の抽象構文木 *)

type 't node = {
    exp: 't;
    loc: Lexing.position [@opaque]
}

and pp_token_exp =
| Wsp of string
| Punct of string
| Id of string
| Str of string
| Char of string
| Num of string
| NewLine
| NewLines of int
| Eof
(* | Lpar *)
(* | Rpar *)
(* | Comma *)
| LineMarker of int * string * int option (* linemarker表示用 *)

and pp_token = pp_token_exp node
and pp_token_list = pp_token list

and group_part = 
| If of cond_r list
| DefineObject of string * pp_token list
| DefineFunction of string * string list * pp_token list
| Include of include_r (* ファイル名がマクロで定義されている場合もあるのでastの段階ではpp_token列 *)
| NonDirective of pp_token list
| Line of pp_token list
(* | DefineFunction of string list * bool * pp_token list (* true: 可変引数マクロ *) *)
(* | Undef of string *)

and include_r = {
    include_pp_tokens: pp_token list;
    include_loc: Lexing.position [@opaque]
}

and cond_r = {
    cond_expr: pp_token list;
    cond_groups: group_part list
}

and ast = group_part list
[@@deriving show {with_path = false}]
