(** プリプロセッサ用の抽象構文木 *)

type pp_token =
| Wsp of string
| Punct of string
| Id of string
| Str of string
| Num of string
| NewLine
| NewLines of int
| Eof
(* | Lpar *)
(* | Rpar *)
(* | Comma *)
| LineMarker of int * string * int option (* linemarker表示用 *)

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
    pp_tokens: pp_token list;
    loc: Lexing.position [@opaque]
}

and cond_r = {
    cond_expr: pp_token list;
    cond_groups: group_part list
}

and ast = group_part list
[@@deriving show {with_path = false}]
