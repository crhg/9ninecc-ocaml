(** プリプロセッサ用の抽象構文木 *)

type pp_token =
| Wsp of string
| Punct of string
| Id of string
| Str of string
| Num of string
| NewLine
| Eof
(* | Lpar *)
(* | Rpar *)
(* | Comma *)
(* | If of expr * pp_token list * pp_token list *)
(* | Ifdef of string * pp_token list * pp_token list *)
(* | Ifndef of string * pp_token list * pp_token list *)
(* | Include of string * bool (* true: <>, false: "" *) *)

and group_part = 
| DefineObject of string * pp_token list
| DefineFunction of string * string list * pp_token list
| NonDirective of pp_token list
| Line of pp_token list
(* | DefineFunction of string list * bool * pp_token list (* true: 可変引数マクロ *) *)
(* | Undef of string *)
[@@deriving show]
