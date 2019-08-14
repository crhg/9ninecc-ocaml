(* nをmの倍数に切り捨てる *)
let round_down n m = n - (n mod m)

(* nをmの倍数に切り上げる *)
let round_up n m = round_down (n + m - 1) m

(* ソース位置を持った例外 *)
exception Error_at of string * Lexing.position
exception Error of string

(* 関数合成 *)
let compose f g x = f (g x)
