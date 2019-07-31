type stmt = 
Expr of expr

and expr =
| Num of string
| Ident of string
| Add of expr * expr
| Sub of expr * expr
| Mul of expr * expr
| Div of expr * expr
| Lt of expr * expr
| Le of expr * expr
| Eq of expr * expr
| Ne of expr * expr
| Assign of expr * expr
[@@deriving show]
