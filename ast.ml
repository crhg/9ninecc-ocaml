type decl =
| Function of string * string list * stmt

and stmt = 
| Expr of expr
| Return of expr
| If of expr * stmt * stmt option
| While of expr * stmt
| For of expr option * expr option * expr option * stmt
| Block of stmt list

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
| Call of string * expr list
| Deref of expr
| Addr of expr
[@@deriving show]
