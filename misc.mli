val round_down : int -> int -> int
val round_up : int -> int -> int
exception Error_at of string * Lexing.position
exception Error of string
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val id : 'a -> 'a
val sum : int list -> int
