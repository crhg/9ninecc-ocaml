val may : ('a -> unit) -> 'a option -> unit
val round_down : int -> int -> int
val round_up : int -> int -> int
exception Error_at of string * Lexing.position
