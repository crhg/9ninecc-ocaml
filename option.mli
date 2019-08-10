exception No_value
val get : 'a option -> 'a
val map : ('a -> 'b) -> 'a option -> 'b option
val is_none : 'a option -> bool
val is_some : 'a option -> bool
val may : ('a -> unit) -> 'a option -> unit
