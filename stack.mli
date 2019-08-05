val sp : int ref
val reset : 'a -> unit
val add : int -> unit
val sub : int -> unit
val push : string -> unit
val pop : string -> unit
val with_adjust : int -> (unit -> 'a) -> unit
val with_save : (unit -> 'a) -> unit
