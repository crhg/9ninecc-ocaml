val set : int -> unit
val inc : int -> unit
val dec : int -> unit
val add : int -> unit
val sub : int -> unit
val push : string -> unit
val pop : string -> unit
val with_adjust : int -> (unit -> unit) -> unit
val with_save : (unit -> 'a) -> 'a
val diff : (unit -> unit) -> int
exception Stack_changed
val check_no_change : (unit -> unit) -> unit
