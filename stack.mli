val set : int -> unit
val inc : int -> unit
val dec : int -> unit
val add : int -> unit
val sub : int -> unit
val push : string -> unit
val pop : string -> unit
val with_adjust : int -> (unit -> 'a) -> unit
val with_save : (unit -> 'a) -> unit
val check_no_change : (unit -> 'a) -> unit
exception Stack_changed
