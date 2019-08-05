val get_line : string -> int -> string * int * int
val repeat : int -> string -> string
val compile : string -> string -> unit
val compile_file : string -> unit
val verbose : bool ref
val filenames : string list ref
val spec : (string * Arg.spec * string) list
