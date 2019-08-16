module Env = Map.Make(String)

type entry = 
| ObjectMacro of Pp_ast.pp_token list
| FunctionMacro of string list * Pp_ast.pp_token list
[@@deriving show { with_path = false }]

let map = ref Env.empty

let mem name = Env.mem name !map

let find name = Env.find name !map
let find_opt name = Env.find_opt name !map

let add name entry =
    map := Env.add name entry !map

let remove name =
    map := Env.remove name !map

