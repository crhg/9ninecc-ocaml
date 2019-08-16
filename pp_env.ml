module Env = Map.Make(String)

type entry = 
| ObjectMacro of Pp_ast.pp_token list
| FunctionMacro of string list * Pp_ast.pp_token list

and t = entry Env.t ref

let make _ = ref Env.empty

let mem name env = Env.mem name !env

let find name env = Env.find name !env
let find_opt name env = Env.find_opt name !env

let add name entry env =
    env := Env.add name entry !env

let remove name env =
    env := Env.remove name !env

