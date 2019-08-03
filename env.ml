open Misc

module Env = Map.Make(String)

type entry = 
| DummyEntry
| LocalVar of Type.t * int
| GlobalVar of Type.t * string
[@@deriving show]

let local_env = ref Env.empty
let global_env = ref Env.empty

let offset = ref 0

let reset _ =
    local_env := Env.empty;
    offset := 0

exception DuplicatedLocal of Type.t * string

let register_local_var ty name =
    if Env.mem name !local_env then
        raise(DuplicatedLocal(ty,name))
    else
        let size = Type.get_size ty in
        let alignment = Type.get_alignment ty in
        offset := round_up !offset alignment + size;
        local_env := Env.add name (LocalVar (ty, !offset)) !local_env

exception DuplicatedGlobal of Type.t * string

let register_global_var ty name =
    if Env.mem name !global_env then
        raise(DuplicatedGlobal(ty,name))
    else
        global_env := Env.add name (GlobalVar (ty, name)) !global_env

let local_var_size _ = !offset

let get_entry name =
    try Env.find name !local_env with
    | Not_found ->
        Env.find name !global_env

let entry_type entry = match entry with
| LocalVar (t, _) -> t
| GlobalVar (t, _) -> t
