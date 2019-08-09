open Misc

module Env = Map.Make(String)

type entry = 
| LocalVar of Type.t * int (* offset *)
| GlobalVar of Type.t * string (* label *)
[@@deriving show]

let map = ref Env.empty
let tag_map = ref Env.empty

let offset = ref 0

let with_new_local_frame action =
    offset := 0;
    action();
    !offset

let with_new_scope action = 
    let saved_map = !map in
    let saved_tag_map = !tag_map in
    action();
    map := saved_map;
    tag_map := saved_tag_map

let register_local_var ty name =
    let size = Type.get_size ty in
    let alignment = Type.get_alignment ty in
    offset := round_up !offset alignment + size;
    map := Env.add name (LocalVar (ty, !offset)) !map

let register_global_var ty name =
    map := Env.add name (GlobalVar (ty, name)) !map

let get_entry name = Env.find name !map

let entry_type entry = match entry with
| LocalVar (t, _) -> t
| GlobalVar (t, _) -> t

let register_tag name ty =
    tag_map := Env.add name ty !tag_map

let get_tag name = Env.find name !tag_map

let get_tag_opt name = Env.find_opt name !tag_map
