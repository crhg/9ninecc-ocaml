open Misc

module Env = Map.Make(String)

type entry = 
| LocalVar of Type.t * int (* offset *)
| GlobalVar of Type.t * string (* label *)
| TypeDef of Type.t
| EnumConstant of int
[@@deriving show { with_path = false }]

let map = ref Env.empty
let tag_map = ref Env.empty
let current_tag_map = ref Env.empty

let offset = ref 0

let with_new_local_frame action =
    offset := 0;
    action();
    !offset

let with_new_scope action = 
    let saved_map = !map in
    let saved_tag_map = !tag_map in
    let saved_current_tag_map = !current_tag_map in
    current_tag_map := Env.empty;
    action();
    map := saved_map;
    tag_map := saved_tag_map;
    current_tag_map := saved_current_tag_map

let register_local_var ty name =
    let size = Type.get_size ty in
    let alignment = Type.get_alignment ty in
    offset := round_up !offset alignment + size;
    map := Env.add name (LocalVar (ty, !offset)) !map

let register_global_var ty name =
    map := Env.add name (GlobalVar (ty, name)) !map

let register_typedef ty name =
    map := Env.add name (TypeDef ty) !map

let register_enum name value =
    map := Env.add name (EnumConstant value) !map

let get_entry name = Env.find name !map

let entry_type entry = match entry with
| LocalVar (t, _) -> t
| GlobalVar (t, _) -> t
| EnumConstant _ -> Type.Int
| _ -> raise(Misc.Error("not variable: "^(show_entry entry)))

let register_tag name ty =
    (* Printf.fprintf stderr "register_tag %s %s\n" name (Type.show_type ty); *)
    tag_map := Env.add name ty !tag_map;
    current_tag_map := Env.add name ty !current_tag_map

let get_tag name =
    let r = Env.find name !tag_map in
    (* Printf.fprintf stderr "get_tag %s -> %s\n" name (Type.show_type r); *)
    r

let get_tag_opt name =
    let r = Env.find_opt name !tag_map in
    (* Printf.fprintf stderr "get_tag %s -> %s\n" name *)
        (* (match r with | None -> "None" | Some(x) -> Printf.sprintf "Some(%s)" (Type.show_type x)); *)
    r

let defined_tag_in_current_scope name =
    Env.mem name !current_tag_map

(* break/continue *)
let with_new_label prefix stack action =
    let label = Unique_id.new_id prefix in
    let saved_stack = !stack in
    stack := label :: !stack;
    action label;
    stack := saved_stack

let get_label stack =
    match !stack with
    | label :: _ -> label
    | [] -> failwith "empty"

let break_stack = ref []
let with_new_break_label action = with_new_label ".Lbreak" break_stack action
let get_break_label _ = get_label break_stack

let continue_stack = ref []
let with_new_continue_label action = with_new_label ".Lcontinue" continue_stack action
let get_continue_label _ = get_label continue_stack
