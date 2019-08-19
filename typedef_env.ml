(* とりあえず雑に一度typedef名になったらずっとtypedef名としてトークナイズ *)
module S = Set.Make(String)
let map = ref S.empty
let map_stack = ref []

let mem name = 
    let r = S.mem name !map in
    (* Printf.fprintf stderr "Typedef_env.mem %s = %s\n" name (if r then "true" else "false"); *)
    r

let add name =
    Printf.fprintf stderr "Typedef_env.add %s\n" name;
    map := S.add name !map

let remove name =
    Printf.fprintf stderr "Typedef_env.remove %s\n" name;
    map := S.remove name !map

let new_scope _ =
    (* Printf.fprintf stderr "Typedef_env.new_scope\n"; *)
    map_stack := !map :: !map_stack

let restore_scope _ =
    (* Printf.fprintf stderr "Typedef_env.restore_scope\n"; *)
    match !map_stack with
    | top::rest ->
        map := top;
        map_stack := rest;
    | _ ->
        raise(Misc.Error("typedef_env: stack is empty"))

let overwrite_scope _ =
    (* Printf.fprintf stderr "Typedef_env.restore_scope\n"; *)
    match !map_stack with
    | _::rest ->
        map_stack := rest;
    | _ ->
        raise(Misc.Error("typedef_env: stack is empty"))
