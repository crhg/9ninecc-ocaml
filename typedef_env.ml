(* とりあえず雑に一度typedef名になったらずっとtypedef名としてトークナイズ *)
module S = Set.Make(String)
let map = ref S.empty

let mem name = 
    let r = S.mem name !map in
    Printf.fprintf stderr "Typedef_env.mem %s = %s\n" name (if r then "true" else "false");
    r
let add name =
    Printf.fprintf stderr "Typedef_env.add %s\n" name;
    map := S.add name !map
