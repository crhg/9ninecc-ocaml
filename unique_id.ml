let seq = ref 0

let new_seq _ =
    incr seq;
    !seq

let new_id prefix =
    Printf.sprintf "%s%d" prefix (new_seq())
