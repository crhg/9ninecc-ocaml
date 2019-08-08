let seq = ref 0

let new_seq _ =
    seq := !seq + 1;
    !seq

let new_id prefix =
    Printf.sprintf "%s%d" prefix (new_seq())
