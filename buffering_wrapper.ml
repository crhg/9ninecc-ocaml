let make _ =
    ref []

and next buf action =
    match !buf with
    | x::rest ->
        buf := rest;
        x
    | [] ->
        action buf

and push buf x =
    buf := x :: !buf

and push_list buf l =
    buf := l @ !buf

