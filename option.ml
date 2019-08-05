exception No_value

let get opt = match opt with
| Some x -> x
| None -> raise No_value

let map f opt = match opt with
| Some x -> Some (f x)
| None -> None
