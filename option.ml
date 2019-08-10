exception No_value

let get opt = match opt with
| Some x -> x
| None -> raise No_value

let map f opt = match opt with
| Some x -> Some (f x)
| None -> None

let is_none opt = match opt with
| None -> true
| Some _ -> false

let is_some opt = not (is_none opt)

let may f x = match x with
| Some x' -> f x'
| None    -> ()

