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

let default x default_value = match x with
| Some x -> x
| None -> default_value

let default_lazy x default_value = match x with
| Some x -> x
| None -> Lazy.force default_value

let may_apply f x = match f with
| Some f -> f x
| None -> x

let may_compose f g = match f, g with
| Some f, Some g -> Misc.compose f g
| Some f, None -> f
| None, Some g -> g
| None, None -> Misc.id
