let may f x = match x with
| Some x' -> f x'
| None    -> ()
