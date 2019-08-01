type t =
| Int
| Ptr of t
| Array of t * int
[@@deriving show]

let rec get_size ty = match ty with
| Int -> 4
| Ptr _ -> 8
| Array (t, n) -> get_size t * n

and get_alignment ty = match ty with
| Int -> 4
| Ptr _ -> 8
| Array (t, _) -> get_alignment t
