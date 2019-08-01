type t =
| Int
| Ptr of t
[@@deriving show]

let rec get_size ty = match ty with
| Int -> 4
| Ptr _ -> 8

and get_alignment ty = match ty with
| Int -> 4
| Ptr _ -> 8
