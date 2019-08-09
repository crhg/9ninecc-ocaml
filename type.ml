type t =
| Int
| Char
| Ptr of t
| Array of t * int option
| Function of t * (t * string) list (* 戻り値とパラメタ *)
[@@deriving show]

let rec get_size ty = match ty with
| Int -> 4
| Char -> 1
| Ptr _ -> 8
| Array (t, Some n) -> get_size t * n
| _ -> failwith @@ "size?: " ^ show ty

and get_alignment ty = match ty with
| Int -> 4
| Char -> 1
| Ptr _ -> 8
| Array (t, _) -> get_alignment t
| _ -> failwith @@ "alignment?: " ^ show ty

and is_complete_type ty = match ty with
| Int
| Char
| Ptr _ ->
    true
| Array (t, Some _) ->
    is_complete_type t
| _ ->
    false
