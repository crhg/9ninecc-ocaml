type t =
| Int
| Char
| Ptr of t
| Array of t * int option
| Function of t * (string * t) list (* 戻り値とパラメタ *)
| Struct of st_un

and field = { field_name : string; field_type: t; field_offset: int }

(* 構造体・共用体の型の共通の中身 *)
and st_un_exp = {
    fields: field list;
    size: int;
    alignment: int
}
and st_un = { mutable exp: st_un_exp option }
[@@deriving show]

let rec get_size ty = match ty with
| Int -> 4
| Char -> 1
| Ptr _ -> 8
| Array (t, Some n) -> get_size t * n
| Struct {exp=Some{size=size}} -> size
| _ -> failwith @@ "size?: " ^ show ty

and get_alignment ty = match ty with
| Int -> 4
| Char -> 1
| Ptr _ -> 8
| Array (t, _) -> get_alignment t
| Struct {exp=Some{alignment=alignment}} -> alignment
| _ -> failwith @@ "alignment?: " ^ show ty

and is_complete_type ty = match ty with
| Int
| Char
| Ptr _
| Struct {exp=Some _} ->
    true
| Array (t, Some _) ->
    is_complete_type t
| _ ->
    false

and get_field ty field_name = match ty with
| Struct { exp = Some { fields = fields } } ->
    List.find (fun { field_name = name } -> name = field_name) fields
| _ -> failwith ("get_field ty? " ^ (show ty))
