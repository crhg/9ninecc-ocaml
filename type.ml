module SS = Set.Make(String)

let shown = ref SS.empty

type t =
| Int
| Char
| Ptr of t
| Array of t * int option
| Function of t * (string * t) list (* 戻り値とパラメタ *)
| Struct of aggregate 
    [@printer fun fmt aggregate -> match aggregate with
        | {id=id} when SS.mem id !shown ->
            fprintf fmt "Struct{id=%s;...}" id
        | {id=id} ->
            shown := SS.add id !shown;
            fprintf fmt "Struct ";
            pp_aggregate fmt  aggregate ]
| Union of aggregate 
    [@printer fun fmt aggregate -> match aggregate with
        | {id=id} when SS.mem id !shown ->
            fprintf fmt "Union{id=%s;...}" id
        | {id=id} ->
            shown := SS.add id !shown;
            fprintf fmt "Union ";
            pp_aggregate fmt  aggregate ]

and aggregate = {
    id: string; (* 識別用 *)
    tag: string option; (* 表示用 *)
    mutable body: body option
}

and field = {
    field_name: string;
    field_type: t;
    field_offset: int
}

(* 構造体・共用体の型の共通の中身 *)
and body = {
    fields: field list;
    size: int;
    alignment: int
}
[@@deriving show { with_path = false }]

let show_type ty =
    shown := SS.empty;
    show ty

exception Incomplete

let rec get_size ty = match ty with
| Int -> 4
| Char -> 1
| Ptr _ -> 8
| Array (t, Some n) -> get_size t * n
| Struct {body=Some{size=size}} -> size
| _ -> raise Incomplete

and get_alignment ty = match ty with
| Int -> 4
| Char -> 1
| Ptr _ -> 8
| Array (t, _) -> get_alignment t
| Struct {body=Some{alignment=alignment}} -> alignment
| _ -> raise Incomplete

and is_complete_type ty = match ty with
| Int
| Char
| Ptr _
| Struct {body=Some _} ->
    true
| Array (t, Some _) ->
    is_complete_type t
| _ ->
    false

and get_field ty field_name = match ty with
| Struct { body = Some { fields = fields } } ->
    List.find (fun { field_name = name } -> name = field_name) fields
| Struct _ ->
    raise Incomplete
| _ -> failwith ("get_field ty? " ^ (show ty))

