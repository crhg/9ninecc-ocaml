module SS = Set.Make(String)

let shown = ref SS.empty

type t =
| Void
| Long
| Int
| Short
| Char
| Bool
| Ptr of t
| Array of t * int option
    [@printer fun fmt (ty, size) -> match size with
        | None      -> fprintf fmt "Array(%s)[?]" (show ty)
        | Some size -> fprintf fmt "Array(%s)[%d]" (show ty) size ]
| Function of function_r
| Struct of aggregate 
    [@printer fun fmt aggregate -> match aggregate with
        | {id=id;_} when SS.mem id !shown ->
            fprintf fmt "Struct{id=%s;...}" id
        | {id=id;_} ->
            shown := SS.add id !shown;
            fprintf fmt "Struct ";
            pp_aggregate fmt  aggregate ]
| Union of aggregate 
    [@printer fun fmt aggregate -> match aggregate with
        | {id=id;_} when SS.mem id !shown ->
            fprintf fmt "Union{id=%s;...}" id
        | {id=id;_} ->
            shown := SS.add id !shown;
            fprintf fmt "Union ";
            pp_aggregate fmt  aggregate ]

and function_r = {
    function_ret_type: t;
    function_params: (string * t) list;
    function_has_varargs: bool
}

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
exception NoSize

let rec get_size ty = match ty with
| Void -> 1
| Long -> 8
| Int -> 4
| Short -> 2
| Char -> 1
| Bool -> 1
| Ptr _ -> 8
| Array (t, Some n) -> get_size t * n
| Array (_, None) -> raise Incomplete
| Struct {body=Some{size=size;_};_} -> size
| Struct {body=None;_} -> raise Incomplete
| Union {body=Some{size=size;_};_} -> size
| Union {body=None;_} -> raise Incomplete
| Function _ -> raise NoSize

and get_alignment ty = match ty with
| Long -> 8
| Int -> 4
| Short -> 2
| Char -> 1
| Bool -> 1
| Ptr _ -> 8
| Array (t, _) -> get_alignment t
| Struct {body=Some{alignment=alignment;_};_} -> alignment
| Struct {body=None;_} -> raise Incomplete
| Union {body=Some{alignment=alignment;_};_} -> alignment
| Union {body=None;_} -> raise Incomplete
| Void
| Function _ -> raise NoSize

and is_complete_type ty = match ty with
| Long
| Int
| Short
| Char
| Bool
| Ptr _
| Struct {body=Some _; _}
| Union {body=Some _; _} ->
    true
| Array (t, Some _) ->
    is_complete_type t
| Void
| Struct {body=None; _}
| Union {body=None; _}
| Array (_, None)
| Function _ ->
    false

and is_simple ty = match ty with
| Long
| Int
| Short
| Char
| Bool
| Ptr _ ->
    true
| Struct _
| Union _
| Array _
| Void 
| Function _ ->
    false

and get_field ty field_name = match ty with
| Struct { body = Some { fields = fields; _ }; _ } ->
    List.find (fun { field_name = name; _ } -> name = field_name) fields
| Struct _ ->
    raise Incomplete
| Union { body = Some { fields = fields; _ }; _ } ->
    List.find (fun { field_name = name; _ } -> name = field_name) fields
| Union _ ->
    raise Incomplete
| _ -> failwith ("get_field ty? " ^ (show ty))

and is_function ty = match ty with
| Function _ -> true
| _ -> false
