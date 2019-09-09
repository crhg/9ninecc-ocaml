exception No_value

module Option = struct
    include Option

    let may f x = match x with
    | Some x' -> f x'
    | None    -> ()

    let default_lazy x default_value = match x with
    | Some x -> x
    | None -> Lazy.force default_value

    let may_apply f x = match f with
    | Some f -> f x
    | None -> x

    (* 'a->'a optionを合成して'a->'aの関数を作る *)
    (* Noneは恒等関数と考えても良い *)
    let compose f g = match f, g with
    | Some f, Some g -> Misc.compose f g
    | Some f, None -> f
    | None, Some g -> g
    | None, None -> Misc.id

end

module List = struct
    include List

    (* 総和 *)
    let sum = List.fold_left (+) 0

    (* listの先頭n要素のリスト *)
    let rec take n l = match n, l with
    | 0, _
    | _, [] ->
        []
    | n, x :: xs ->
        x :: take (n-1) xs

    let rec zip x y = match x, y with
    | [], _
    | _, [] ->
        []
    | x::xs, y::ys ->
        (x, y) :: zip xs ys

    (* sからeまでのリスト *)
    (* [s;s+1;...;e] *)
    let range s e =
        List.init (e - s + 1) (fun x -> x + s)
end
