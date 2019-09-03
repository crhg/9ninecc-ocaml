let printf = Printf.printf

let rec gen ty label init =
    (* まずラベルまで出力する *)
    printf "    .data\n";
    printf "    .globl %s\n" label;
    printf "    .align %d\n" (Type.get_alignment ty);
    printf "%s:\n" label;

    (* あとは型に応じてデータを出力するのみ *)
    init_data ty init

and init_data ty init =
    let open Type in
    match ty with
    | Char | Short | Int | Long ->
        init_data_int ty init
    | Ptr _ ->
        init_data_pointer init
    | Array (ty, Some n) ->
        init_data_array ty n init
    | _ -> raise(Misc.Error_at("cannot initialize type: " ^ (Type.show ty), init.Ast.loc))

and init_data_int ty init =
    let value = match scalar_init_value init with
        | (None, value) -> value
        | _ -> raise(Misc.Error("not a number")) in
    let open Type in
    match ty with
    | Char  -> printf "    .byte %d\n" value
    | Short -> printf "    .word %d\n" value
    | Int   -> printf "    .long %d\n" value
    | Long  -> printf "    .quad %d\n" value
    | _ -> failwith "not integral: "

and init_data_pointer init =
    match scalar_init_value init with
    | (None, n) -> 
        printf "    .quad %d\n" n
    | (Some label, 0) ->
        printf "    .quad %s\n" label
    | (Some label, offset) ->
        printf "    .quad %s%+d\n" label offset

and scalar_init_value init =
    let open Ast in
    match init.exp with
    | ExprInitializer expr_s ->
        Const.eval @@ Option.get expr_s.i_expr
    | ListInitializer (({exp=ExprInitializer _; _} as init)::_) ->
        scalar_init_value init 
    | _ -> failwith("not scalar initializer")

and init_data_array ty n init =
    let open Ast in
    match init.exp with
    | ListInitializer l ->
        init_data_by_list ty n l
    | ExprInitializer {expr={exp=Str (s, _);_};_} when ty == Type.Char -> 
        init_str n s
    | _ -> raise(Misc.Error_at("cannot initialize", init.loc))

and init_data_by_list ty n inits = 
    List.iter (init_data ty) @@ Misc.take n inits;

    (* 初期化リストが足りなければ残りは0で埋める *)
    let n_rest = n - List.length inits in
    if n_rest > 0 then
        init_zero (Type.get_size ty * n_rest)

(* 文字列sを使ってnバイトを初期化する *)
and init_str n s =
    let l = String.length s in
    if l >= n then
        printf "    .ascii \"%s\"\n" (String.sub s l n)
    else
        begin 
            printf "    .string \"%s\"\n" s;

            (* 初期化すべきバイト数の残りの処理 *)
            let n_rest = n - (l + 1) in
            if n_rest > 0 then
                init_zero n_rest
        end

(* nバイトの0で初期化します *)
and init_zero n =
    printf "    .zero %d\n" n
