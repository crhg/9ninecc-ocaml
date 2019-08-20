open Ast
open Misc
open Printf

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
    | Type.Array (ty, Some n) ->
        init_data_array ty n init
    | _ -> raise(Error_at("cannot initialize type: " ^ (Type.show ty), init.loc))

and init_data_int ty init =
    let open Type in
    let value = match scalar_init_value init with
        | (None, value) -> value
        | _ -> raise(Misc.Error("not a number")) in
    (match ty with
    | Char  -> printf "    .byte %d\n" value
    | Short -> printf "    .word %d\n" value
    | Int   -> printf "    .long %d\n" value
    | Long  -> printf "    .quad %d\n" value
    | _ -> failwith "not integral: "
    )

and init_data_pointer init =
    match scalar_init_value init with
    | (None, n) -> 
        printf "    .quad %d\n" n
    | (Some label, 0) ->
        printf "    .quad %s\n" label
    | (Some label, offset) ->
        printf "    .quad %s%+d\n" label offset

and scalar_init_value init = match init.exp with
| ExprInitializer expr_s ->
    Const.eval @@ Option.get expr_s.i_expr
| ListInitializer (({exp=ExprInitializer _; _} as init)::_) ->
    scalar_init_value init 
| _ -> failwith("not scalar initializer")

and init_data_array ty n init = match init.exp with
| ListInitializer l ->
    init_data_by_list ty n l
| ExprInitializer {expr={exp=Str (s, _);_};_} when ty == Type.Char -> 
    init_str n s
| _ -> raise(Error_at("cannot initialize", init.loc))

and init_data_by_list ty n inits = 
    inits |> List.iteri (fun i init ->
        if i < n then 
            init_data ty init
    );

    (* 初期化リストが足りなければ残りは0で埋める *)
    if n > List.length inits then
        init_zero (Type.get_size ty * (n - List.length inits))

and init_str n s =
    let l = String.length s in
    if l >= n then
        printf "    .ascii \"%s\"\n" (String.sub s l n)
    else
        begin 
            printf "    .string \"%s\"\n" s;
            if l + 1 < n then
                init_zero (n-l-1)
        end

(* nバイトの0で初期化します *)
and init_zero n =
    printf "    .zero %d\n" n
