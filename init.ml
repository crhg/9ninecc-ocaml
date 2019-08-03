open Ast
open Env
open Misc
open Printf

let rec init_global ty name init =
    (* まずラベルまで出力する *)
    printf "    .data\n";
    printf "    .globl %s\n" name;
    printf "    .align %d\n" (Type.get_alignment ty);
    printf "%s:\n" name;

    (* あとは型に応じてデータを出力するのみ *)
    init_data ty init

and init_data ty init = match ty with
    | Type.Char -> init_data_char init
    | Type.Int -> init_data_int init
    | Type.Ptr _ -> init_data_pointer init
    | Type.Array (ty, n) -> init_data_array ty n init
    | _ -> raise(Error_at("cannot initialize type: " ^ (Type.show ty), init.loc))

and init_data_scalar out init = match init.exp with
| ExprInitializer expr -> out expr
| ListInitializer (({exp=ExprInitializer _} as init)::_) -> init_data_scalar out init 

and out_char expr = printf "    .byte %d\n" (Const.eval_int expr)
and out_int  expr = printf "    .long %d\n" (Const.eval_int expr)

and out_pointer expr =
    let pointer = Const.eval_pointer expr in
    match pointer with
    | (label, 0) ->
        printf "    .quad %s\n" label
    | (label, offset) ->
        printf "    .quad %s%+d\n" label offset

and init_data_char    init = init_data_scalar out_char    init
and init_data_int     init = init_data_scalar out_int     init
and init_data_pointer init = init_data_scalar out_pointer init

and init_data_array ty n init = match init.exp with
| ListInitializer l ->
    init_data_by_list ty n l
| ExprInitializer {exp={e=Str s}} when ty == Type.Char -> 
    init_str n s
| _ -> raise(Error_at("cannot initialize", init.loc))

and init_data_by_list ty n l = match n, l with
| 0, _ -> ()
| n, [] -> init_zero (Type.get_size ty * n)
| n, i::r ->
    init_data ty i;
    init_data_by_list ty (n-1) r

and init_str n s =
    let l = String.length s in
    if l >= n then
        printf "    .ascii \"%s\"\n" s
    else
        begin 
            printf "    .string \"%s\"\n" s;
            if l + 1 < n then
                init_zero (n-l-1)
        end

(* nバイトの0で初期化します *)
and init_zero n =
    printf "    .zero %d\n" n
