(* 読み込んだソースコードを管理する *)

module StringMap = Map.Make(String)

let contents_map = ref StringMap.empty

(** ファイルを読み込んで保存する。ファイルの中身を文字列で返す
 *  ファイルが改行で終わっていないときは改行を補う *)
let read ?(contents) filename =
    let get_contents filename =
        let chan = open_in filename in
        really_input_string chan (in_channel_length chan) in
    let add_new_line s =
        if String.length s = 0 then "\n"
        else if s.[String.length s - 1] <> '\n' then s ^ "\n"
        else s in

    let contents = Option.default_lazy contents (lazy(get_contents filename)) in
    let contents = add_new_line contents in
    contents_map := StringMap.add filename contents !contents_map;
    contents

let contents filename =
    StringMap.find filename !contents_map

(** 指定されたファイルの指定された行番号の行を文字列で返す
 *  行末の改行は含まない *)
let line filename lno =
    let s = contents filename in
    let open String in
    let rec find_line lno pos =
        if lno = 1 then sub s pos (index_from s pos '\n' - pos)
        else find_line (lno - 1) (index_from s pos '\n' + 1) in
    find_line lno 0

