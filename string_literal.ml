open Printf

let queue = Queue.create()

let rec add s =
    let label = Unique_id.new_id ".LC" in
    Queue.push (label, s) queue;
    label

and gen _ =
    printf "    .section .rodata\n";

    Queue.iter
        (fun (label, s) ->
            printf "%s:\n" label;
            printf "    .string \"%s\"\n" (escaped s)
        )
        queue

(* 文字列を文字列リテラル用にエスケープする *)
and escaped s =
    let buf = Buffer.create (String.length s * 2) in
    s |> String.iter (fun c -> match c with
    | '"'
    | '\\' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
    | _ when (Char.code c >= 0x20 && Char.code c <= 0x7e) ->
        Buffer.add_char buf c
    | _ ->
        Buffer.add_string buf @@ Printf.sprintf "\\x%02x" (Char.code c)
    );
    Buffer.contents buf
