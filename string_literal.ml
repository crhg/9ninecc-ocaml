open Printf

let queue = Queue.create()

let add s =
    let label = Unique_id.new_id ".LC" in
    Queue.push (label, s) queue;
    label

let gen _ =
    printf "    .section .rodata\n";

    Queue.iter
        (fun (label, s) ->
            printf "%s:\n" label;
            printf "    .string \"%s\"\n" s
        )
        queue
