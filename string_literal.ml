open Printf

let queue = Queue.create()

let add s =
    let seq = Queue.length queue in
    let label = sprintf ".L%d" seq in
    Queue.push (label, s) queue;
    label

let gen _ =
    printf "    .section .rodata\n";

    Queue.iter
        (fun (label, s) ->
            printf "%s:\n" label;
            printf "    .string %s\n" s
        )
        queue
