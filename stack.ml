open Printf

(* スタックの深さ管理 *)
let sp = ref 0

let reset _ = sp := 0

let add n = sp := !sp + n

let sub n = sp := !sp - n

let push operand =
    printf "    push %s\n" operand;
    add 8

let pop operand =
    printf "    pop %s\n" operand;
    sub 8

let with_adjust n action =
    let sp' = n + !sp in
    let m = sp' mod 16 in
    let adjust = if m == 0 then 0 else 16 - m in
    if adjust > 0 then
        begin
            printf "    sub rsp, %d\n" adjust;
            sub adjust
        end;
    action();
    if adjust > 0 then
        begin
            printf "    add rsp, %d\n" adjust;
            add adjust
        end

let with_save action =
    let saved = !sp in
    action();
    sp := saved
