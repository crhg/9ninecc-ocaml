open Printf

(* スタックの深さ管理 *)
let sp = ref 0

let set addr = sp := addr

let inc n = sp := !sp + n
let dec n = sp := !sp - n

let add n =
    printf "    add rsp, %d\n" n;
    inc n

let sub n =
    printf "    sub rsp, %d\n" n;
    dec n

let push operand =
    printf "    push %s\n" operand;
    dec 8

let pop operand =
    printf "    pop %s\n" operand;
    inc 8

(* これからパラメタをnバイト積むとしたときにrspが16バイト境界になるように *)
(* 調整してactionを実行し、終わったら調整をしていたら戻す *)
let with_adjust n action =
    let sp' = !sp - n in
    let m = sp' mod 16 in (* ocamlのmodは負の数-xと正の数yについて-x mod y = -(x mod y) *)
    let adjust = if m == 0 then 0 else 16 + m in
    if adjust > 0 then begin sub adjust end;
    action();
    if adjust > 0 then begin add adjust end

let with_save action =
    let saved = !sp in
    action();
    sp := saved
