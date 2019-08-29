let register _ =
    let ty = Type.(Function (Void, [])) in
    let name = "__builtin_va_start" in
    Env.register_global_var ty name name

and is_builtin label =
    match label with
    | "__builtin_va_start" -> true
    | _ -> false

and gen label =
    match label with
    | "__builtin_va_start" ->
        Varargs.gen_va_start()

    | _ -> failwith("unknown: " ^ label)
