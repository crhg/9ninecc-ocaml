let register _ =
    let ty = Type.(Function { function_ret_type = Void; function_params = []; function_has_varargs = false } ) in
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
