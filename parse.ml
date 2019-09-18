module Make(I: MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE) = struct

    let parse start message lexer lexbuf =

        let get_parse_error env = 
            match I.stack env with
            | lazy Nil -> "Invalid syntax"
            | lazy (Cons (I.Element (state, _, _, _), _)) ->
                try (message (I.number state)) with
                | Not_found -> "invalid syntax (no message)" in

        let open Lexing in
        let checkpoint = start lexbuf.lex_curr_p in
        let rec parse lexbuf checkpoint =
            match checkpoint with
            | I.InputNeeded _env ->
                let token = lexer lexbuf in
                let startp = lexbuf.lex_start_p in
                let endp = lexbuf.lex_curr_p in
                let checkpoint = I.offer checkpoint (token, startp, endp) in
                parse lexbuf checkpoint
            | I.Shifting _
            | I.AboutToReduce _ ->
                let checkpoint = I.resume checkpoint in
                parse lexbuf checkpoint
            | I.HandlingError _env ->
                let pos = Lexing.lexeme_start_p lexbuf in
                let err = get_parse_error _env in
                raise (Misc.Error_at (err, pos))
            | I.Accepted v ->
                v
            | I.Rejected ->
                raise (Misc.Error "invalid syntax (rejected)") in
        parse lexbuf checkpoint

end
