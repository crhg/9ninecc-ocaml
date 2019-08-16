let preprocess filename source =
    let token lexbuf =
        let t = Pp_lexer.token lexbuf in
        Printf.fprintf stderr "pp_token=%s\n" (Pp_token.show_token t);
        t in
    Compiler.process filename source Pp_parser.preprocessing_file token (fun ast ->
        Pp.preprocess ast
    )

let compile filename source =
    Compiler.process filename source Parser.translation_unit Typedef_lex.token (fun ast ->
        Type_check.check ast;
        Gen.gen ast
    )


let preprocess_only = ref false

let compile_file filename =
    let file = open_in filename in
    let source = really_input_string file (in_channel_length file) in
    let preprocessed_source = preprocess filename source in
    if !preprocess_only then
        Printf.printf "%s" preprocessed_source
    else 
        compile filename preprocessed_source

let verbose = ref false

let filenames = ref []

let spec = [
    ("-E", Arg.Set preprocess_only, "Output preprocessed source");
    ("-v", Arg.Set verbose, "Turn on verbose message");
    ("-s", Arg.String (compile "-"), "Source string");
]

let () =
    Arg.parse spec
        (fun s -> filenames := s :: !filenames)
        "Usage: 9ninecc -v filename ...";
    List.iter compile_file !filenames
