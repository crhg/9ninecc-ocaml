let preprocess filename source =
    let token lexbuf =
        let t = Pp_lex.token lexbuf in
        (* Printf.fprintf stderr "pp_token=%s\n" (Pp_token.show_token t); *)
        t in
    Compiler.process filename source Pp_parser.preprocessing_file token (fun ast ->
        Pp.preprocess ast
    )

let compile filename source =
    Compiler.process filename source Parser.translation_unit Lex.token (fun ast ->
        Builtin.register();
        Type_check.check ast;
        Gen.gen ast
    )

let preprocess_only = ref false

let compile_source filename source =
    let pp_ast = Pp.ast_of filename source in
    let preprocessed_source = Pp.preprocess pp_ast in
    if !preprocess_only then
        Printf.printf "%s" preprocessed_source
    else 
        compile filename preprocessed_source

let compile_file filename =
    let source = Source.read filename in
    compile_source filename source

let compile_command_line source =
    let source = Source.read "-" ~contents:source in
    compile_source "-" source

let verbose = ref false
let filenames = ref []

let spec = [
    ("-E", Arg.Set preprocess_only, "Output preprocessed source");
    ("-v", Arg.Set verbose, "Turn on verbose message");
    ("-s", Arg.String (compile_command_line), "Source string");
]

let () =
    Arg.parse spec
        (fun s -> filenames := s :: !filenames)
        "Usage: 9ninecc -v filename ...";
    List.iter compile_file !filenames
