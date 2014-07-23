let compile lexbuf =
    let ast = Parser.program Scanner.token lexbuf in
    let bc = Compile.bytecode_of_ast ast in

    Translate.sh_of_bytecode bc


let compile_string source =
    let lexbuf = Lexing.from_string source in

    compile lexbuf

let compile_file filename =
    let lexbuf = Lexing.from_channel (open_in filename) in

    compile lexbuf


let _ =
    let stdlib_source =
        List.fold_left (fun acc x -> acc ^ "\n" ^ (compile_file x))
        "" Stdlib.stdlib_files in
    let stdlib = Stdlib.sh_source ^ "\n" ^ stdlib_source in

    let stdin_lexbuf = Lexing.from_channel stdin in
    let result = stdlib ^ "\n" ^ (compile stdin_lexbuf) in

    print_endline result
