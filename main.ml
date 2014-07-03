let compile lexbuf =
    let ast = Parser.program Scanner.token lexbuf in
    let bc = Compile.bytecode_of_ast ast in

    Translate.sh_of_bytecode bc


let compile_string source =
    let lexbuf = Lexing.from_string source in

    compile lexbuf


let _ =
    let lexbuf = Lexing.from_channel stdin in
    (* let stdlib = compile_string Stdlib.stdlib_source in *)
    let stdlib = "" in
    let result = Stdlib.sh_source ^ "\n" ^ stdlib ^ "\n" ^ (compile lexbuf) in

    print_endline result
