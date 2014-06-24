let compile lexbuf =
    let ast = Parser.program Scanner.token lexbuf in
    let bc = Compile.bytecode_of_ast ast in

    Translate.sh_of_bytecode bc


let compile_string source =
    let lexbuf = Lexing.from_string source in

    print_endline (compile lexbuf)


let _ =
    let lexbuf = Lexing.from_channel stdin in

    print_endline (compile lexbuf)
