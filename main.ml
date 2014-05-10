
let _ =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    let bc = Compile.bytecode_of_ast ast in
    print_endline(Translate.sh_of_bytecode bc)
