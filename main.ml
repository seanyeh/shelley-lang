let compile lexbuf =
    try
        let ast = Parser.program Scanner.token lexbuf in
        let bc = Compile.bytecode_of_ast ast in

        Translate.sh_of_bytecode bc
    with exn ->
        begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in

        let error =
            "line " ^ (string_of_int line) ^ ", char " ^ (string_of_int cnum) ^
            ":\'" ^ tok ^ "\'" in
        raise (Failure error)
      end

let compile_string source =
    let lexbuf = Lexing.from_string (source ^ "\n") in

    compile lexbuf

let compile_file filename =
    let lexbuf = Lexing.from_channel (open_in filename) in

    compile lexbuf

let stdlib =
    let stdlib_source =
        List.fold_left (fun acc x -> acc ^ "\n" ^ (compile_file x))
        "" Stdlib.stdlib_files in

    Stdlib.sh_source ^ "\n" ^ stdlib_source

let _ =

    let stdin_lexbuf = Lexing.from_channel stdin in
    let result = stdlib ^ "\n" ^ (compile stdin_lexbuf) in

    print_endline result


    (* let source = "def f(x):\n  return x + 1" in *)
    (* let lexbuf = Lexing.from_string source in *)
