{ 
open Parser 
open Lexing

let indent_stack = (Stack.create() : string Stack.t)

let parse_indents token lexbuf indents = 
    let stack_empty = fun() ->
        ((Stack.length indent_stack) == 0) in
    let curr_length = String.length indents in
    let get_stack_length = fun () -> 
        String.length (Stack.top indent_stack) in
    let backtrack = fun() -> 
        (* print_endline("Backtrack"); *)
        lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - curr_length
    in
        (* print_endline("Cur pos: " ^ string_of_int(lexbuf.lex_curr_pos)); *)
        if (String.length indents = 0) then
            if not (stack_empty()) then (
                Stack.pop indent_stack;
                (if not (stack_empty()) then 
                    backtrack());
                (* print_endline "DEDENT"; *)
                DEDENT
            )
            else (
                (* print_endline("noindent END"); *)
                END_STMT (* end statement *)
            )
        else 
            if (stack_empty() || get_stack_length() < curr_length) then
                (Stack.push indents indent_stack;
                (* print_endline "INDENT"; *)
                INDENT)
            else if not (stack_empty()) && get_stack_length() > curr_length then
                (
                Stack.pop indent_stack;
                (if not (stack_empty()) && get_stack_length() > curr_length then
                    backtrack());
                (* print_endline "DEDENT"; *)
                DEDENT
                )
            else (
                (* print_endline("same level END"); *)
                END_STMT
            )
}

rule token =
    parse '\n'[' ']* as s { let len = String.length s in
                            let indents = String.sub s 1 (len - 1) in 
                                parse_indents token lexbuf indents }
    | [' ' '\t' '\r']  { token lexbuf }

    | '\n'                      { print_endline("newline?"); NEWLINE }
    | ';'                       { SEMI }
    | ':'                       { COLON }
    | ','                       { COMMA }
    | '('                       { LPAREN }
    | ')'                       { RPAREN }
    | '+'                       { PLUS }
    | '-'                       { MINUS }
    | '*'                       { TIMES }
    | '/'                       { DIVIDE }

    | "or"                      { OR }
    | "and"                     { AND }
    | "def "                     { DEF }
    | "return "                 { RETURN }

    | ['0'-'9']+ as lit         { LITERAL(int_of_string lit) }
    | '"' [^'"']* '"'  as lxm   { STRING(lxm) }
    | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
    | '='                       { ASN }
    | eof                       { EOF }
    | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
