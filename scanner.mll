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
        (lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - (curr_length + 1))
    in
        (* print_endline("Cur pos: " ^ string_of_int(lexbuf.lex_curr_pos)); *)

        (* If no indent *)
        if (String.length indents = 0) then
            (* Indents in stack -> DEDENT *)
            if not (stack_empty()) then (
                Stack.pop indent_stack;
                (if not (stack_empty()) then 
                    backtrack());
                (* print_endline "DEDENT"; *)
                DEDENT
            )

            (* No indents on stack. Normal end statement *)
            else (
                (* print_endline("noindent END"); *)
                END_STMT (* end statement *)
            )

        (* If indent *)
        else 
            (* If current indent is greater than indent on stack -> INDENT *)
            if (stack_empty() || get_stack_length() < curr_length) then
                (Stack.push indents indent_stack;
                (* print_endline "INDENT"; *)
                INDENT)

            (* If current indent is less than indent on stack -> DEDENT *)
            else if not (stack_empty()) && get_stack_length() > curr_length then
                (
                Stack.pop indent_stack;
                (if not (stack_empty()) && get_stack_length() > curr_length then
                    backtrack());
                (* print_endline "DEDENT"; *)
                DEDENT
                )

            (* Current indent is same as indent on stack. Normal end statement *)
            else (
                (* print_endline("same level END"); *)
                END_STMT
            )
}

rule token =
    parse '\n'[' ']* as s { let len = String.length s in
                            let indents = String.sub s 1 (len - 1) in 
                                parse_indents token lexbuf indents }

    | [' ' '\t' '\r']           { token lexbuf }

    | '\n'                      { print_endline "ENDLINE?"; EOF }
    | '#'                       { comment lexbuf }
    | ';'                       { SEMI }
    | ':'                       { COLON }
    | ','                       { COMMA }
    | '.'                       { DOT }

    | '('                       { LPAREN }
    | ')'                       { RPAREN }
    | '['                       { LBRACKET }
    | ']'                       { RBRACKET }


    | '+'                       { PLUS }
    | '-'                       { MINUS }
    | '*'                       { TIMES }
    | '/'                       { DIVIDE }

    | "or"                      { OR }
    | "and"                     { AND }

    | ">="                      { GTE }
    | "<="                      { LTE }
    | ">"                       { GT }
    | "<"                       { LT }
    | "=="                      { EQ }

    | "if"                      { IF }

    | "def "                    { DEF }
    | "return "                 { RETURN }

    | "class "                  { CLASS }

    | "pass"                    { PASS }


    | "```" ([^'`']*("`"[^'`'])*)* "```" as s  {
                                    let len = String.length s in
                                    let inner = String.sub s 3 (len - 6) in
                                        RAWSTRING(inner)
                                }

    | ['0'-'9']+ as lit         { LITERAL(int_of_string lit) }
    | '"' [^'"']* '"'  as s     { STRING(s) }
    | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
                                { ID(lxm) }
    | '='                       { ASN }
    | eof                       { EOF }
    | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    '\n'  { END_STMT }
|   _ as x     { comment lexbuf }


