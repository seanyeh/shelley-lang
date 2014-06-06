%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF ASN SEQ NEWLINE SEMI END_STMT
%token END DEF INDENT DEDENT COLON COMMA RPAREN LPAREN RETURN
%token OR AND EQ GT LT GTE LTE
%token IF
%token <int> LITERAL
%token <string> ID
%token <string> STRING


%start program
%type <Ast.program> program

%%

id: ID {Id($1)}

program:
    /* nothing */ { {stmt_list=[]} }
|   stmt_list EOF { {stmt_list=$1} }

stmt_list:
    stmt {[$1]}
|   stmt stmt_list {[$1] @ $2}


stmt:
    simple_stmt {$1}
|   compound_stmt {$1}


/* 
 * Simple Statement
 */
simple_stmt:

    small_stmt {$1}
|   small_stmt end_stmt {$1}

small_stmt:
    expr_stmt {Expr($1)}
|   flow_stmt {$1}

flow_stmt: return_stmt {$1}

return_stmt:
    RETURN expr_stmt {Return($2)}

expr_stmt:
    testlist {$1}
|   id ASN testlist { Asn($1, $3) }

testlist:
    test {$1}
/*|   test COMMA testlist { [$1] @ $3 } */

test:
    or_test {$1}

or_test:
    and_test {$1}
|   and_test OR or_test { Logical(Or, $1, $3) }

and_test:
    not_test {$1}
|   not_test AND and_test { Logical(And, $1, $3) }

not_test:
    comparison {$1}

comparison:
    expr {$1}
|   expr comp_op comparison {Compare($2, $1, $3)}

comp_op:
    EQ {Eq} | GT {Gt} | LT {Lt} | GTE {Gte} | LTE {Lte}

expr:
    xor_expr {$1}
    /* bitwise xor stuff */

xor_expr:
    and_expr {$1}
/* and bitwise stuff */

and_expr:
    shift_expr {$1}
/* shift bitwise stuff */

shift_expr:
    arith_expr {$1}

arith_expr:
    term {$1}
|   term PLUS arith_expr {Binop($1, Add, $3)}
|   term MINUS arith_expr {Binop($1, Sub, $3)}

term:
    factor {$1}
|   factor TIMES term {Binop($1, Mul, $3)}
|   factor DIVIDE term {Binop($1, Div, $3)}

factor:
    /* some stuff*/
    power {$1}

power:
    /* some stuff */
    atom {$1}

|   id trailer_list {FuncCall($1, $2)}

trailer_list:
    trailer {$1}
|   trailer trailer_list {$1 @ $2}

trailer:
    LPAREN RPAREN {[]}
|   LPAREN arglist RPAREN {$2}

arglist:
    /* some stuff */
|   argument {[$1]}
|   argument COMMA arglist {[$1] @ $3}

argument:
    /* some other stuff*/

    /* this differs from python grammar */
    expr_stmt {$1}

atom:
    LITERAL {Lit($1)}
|   STRING {Str($1)}
|   id {Var($1)}



/* 
 * Compound Statement
 */
compound_stmt:
    if_stmt {$1}
|   funcdef {$1}

if_stmt:
    IF test COLON suite { If($2, $4) }

suite:
    simple_stmt {[$1]}
|   INDENT stmt_list dedent {$2}

dedent:
    DEDENT {}
|   DEDENT end_stmt {}


funcdef:
    DEF id parameters COLON suite {FuncDef($2, $3, $5)}

parameters:
    LPAREN RPAREN {[]}
|   LPAREN varargslist RPAREN {$2}

varargslist:
    /* keyword args */
    id {[ArgVar($1)]}
|   id COMMA varargslist {[ArgVar($1)] @ $3}




end_token: SEMI {} | END_STMT {}
end_stmt:
    end_token       {}
|   end_token end_stmt {$2}
