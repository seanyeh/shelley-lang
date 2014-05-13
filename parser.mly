%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF ASN SEQ NEWLINE SEMI END_STMT
%token END DEF INDENT DEDENT COLON COMMA RPAREN LPAREN
%token <int> LITERAL
%token <string> ID

%left COMMA
%left SEMI END_STMT
%left ASN
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
    /* nothing */ { {stmt_list=[]} }
|   stmt_list EOF { {stmt_list=$1} }

atom:
    LITERAL     { Lit($1) }
|   ID          { Id($1) }

expr:
    atom                { $1 }
|   expr PLUS   expr    { Binop($1, Add, $3) }
|   expr MINUS  expr    { Binop($1, Sub, $3) }
|   expr TIMES  expr    { Binop($1, Mul, $3) }
|   expr DIVIDE expr    { Binop($1, Div, $3) }
|   ID ASN expr        { Asn($1, $3) }
|   ID expr_list {FuncCall($1, $2)}

expr_list_items:
    expr    { [$1] }
|   expr_list_items COMMA expr_list_items { $1 @ $3 }

expr_list:
    LPAREN RPAREN {[]}
|   LPAREN expr_list_items RPAREN {$2}


small_stmt:
    expr    {Expr($1)}

stmt:
     small_stmt end_stmt { $1 }
|    funcdef {$1}


end_token:
    SEMI            {}
|   END_STMT        {}

end_stmt:
    end_token       {}
|   end_token end_stmt {$2}


stmt_list:
    stmt                        { [$1] }
|   small_stmt      {[$1]}
|   stmt stmt_list    { $1::$2 }


/* last one won't need an end_stmt */
suite:
    INDENT stmt_list dedent  {$2}

dedent:
    DEDENT {}
    |DEDENT end_stmt {}


var_args:
    ID    { [Var($1)] }
|   var_args COMMA var_args { $1 @ $3 }


funcdef:
    DEF ID LPAREN var_args RPAREN COLON suite { FuncDef($2, $4, $7) }

