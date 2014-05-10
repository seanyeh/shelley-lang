type operator = Add | Sub | Mul | Div
type expr =
    Id of string
|   Binop of expr * operator * expr
|   Lit of int
|   Asn of string * expr

type var_args =
    Var of string
|   Keyword of string * expr

type stmt =
    Expr of expr
|   Funcdef of string * var_args list * stmt list


type program = {
  stmt_list : stmt list;
}


