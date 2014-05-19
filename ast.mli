type operator = Add | Sub | Mul | Div
type logical_op = Or | And

type scope =
|   Scope of string * scope * (string, string) Hashtbl.t
|   None

type var =
|   Id of string
|   BuiltinId of string * scope

type expr =
    Var of var
|   Binop of expr * operator * expr
|   Lit of int
|   Str of string
|   Asn of var * expr
|   FuncCall of var * expr list
|   Logical of logical_op * expr * expr
and

var_args =
    ArgVar of var
|   Keyword of var * expr

type stmt =
    Expr of expr
|   FuncDef of var * var_args list * stmt list
|   Return of expr


type program = {
  stmt_list : stmt list;
}


