type operator = Add | Sub | Mul | Div
type logical_op = Or | And
type compare_op = Eq | Gt | Lt | Gte | Lte

type scope =
|   Scope of string * scope * (string, string) Hashtbl.t
|   None

type var =
|   Id of string

(* Like Id, but don't check for scope *)
|   TempId of string * scope

|   BuiltinId of string * scope

type expr =
    Var of var
|   Binop of expr * operator * expr
|   Lit of int
|   Str of string
|   Array of expr list
|   Asn of var * expr

|   FuncCall of expr * expr list
|   Subscript of var * expr

|   Compare of compare_op * expr * expr
|   Logical of logical_op * expr * expr
and

var_args =
    ArgVar of var
|   Keyword of var * expr

type stmt =
    Expr of expr
|   FuncDef of var * var_args list * stmt list
|   Return of expr
|   If of expr * stmt list
|   NoStmt


type program = {
  stmt_list : stmt list;
}


