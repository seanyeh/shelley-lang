type operator = Add | Sub | Mul | Div
type logical_op = Or | And
type compare_op = Eq | Gt | Lt | Gte | Lte

type scope =
|   Scope of string * scope * (string, string) Hashtbl.t
|   None

type var =
|   Id of string

(* Like Id, but don't check for scope *)
(* Used for temp binop argmuents (EXPR0, EXPR1...) *)
|   TempId of string * scope

(* Raw string ID: name * is_var *)
|   BuiltinId of string * bool

type expr =
    Var of var
|   Binop of expr * operator * expr
|   Lit of int
|   Str of string
|   Array of expr list
|   Asn of expr * expr

(* Trailers *)
|   RawFuncCall of string * string list

|   FuncCall of expr * expr list
|   Subscript of expr * expr
|   Dot of expr * var * bool

|   Compare of compare_op * expr * expr
|   Logical of logical_op * expr * expr
|   RawExpr of string
and

var_args =
    ArgVar of var
|   Keyword of var * expr

type stmt =
    Expr of expr
|   FuncDef of var * var_args list * stmt list
|   ClassDef of var * stmt list
|   Return of expr
|   If of expr * stmt list
|   NoStmt


type program = {
  stmt_list : stmt list;
}


