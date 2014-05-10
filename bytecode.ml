(* open Ast *)

(* type atom = *)
(*     Lit of int *)

type scope =
    BScope of string list

let global_scope = BScope(["GLOBAL"])

type batom =
    BLit of int
|   BId of string

type barith_atom =
    BArith_Atom of batom
|   BArith_Op of Ast.operator
    

type bexpr =
    BAtom of batom
|   BArith_Expr of barith_atom list


type bstmt =
    (* BExpr of bexpr * scope *)
    BAsn of string * bexpr
|   BFuncdef of string * Ast.var_args list * bstmt list

type program =
  BStmt_List of bstmt list

