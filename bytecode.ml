(* open Ast *)

(* type atom = *)
(*     Lit of int *)

(* type scope = *)
(*     BScope of string list *)

(* let global_scope = BScope(["GLOBAL"]) *)

type batom =
    BLit of int
|   BId of string

type barith_atom =
    BArith_Atom of batom
|   BArith_Op of Ast.operator
    

type bexpr =
    BAtom of batom
|   BArith_Expr of barith_atom list

(* BFuncCall can only be of BAtom or BArith_Expr, not BFuncCall *)
(* |   BFuncCall of string * bexpr list *)


type bstmt =
    BAsn of string * bexpr
    (* BAsn of string * batom *)
|   BFuncDef of string * Ast.var_args list * bstmt list
|   BFuncCall of string * bexpr list

type program =
  BStmt_List of bstmt list

