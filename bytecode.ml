(* open Ast *)

type batom =
    BLit of int
|   BStr of string
|   BId of string
|   BRawId of string

type barith_atom =
    BArith_Atom of batom
|   BArith_Op of Ast.operator
    

type bexpr =
    BAtom of batom
|   BArith_Expr of barith_atom list
|   NoBexpr

type bstmt =
    BAsn of batom * bexpr * string * string
|   BFuncDef of string * Ast.var_args list * bstmt list
|   BFuncCall of string * bexpr list
|   BReturn of string
|   BLogical of Ast.logical_op * bstmt * bstmt
|   BRaw of string
|   BIf of string * bstmt list

type program =
  BStmt_List of bstmt list

