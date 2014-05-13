open Bytecode

(* Bytecode -> Shell *)

let string_of_op op = match op with
    Ast.Add -> " + "
|   Ast.Sub -> " - "
|   Ast.Mul -> " * "
|   Ast.Div -> " / "

let string_of_batom a =  match a with
    BLit(x) -> string_of_int(x)
|   BId(id) -> "$" ^ id


let string_of_barith_atom batom = match batom with
    BArith_Op(op) -> string_of_op op
|   BArith_Atom(a) -> string_of_batom a

let rec sh_of_bexpr bexpr = match bexpr with
    BAtom(a) -> string_of_batom a
|   BArith_Expr(barith_atoms) -> 
        let accum = (fun acc x -> acc ^ string_of_barith_atom x) in
        let expr_str = List.fold_left accum "" barith_atoms in
            "`expr " ^ expr_str ^ "`"

and

sh_of_bstmt bstmt = match bstmt with
|   BAsn(id, e) -> id ^ "=" ^ (sh_of_bexpr e)
    (* BExpr(x, scope) -> sh_of_bexpr x scope *)
|   BFuncDef(f, var_args_list, bstmt_list) ->
        "function " ^ f ^ "(){\n" ^ sh_of_bstmt_list bstmt_list ^ "}"
|   BFuncCall(f, bexpr_list) ->
        let sh_bexpr_list = List.map sh_of_bexpr bexpr_list in
        let args = List.fold_left (fun acc x -> acc ^ " " ^ x)
            "" sh_bexpr_list in
        f ^ args
        
and

sh_of_bstmt_list bstmt_list =
    List.fold_left (fun acc x -> acc ^ (sh_of_bstmt x) ^ "\n")
        "" bstmt_list

and
sh_of_bytecode program = match program with
    BStmt_List(l) -> sh_of_bstmt_list l
