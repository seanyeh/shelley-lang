open Bytecode

(* Bytecode -> Shell *)

let string_of_op op = match op with
    Ast.Add -> " + "
|   Ast.Sub -> " - "
|   Ast.Mul -> " * "
|   Ast.Div -> " / "

let string_of_logical_op op = match op with
|   Ast.Or -> " || "
|   Ast.And -> " && "

let val_of_id id = "$(__VAL__ " ^ id ^ ")"


(* expand is false if a is a func arg,
 * val_of is true when a is being evaluated *)
let string_of_batom ?(expand = true) ?(deref = false) a =  match a with
    BLit(x) -> string_of_int(x)
|   BStr(s) -> s
|   BId(id) ->
        let expanded_id = if expand then "$" ^ id else id in
            if deref then  val_of_id expanded_id
            else            expanded_id


let string_of_barith_atom ?(expand = true) ?(val_of = false) batom = match batom with
    BArith_Op(op) -> string_of_op op
|   BArith_Atom(a) -> string_of_batom a ~expand:expand

let rec sh_of_bexpr ?(expand = true) ?(deref = false) bexpr = match bexpr with
    BAtom(a) -> string_of_batom a ~expand:expand ~deref:deref
|   BArith_Expr(barith_atoms) -> 
        let accum = (fun acc x -> acc ^ string_of_barith_atom x) in
        let expr_str = List.fold_left accum "" barith_atoms in
            "`expr " ^ expr_str ^ "`"

and

sh_of_bstmt bstmt = match bstmt with
|   BAsn(id, e, t, deref) ->
        (* Adds type to e *)
        let e_type = if (t = "v") then "" else t in
            id ^ "=" ^ e_type ^ (sh_of_bexpr e ~deref:deref)

|   BFuncDef(f, var_args_list, bstmt_list) ->
        f ^ "(){\n" ^ sh_of_bstmt_list bstmt_list ^ "}"
|   BFuncCall(f, bexpr_list) ->
        let sh_bexpr_list = List.map (sh_of_bexpr ~expand:false) bexpr_list in
        let args = List.fold_left (fun acc x -> acc ^ " " ^ x)
            "" sh_bexpr_list in
        f ^ args
|   BReturn ->
        "__RETCODE__; return $?"
|   BLogical(op, bstmt1, bstmt2) ->
        sh_of_bstmt bstmt1 ^ string_of_logical_op op ^ sh_of_bstmt bstmt2

    (* Used for debug only *)
|   BRaw(s) -> s
        
and

sh_of_bstmt_list bstmt_list =
    List.fold_left (fun acc x -> acc ^ (sh_of_bstmt x) ^ "\n")
        "" bstmt_list

and
sh_of_bytecode program = match program with
    BStmt_List(l) -> 
        Stdlib.source ^
        sh_of_bstmt_list l
