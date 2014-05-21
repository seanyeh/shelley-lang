open Bytecode

(* Bytecode -> Shell *)

let string_of_op op = match op with
    Ast.Add -> " + "
|   Ast.Sub -> " - "
|   Ast.Mul -> " \* "
|   Ast.Div -> " / "

let string_of_logical_op op = match op with
|   Ast.Or -> " || "
|   Ast.And -> " && "


let val_of_id id = "$(__VAL__ " ^ id ^ ")"

let gen_cmd ?(newline = true) cmds =
    let cmd =
        List.fold_left (fun acc x -> acc ^ x ^ " ") "" cmds
    in
    if newline then cmd ^ "\n"
    else cmd

let __SETCOUNT__ s =
    gen_cmd ["__SETCOUNT__";s]
let __DECCOUNT__ s =
    gen_cmd ["__DECCOUNT__ ";s]
let __RETCODE__ s =
    gen_cmd ["__RETCODE__";s]
let __SET__ scope id =
    gen_cmd ["__SET__";scope;id] ~newline:false

let string_of_batom ?(deref = false) ?(scope = "") a =  match a with
    BLit(x) -> string_of_int(x)
|   BStr(s) -> s
|   BRawId(id) -> "\"$" ^ id ^ "\""
|   BId(id) ->
        let s = "\"$(__GET__ " ^ scope ^ " \"" ^ id ^ "\")\"" in
        if deref then (val_of_id s)
                 else s

let string_of_barith_atom ?(deref = false) ?(scope = "") batom = match batom with
    BArith_Op(op) -> string_of_op op
|   BArith_Atom(a) -> string_of_batom a ~deref:deref ~scope:scope

let rec sh_of_bexpr ?(deref = false) ?(scope = "") bexpr = match bexpr with
    BAtom(a) -> string_of_batom a ~deref:deref ~scope:scope
|   BArith_Expr(barith_atoms) -> 
        let accum = (fun acc x -> acc ^ string_of_barith_atom x ~deref:true ~scope:scope) in
        let expr_str = List.fold_left accum "" barith_atoms in
            "`expr " ^ expr_str ^ "`"

and

sh_of_bstmt bstmt = match bstmt with
|   BAsn(batom, e, t, scope_str) ->
        let asn = match batom with
        |   BRawId(id) -> id ^ "="
        |   BId(id) -> __SET__ scope_str id
        in

        (* Adds type to e *)
        let e_type = if (t = "v") then "" else t
        in
        asn ^ e_type ^ (sh_of_bexpr e ~scope:scope_str)

|   BFuncDef(f, var_args_list, bstmt_list) ->
        (* Function header *)
        f ^ "(){\n" ^
        (* Set count for function *)
        (__SETCOUNT__ (f ^ "__")) ^
        (* Statements *)
        sh_of_bstmt_list bstmt_list ^
        (* Add DECCOUNT before function end *)
        (__DECCOUNT__  (f ^ "__")) ^
        "}"
|   BFuncCall(f, bexpr_list) ->
        let sh_bexpr_list = List.map (sh_of_bexpr) bexpr_list in
        let args = List.fold_left (fun acc x -> acc ^ " " ^ x)
            "" sh_bexpr_list in
        f ^ args
|   BReturn(scope_str) ->
        let pre_stmt = 
            if (scope_str = "") then ""
            else (__DECCOUNT__ scope_str) 
        in
        pre_stmt ^
        (__RETCODE__ "$__RET__") ^
        "return $?"
|   BLogical(op, bstmt1, bstmt2) ->
        sh_of_bstmt bstmt1 ^ string_of_logical_op op ^ sh_of_bstmt bstmt2
|   BIf(bexpr, bstmt_list) ->
        (__RETCODE__ "$TEMPIF") ^
        "if [ $? -eq 0 ]; then\n" ^
        sh_of_bstmt_list bstmt_list ^
        "fi"

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
