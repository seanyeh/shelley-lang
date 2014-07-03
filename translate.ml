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


let rec gen_string character length =
    if length <= 0 then "" 
    else character ^ (gen_string character (length - 1))

let rec pow n exp = match exp with
|   0 -> 1
|   x -> n * (pow n (x - 1))

let __BT__ btlevel =
    let num_bs = (pow 2 btlevel) - 1 in
    let num_bs = max 0 num_bs in
    (gen_string "\\" num_bs) ^ "`"

let val_of_id id btlevel=
    let bt = __BT__ btlevel in
    bt ^ ("__VAL__ " ^ id) ^ bt

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



let string_of_batom ?(btlevel = 0) ?(deref = false) ?(scope = "") a =  match a with
    BLit(x) -> string_of_int(x)
|   BStr(s) -> s
|   BRawId(id, is_var) ->
        let s = if is_var then "\"$" ^ id ^ "\""
                          else id
        in
        if deref then (val_of_id s btlevel)
                 else s
|   BId(id, scope_TEMP) ->
        let s = "__GET__ " ^ scope_TEMP ^ " \"" ^ id ^ "\"" in
        if deref then
            let bt = __BT__ (btlevel+1) in
                val_of_id ("\"" ^ bt ^ s ^ bt ^ "\"") btlevel
        else
            let bt = __BT__ btlevel in
                "\"" ^ bt ^ s ^ bt ^ "\""

|   BRawString(s) -> s


let string_of_barith_atom ?(btlevel = 0) ?(deref = false) ?(scope = "") batom = match batom with
    BArith_Op(op) -> string_of_op op
|   BArith_Atom(a) -> string_of_batom a ~btlevel:btlevel ~deref:deref ~scope:scope

let rec sh_of_bexpr ?(deref = false) ?(scope = "") ?(btlevel = 0) bexpr = match bexpr with
    BAtom(a) -> string_of_batom a ~deref:deref ~scope:scope

|   BArith_Expr(barith_atoms) -> 
        let bt = __BT__ btlevel in
        let accum = (fun acc x -> acc ^ string_of_barith_atom x ~btlevel:(btlevel + 1) ~deref:true ~scope:scope) in
        let expr_str = List.fold_left accum "" barith_atoms in
            bt ^ ("expr " ^ expr_str) ^ bt

|   BRawFuncCall(f, bexpr_list) ->
        let bt = __BT__ btlevel in
        let sh_bexpr_list = List.map (sh_of_bexpr ~btlevel:(btlevel + 1)) bexpr_list in
        let args = List.fold_left (fun acc x -> acc ^ " " ^ x)
            "" sh_bexpr_list in
        bt ^ f ^ args ^ bt

|   NoBexpr ->
        ""
        (* raise (Failure ("NoBexpr")) *)

and

sh_of_bstmt bstmt = 
    match bstmt with
|   BAsn(batom, e, t, scope_str) ->
        let asn = match batom with
        |   BRawId(id, _) -> id ^ "="
        |   BId(id, scope_TEMP) -> __SET__ scope_str id
        in

        (* Adds type to e *)
        let e_type = if (t = "v") then "" else t
        in
        asn ^ e_type ^ (sh_of_bexpr e ~scope:scope_str)

|   BFuncDef(f, var_args_list, bstmt_list) ->
        (* Function header: Prefix all function names with __F *)
        "__F" ^ f ^ "(){\n" ^
        (* Set count for function *)
        (__SETCOUNT__ (f ^ "__")) ^
        (* Statements *)
        sh_of_bstmt_list bstmt_list ^
        (* Add DECCOUNT before function end *)
        (__DECCOUNT__  (f ^ "__")) ^
        "}"

(* TODO:f_expr is not used?? *)
|   BFuncCall(fid, bexpr_list) ->
        let sh_bexpr_list = List.map (sh_of_bexpr) bexpr_list in
        let args = List.fold_left (fun acc x -> acc ^ " " ^ x)
            "" sh_bexpr_list in
        fid ^ " " ^ args

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
        sh_of_bstmt_list l
