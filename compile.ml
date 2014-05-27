open Ast

(* AST -> Bytecode *)


let __RET__ = BuiltinId("__RET__", None)


let is_temp var = match var with
|   BuiltinId(_, _) -> true
|   TempId(_, _) -> true
|   _ -> false


let rec string_of_scope scope = match scope with
|   None -> ""
|   Scope(name, parent, vars) -> (string_of_scope parent) ^ name ^ "__"

let string_of_id scope id = string_of_scope scope ^ id


let create_inner_scope parent name =
    Scope(name, parent, Hashtbl.create 10)


let id_of_var var = match var with
|   BuiltinId(id, scope) -> id
|   TempId(id, _) -> id
|   Id(id) -> id


let _add_to_scope_ scope var = match scope with
|   None -> raise (Failure ("Non-existent scope"))
|   Scope(_, _, vars_tbl) -> 
        Hashtbl.replace vars_tbl (id_of_var var) ""

let rec find_in_scope ?(check = true) scope var = match var with
|   BuiltinId(id, scope) -> string_of_id scope id
|   TempId(id, scope) -> string_of_id scope id
|   Id(id) -> (match scope with
    |   None ->
            raise (Failure ("Undefined variable: " ^ id))
    |   Scope(name, parent, vars_tbl) as curr_scope ->
            if (not check) || (Hashtbl.mem vars_tbl id) then
                (string_of_id curr_scope id)
            else
                find_in_scope parent var
)

(* Scopes *)
let global_scope = 
    let scope = Scope("__GLOBAL", None, Hashtbl.create 10) in
    _add_to_scope_ scope (BuiltinId("print", None)); scope

let temp_scope = Scope("__TEMP", None, Hashtbl.create 10)


(* For temp functions *)
let __TEMPFUNCS__ = ref 0

let create_temp_func () =
    let fid = "__TEMPFUNC__" ^ (string_of_int !__TEMPFUNCS__) in
    incr __TEMPFUNCS__;
    fid



(* Ast -> Bytecode functions *)

let bytecode_of_var var scope =
    let scoped_var = find_in_scope scope var in
    match var with
    (* BuiltinId's are normal variables *)
    |   BuiltinId(_, _) -> Bytecode.BRawId(scoped_var)
    (* TempId and Id are used with __GET__ and __SET__ *)
    |   TempId(_, _) -> Bytecode.BId(scoped_var)
    |   Id(_) -> Bytecode.BId(scoped_var)


(* return list of barith_atom *)
let rec bytecode_of_binop
    ?(arg_counter = ref 0) ?(arg_scope = temp_scope) binop scope =

    match binop with
|   Binop(e1, op, e2) -> 
        let pre_stmts, barith_list1 =
            bytecode_of_binop e1 scope
                ~arg_counter:arg_counter ~arg_scope:arg_scope in
        let post_stmts, barith_list2 =
            bytecode_of_binop e2 scope
                ~arg_counter:arg_counter ~arg_scope:arg_scope in
        let barith_list_acc = 
            barith_list1 @ [Bytecode.BArith_Op(op)] @ barith_list2 in
        (pre_stmts @ post_stmts), barith_list_acc

|   Lit(x) ->
        [], [Bytecode.BArith_Atom(Bytecode.BLit(x))]
|   Var(var) ->
        let bid = bytecode_of_var var scope in
        [], [Bytecode.BArith_Atom(bid)]


(* A function call as a value in a binop statement *)
|   FuncCall(fid, fexpr_list) ->
        (* Get statements for function call *)
        let pre_stmts = 
            bytecode_of_funccall fid fexpr_list scope in

        (* Create new arg scope for arguments to an expr statement *)
        let new_arg_scope = (create_inner_scope arg_scope "ARG") in

        (* Temp (EXPR) arg variable for expr *)
        let temp_arg = "EXPR" ^ string_of_int !arg_counter in
        let temp_id = (TempId(temp_arg, new_arg_scope)) in

        (* Side effect! *)
        incr arg_counter;

        (* Assign temp (EXPR) arg = __RET__ *)
        let temp_asn = bytecode_of_asn temp_id (Var(__RET__)) scope
                            ~arg_scope:new_arg_scope in

        let _, barith_list = bytecode_of_binop (Var(temp_id)) scope
        in

        pre_stmts @ temp_asn, barith_list

|   Str(_) -> 
        raise (Failure ("Strings do not support arithmetic operators"))
        

(* Returns stmt list , bexpr *)
and bytecode_of_expr ?(arg_scope = temp_scope) expr scope = match expr with

|   Lit(x) -> [], Bytecode.BAtom(Bytecode.BLit(x))
|   Str(x) -> [], Bytecode.BAtom(Bytecode.BStr(x))
|   Var(var) -> 
        let bid = bytecode_of_var var scope in
        [], Bytecode.BAtom(bid)

|   Binop(e1, op, e2) -> 
        let pre_stmts, barith_atom_list = 
            bytecode_of_binop (Binop(e1, op, e2)) scope ~arg_scope:arg_scope in
        pre_stmts, Bytecode.BArith_Expr(barith_atom_list)

|   Asn(id, e) -> 
        let pre_stmts = bytecode_of_asn id e scope in
        let _, var_bexpr =  (bytecode_of_expr (Var(id)) scope) in
        pre_stmts, var_bexpr
|   FuncCall(id, expr_list) ->
        bytecode_of_funccall id expr_list scope, Bytecode.NoBexpr
|   Logical(op, e1, e2) ->
        (* Initialize some temp variables *)
        let temp_id = BuiltinId("TEMP", None) in
        let temp_var = Var(temp_id) in
        let _, bexpr = bytecode_of_expr temp_var scope in
        let return_stmt = (bytecode_of_stmt (Return(temp_var)) None)
        in

        (* Generate temp function definition for both expressions *)
        let gen_func_def temp_fid expr =
            let fstmts = 
                (bytecode_of_asn temp_id expr scope) @ return_stmt
            in
            Bytecode.BFuncDef(temp_fid, [], fstmts)
        in
        let temp_fid1 = create_temp_func () in
        let temp_fid2 = create_temp_func () in

        (* Stmts: func1(){}, func2(){}, func1 || func2 *)
        let stmts =
            [gen_func_def (temp_fid1) e1] @
            [gen_func_def (temp_fid2) e2] @
            [Bytecode.BLogical(op,
                Bytecode.BFuncCall(temp_fid1, []),
                Bytecode.BFuncCall(temp_fid2, []))] @

        (* To stay for now. TODO: change this in future *)
            [Bytecode.BRaw("[ $? -ne 0 ] && TEMP=i0")]
        in

        stmts, bexpr


(* return bstmt list *)
and bytecode_of_funccall ?(arg_scope = temp_scope) id expr_list scope =
    let scoped_fid = find_in_scope scope id in

    let new_arg_scope = (create_inner_scope arg_scope "ARG") in
    let arg_counter = ref 0 in

    let temp_args_stmts =
        List.fold_left (fun acc expr ->
            let temp_arg = string_of_int !arg_counter in
            let bytecode_of_e = bytecode_of_asn (BuiltinId(temp_arg, new_arg_scope)) expr scope
                            ~arg_scope:new_arg_scope
                in
                    incr arg_counter;
                    acc @ bytecode_of_e
                ) [] expr_list
    in

    let rec generate_temp_args arg_counter = match arg_counter with
    |   0 -> []
    |   x -> (generate_temp_args (arg_counter - 1)) @
            [string_of_int (arg_counter - 1)]
    in


    let func_args_list =
        (* Use string_of_id instead of find_in_scope because func (temp) args
         * are not found in scope *)
        List.map (fun x -> Bytecode.BAtom(Bytecode.BRawId(string_of_id new_arg_scope x)))
        (generate_temp_args !arg_counter) in

    let funccall = Bytecode.BFuncCall(scoped_fid, func_args_list) in
        temp_args_stmts @ [funccall]


(* Adds variable to scope 
 * !! SIDE EFFECT !!*)
and _assign_id_ var bexpr scope =

    (* Add non-temp variable to scope hashtable *)
    (if not (is_temp var) then
        (_add_to_scope_ scope var));

    let scoped_id = (find_in_scope scope var) in

    let bid = bytecode_of_var var scope in
    let expr_type = match bexpr with
    |   Bytecode.BArith_Expr(_) -> "i"
    |   Bytecode.BAtom(Bytecode.BLit(_)) -> "i"
    |   Bytecode.BAtom(Bytecode.BId(_)) -> "v" (* variable type *)
    |   Bytecode.BAtom(Bytecode.BRawId(_)) -> "v"
    |   _ -> "s"
    in

        Bytecode.BAsn(bid, bexpr, expr_type, (string_of_scope scope))

(* Return list of Bytecode.bstmt *)
and bytecode_of_asn ?(arg_scope = temp_scope) var expr scope =
    match expr with
    |   Asn(id2, expr2) ->
            (* string_of_id instead of find_in_scope because id2 is not defined
             * yet, but guaranteed to be in scope *)
            (* I sense possible bugs here... *)
            let scoped_id2 = find_in_scope scope id2 ~check:false in
            (*...*)

            let batom_id2 = Bytecode.BAtom(Bytecode.BId(scoped_id2)) in
            
            (bytecode_of_asn id2 expr2 scope) @ 
                [_assign_id_ var batom_id2 scope]

        (* Assign scoped_id <- __RET for FuncCall *)
    |   FuncCall(fid, fexpr_list) ->
            let _, return_id = bytecode_of_expr (Var(__RET__)) scope in
                (bytecode_of_funccall fid fexpr_list scope ~arg_scope:arg_scope)
                @ [_assign_id_ var return_id scope]

        (* Otherwise, e is Lit, Id, or Binop *)
    |   e -> let pre_stmts, bexpr = bytecode_of_expr e scope ~arg_scope:arg_scope in
            pre_stmts @ [_assign_id_ var bexpr scope]


(* Return list of Bytecode.bstmt *)
and bytecode_of_stmt stmt scope = match stmt with
|   Expr(e) -> 
        (* Ignore the value of the expr if it is a standalone statement *)
        let stmts, _ = bytecode_of_expr e scope in

        stmts

|   FuncDef(fid, var_args_list, stmt_list) ->
        (* Side Effect! *)
        _add_to_scope_ scope fid;

        (* Create inner scope with function name *)
        let inner_scope = create_inner_scope scope (id_of_var fid) in

        let scoped_fid = find_in_scope scope fid in

        let var_of_varargs va = match va with
        |   ArgVar(v) -> v | Keyword(v, _) -> v in

        (* After the function definition, assign named var_args to $1, $2... *)
        let rec create_var_arg_stmts ?(i = 1) var_args_list =
            match var_args_list with
            |   [] -> []
            |   x::xs ->
                    let arg_name = "{" ^ string_of_int i ^ "}" in
                    let arg_i = (Var(BuiltinId(arg_name, None)))
                    in

                    (bytecode_of_asn (var_of_varargs x) arg_i inner_scope)
                    @ (create_var_arg_stmts xs ~i:(i+1))
        in
        let var_arg_stmts = create_var_arg_stmts var_args_list in

        (* Prepend func_stmts with var_arg_stmts *)
        let func_stmts =
            var_arg_stmts @
            bytecode_of_stmt_list ~scope:inner_scope stmt_list in

        let func_def = Bytecode.BFuncDef(scoped_fid, var_args_list, func_stmts) in

        [func_def]

|   Return(expr) ->
        (* Assign __RET__ = expr and some boilerplate things *)
        bytecode_of_asn __RET__ expr scope @
        [Bytecode.BReturn(string_of_scope scope)]

|   If(expr, stmt_list) ->
        let temp_id = BuiltinId("TEMPIF", None) in
        let scoped_id = find_in_scope scope temp_id in
        let bstmt_list = bytecode_of_stmt_list stmt_list ~scope:scope in

        bytecode_of_asn temp_id expr scope @
        [Bytecode.BIf(scoped_id, bstmt_list)]


and bytecode_of_stmt_list ?(scope = global_scope) stmt_list =
    List.fold_left (fun acc x -> acc @ (bytecode_of_stmt x scope)) [] stmt_list

(* Return list of bstmt *)
and bytecode_of_ast ast =
    Bytecode.BStmt_List(bytecode_of_stmt_list ast.stmt_list)
