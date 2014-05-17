open Ast

(* AST -> Bytecode *)


let __RET__ = BuiltinId("RET__", None)

let is_temp var = match var with
|   BuiltinId(_, _) -> true
|   _ -> false


let rec string_of_scope scope = match scope with
|   None -> ""
|   Scope(name, parent, vars) -> (string_of_scope parent) ^ "__" ^ name

let string_of_id scope id = string_of_scope scope ^ "__" ^ id


let create_inner_scope parent name =
    Scope(name, parent, Hashtbl.create 10)


let id_of_var var = match var with
|   BuiltinId(id, scope) -> id
|   Id(id) -> id


let _add_to_scope_ scope var = match scope with
|   None -> raise (Failure ("Non-existent scope"))
|   Scope(_, _, vars_tbl) -> 
        Hashtbl.replace vars_tbl (id_of_var var) ""

let rec find_in_scope ?(check = true) scope var = match var with
|   BuiltinId(id, scope) -> string_of_id scope id
|   Id(id) -> (match scope with
    |   None ->
            raise (Failure ("Undefined variable: " ^ id))
    |   Scope(name, parent, vars_tbl) as curr_scope ->
            if (not check) || (Hashtbl.mem vars_tbl id) then
                (string_of_id curr_scope id)
            else
                find_in_scope parent var
)


let global_scope = Scope("GLOBAL", None, Hashtbl.create 10)
let temp_scope = Scope("TEMP", None, Hashtbl.create 10)

let rec generate_temp_args arg_counter = match arg_counter with
|   0 -> []
|   x -> (generate_temp_args (arg_counter - 1)) @
            [string_of_int (arg_counter - 1)]


(* return list of barith_atom *)
let rec bytecode_of_binop binop scope = match binop with
|   Lit(x) -> [], [Bytecode.BArith_Atom(Bytecode.BLit(x))]
|   Var(var) -> let scoped_id = find_in_scope scope var in
        [], [Bytecode.BArith_Atom(Bytecode.BId(scoped_id))]
|   Binop(e1, op, e2) -> 
        let pre_stmts, barith_list1 = bytecode_of_binop e1 scope in
        let post_stmts, barith_list2 = bytecode_of_binop e2 scope in
        let barith_list_acc = 
            barith_list1 @ [Bytecode.BArith_Op(op)] @ barith_list2 in
        (pre_stmts @ post_stmts), barith_list_acc
|   FuncCall(fid, fexpr_list) ->
        let pre_stmts = 
            bytecode_of_funccall fid fexpr_list scope in
        let _, barith_list = bytecode_of_binop (Var(__RET__)) scope
        in
        pre_stmts, barith_list
        



(* lit,id,binop, NOT asn or funccall *)
and bytecode_of_expr expr scope = match expr with
|   Lit(x) -> [], Bytecode.BAtom(Bytecode.BLit(x))
|   Var(var) -> let scoped_id = find_in_scope scope var in
        [], Bytecode.BAtom(Bytecode.BId(scoped_id))

|   Binop(e1, op, e2) -> 
        let pre_stmts, barith_atom_list = 
            bytecode_of_binop (Binop(e1, op, e2)) scope in
        pre_stmts, Bytecode.BArith_Expr(barith_atom_list)


and bytecode_of_funccall ?(arg_scope = temp_scope) id expr_list scope =
    let arg_counter = ref 0 in
    let new_arg_scope = (create_inner_scope arg_scope "ARG") in
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
    (* Find function in scope *)
    let scoped_fid = find_in_scope scope id in

    let func_args_list =
        (* Use string_of_id instead of find_in_scope because func (temp) args
         * are not found in scope *)
        List.map (fun x -> Bytecode.BAtom(Bytecode.BId(string_of_id new_arg_scope x)))
        (generate_temp_args !arg_counter) in
    let funccall = Bytecode.BFuncCall(scoped_fid, func_args_list) in
        temp_args_stmts @ [funccall]


(* Adds variable to scope 
 * !! SIDE EFFECT !!*)
and _assign_id_ ?(arg_scope = None) var bexpr scope =

    (* Add non-temp variable to scope hashtable *)
    (if not (is_temp var) then
        (_add_to_scope_ scope var));

    let scoped_id = (find_in_scope scope var) in
        Bytecode.BAsn(scoped_id, bexpr)

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
                [_assign_id_ var batom_id2 scope ~arg_scope:arg_scope]

        (* Assign scoped_id <- __RET for FuncCall *)
    |   FuncCall(fid, fexpr_list) ->
            let _, return_id = bytecode_of_expr (Var(__RET__)) scope in
            (* let return_id = Bytecode.BAtom(Bytecode.BId("__RET")) in *)
                (bytecode_of_funccall fid fexpr_list scope ~arg_scope:arg_scope)
                @ [_assign_id_ var return_id scope ~arg_scope:arg_scope]

        (* Otherwise, e is Lit, Id, or Binop *)
    |   e -> let pre_stmts, bexpr = bytecode_of_expr e scope in
            pre_stmts @ [_assign_id_ var bexpr scope
                            ~arg_scope:arg_scope]


(* Return list of Bytecode.bstmt *)
and bytecode_of_stmt stmt scope = match stmt with
|   Expr(e) -> (match e with
        (* Ignore expressions by themselves *)
        Lit(_) -> [] | Var(_) -> [] | Binop(_,_,_) -> []

        (* Only Asn/FuncCall are valid expression as a statement *)
    |   Asn(id, e) -> bytecode_of_asn id e scope
    |   FuncCall(id, expr_list) ->
            bytecode_of_funccall id expr_list scope
            )

|   FuncDef(fid, var_args_list, stmt_list) ->
        let inner_scope = create_inner_scope scope (id_of_var fid) in
        (* Side Effect! *)
        _add_to_scope_ scope fid;

        [Bytecode.BFuncDef((id_of_var fid), var_args_list,
            bytecode_of_stmt_list ~scope:inner_scope stmt_list)]
|   Return(expr) ->
        bytecode_of_asn __RET__ expr scope



and bytecode_of_stmt_list ?(scope = global_scope) stmt_list =
    List.fold_left (fun acc x -> acc @ (bytecode_of_stmt x scope)) [] stmt_list

(* Return list of bstmt *)
and bytecode_of_ast ast =
    Bytecode.BStmt_List(bytecode_of_stmt_list ast.stmt_list)
