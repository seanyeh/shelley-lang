open Ast

(* AST -> Bytecode *)


type scope =
|   Scope of string * scope * (string, string) Hashtbl.t
|   None

let rec string_of_scope scope = match scope with
|   None -> ""
|   Scope(name, parent, vars) -> (string_of_scope parent) ^ "__" ^ name

let string_of_id scope id = string_of_scope scope ^ "__" ^ id


let create_inner_scope parent name =
    Scope(name, parent, Hashtbl.create 10)

let add_to_scope scope var = match scope with
|   None -> raise (Failure ("Non-existent scope"))
|   Scope(_, _, vars_tbl) -> Hashtbl.replace vars_tbl var ""

let rec find_in_scope scope var = match scope with
|   None ->
        raise (Failure ("Undefined variable: " ^ var))
|   Scope(name, parent, vars_tbl) as curr_scope ->
        if (Hashtbl.mem vars_tbl var) then
            (string_of_id curr_scope var)
        else
            find_in_scope parent var


let global_scope = Scope("GLOBAL", None, Hashtbl.create 10)
let temp_scope = Scope("TEMP", None, Hashtbl.create 10)

let rec generate_temp_args arg_counter = match arg_counter with
|   0 -> []
|   x -> (generate_temp_args (arg_counter - 1)) @
            [string_of_int (arg_counter - 1)]


(* return list of barith_atom *)
let rec bytecode_of_binop binop scope = match binop with
|   Lit(x) -> [Bytecode.BArith_Atom(Bytecode.BLit(x))]
|   Id(id) -> let scoped_id = find_in_scope scope id in
        [Bytecode.BArith_Atom(Bytecode.BId(scoped_id))]
|   Binop(e1, op, e2) -> 
        (bytecode_of_binop e1 scope) @ [Bytecode.BArith_Op(op)] @ 
            (bytecode_of_binop e2 scope)



(* lit,id,binop, NOT asn or funccall *)
let rec bytecode_of_expr expr scope = match expr with
|   Lit(x) -> Bytecode.BAtom(Bytecode.BLit(x))
|   Id(id) -> let scoped_id = find_in_scope scope id in
        Bytecode.BAtom(Bytecode.BId(scoped_id))

|   Binop(e1, op, e2) -> 
        Bytecode.BArith_Expr(bytecode_of_binop (Binop(e1, op, e2)) scope)


let rec bytecode_of_funccall ?(arg_scope = temp_scope) id expr_list scope =
    let arg_counter = ref 0 in
    let new_arg_scope = (create_inner_scope arg_scope "ARG") in
    let temp_args_stmts =
        List.fold_left (fun acc expr ->
            let temp_arg = string_of_int !arg_counter in
            let bytecode_of_e = bytecode_of_asn temp_arg expr scope
                            ~temp:true
                            ~arg_scope:new_arg_scope
                in
                    incr arg_counter;
                    acc @ bytecode_of_e
                ) [] expr_list
    in
    (* Find function in scope *)
    let scoped_fid = string_of_id scope id in

    let func_args_list =
        (* Use string_of_id instead of find_in_scope because func (temp) args
         * are not found in scope *)
        List.map (fun x -> Bytecode.BAtom(Bytecode.BId(string_of_id new_arg_scope x)))
        (generate_temp_args !arg_counter) in
    let funccall = Bytecode.BFuncCall(scoped_fid, func_args_list) in
        temp_args_stmts @ [funccall]


(* Side-effect: adds variable to scope *)
and assign_id ?(temp = false) ?(arg_scope = None) var_id bexpr scope =

    (* Add non-temp variable to scope hashtable *)
    (if not temp then (add_to_scope scope var_id));

    (* Use string_of_id for temp variable, find_in_scope otherwise *)
    let scoped_id = if temp then    (string_of_id arg_scope var_id)
                    else            (find_in_scope scope var_id) in
        Bytecode.BAsn(scoped_id, bexpr)

(* Return list of Bytecode.bstmt *)
and bytecode_of_asn ?(temp = false) ?(arg_scope = temp_scope) id expr scope =
    match expr with
    |   Asn(id2, expr2) ->
            (* string_of_id instead of find_in_scope because id2 is not defined
             * yet, but guaranteed to be in scope *)
            let scoped_id2 = string_of_id scope id2 in

            let batom_id2 = Bytecode.BAtom(Bytecode.BId(scoped_id2)) in
            
            (bytecode_of_asn id2 expr2 scope) @ 
                [assign_id id batom_id2 scope ~temp:temp ~arg_scope:arg_scope]

        (* Assign scoped_id <- __RET for FuncCall *)
    |   FuncCall(fid, fexpr_list) ->
            let return_id = Bytecode.BAtom(Bytecode.BId("__RET")) in
                (bytecode_of_funccall fid fexpr_list scope ~arg_scope:arg_scope)
                @ [assign_id id return_id scope ~temp:temp ~arg_scope:arg_scope]

        (* Otherwise, e is Lit, Id, or Binop *)
    |   e -> [assign_id id (bytecode_of_expr e scope) scope
                ~temp:temp ~arg_scope:arg_scope]


(* Return list of Bytecode.bstmt *)
and bytecode_of_stmt stmt scope = match stmt with
|   Expr(e) -> (match e with
        (* Ignore expressions by themselves *)
        Lit(_) -> [] | Id(_) -> [] | Binop(_,_,_) -> []

        (* Only Asn/FuncCall are valid expression as a statement *)
    |   Asn(id, e) -> bytecode_of_asn id e scope
    |   FuncCall(id, expr_list) ->
            bytecode_of_funccall id expr_list scope
            )

|   Funcdef(f, var_args_list, stmt_list) -> 
        let inner_scope = create_inner_scope scope f in
        [Bytecode.BFuncdef(f, var_args_list, 
            bytecode_of_stmt_list ~scope:inner_scope stmt_list)]


and bytecode_of_stmt_list ?(scope = global_scope) stmt_list =
    List.fold_left (fun acc x -> acc @ (bytecode_of_stmt x scope)) [] stmt_list

(* Return list of bstmt *)
and bytecode_of_ast ast =
    Bytecode.BStmt_List(bytecode_of_stmt_list ast.stmt_list)
