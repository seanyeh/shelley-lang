open Ast

(* AST -> Bytecode *)


let __RET__ = BuiltinId("__RET__", true)

let expr_counter = ref 0
let global_arg_counter = ref 0

let is_temp var = match var with
|   BuiltinId(_, _) -> true
|   TempId(_, _) -> true
|   _ -> false


(* Scopes *)
let rec string_of_scope ?(recurse = true) scope = match scope with
|   None -> ""
|   Scope(name, parent, vars) ->
        if recurse then (string_of_scope parent) ^ name ^ "__"
        else name ^ "__"

let string_of_id scope id = string_of_scope scope ^ id


let create_inner_scope parent name =
    Scope(name, parent, Hashtbl.create 10)


let id_of_var var = match var with
|   BuiltinId(id, _) -> id
|   TempId(id, _) -> id
|   Id(id) -> id


let _add_to_scope_ scope var = match scope with
|   None -> raise (Failure ("Non-existent scope"))
|   Scope(_, _, vars_tbl) ->
        Hashtbl.replace vars_tbl (id_of_var var) ""

let global_scope =
    let scope = Scope("__GLOBAL", None, Hashtbl.create 10) in
    _add_to_scope_ scope (BuiltinId("print", false));
    _add_to_scope_ scope (BuiltinId("compare", false));
    scope

let temp_scope = Scope("__TEMP", None, Hashtbl.create 10)


(* Returns scoped_string, scope *)
let rec find_in_scope ?(check = true) scope var = match var with
|   BuiltinId(id, _) ->
        (string_of_id None id), scope

(* TODO: works but WTF is scope2 and scope?? *)
|   TempId(id, scope2) ->
        (string_of_id scope2 id), scope


|   Id(id) ->
        (match scope with
    |   None ->
            raise (Failure ("Undefined variable: " ^ id))
    |   Scope(name, parent, vars_tbl) as curr_scope ->
            if (not check) || (Hashtbl.mem vars_tbl id) then(
                (* (print_endline ("Found in scope: " ^ (string_of_id curr_scope id))); *)
                (string_of_id curr_scope id), scope
                )
            else(
                (* hack: TODO: make some universal scope instead of multiple *)
                (* branches of scope *)
                let check_global =
                    (name <> "__GLOBAL") && (match parent with None -> true | _ -> false)
                in

                if check_global then
                    find_in_scope global_scope var
                else
                    find_in_scope parent var
            )
)


(* For temp functions *)
let __TEMPFUNCS__ = ref 0

let create_temp_func () =
    let fid = "TEMPFUNC__" ^ (string_of_int !__TEMPFUNCS__) in
    incr __TEMPFUNCS__;
    fid



(* Ast -> Bytecode functions *)

let bytecode_of_var var scope =
    let scoped_var, found_scope = find_in_scope scope var in
    match var with
    (* BuiltinId's are normal variables *)
    |   BuiltinId(_, is_var) -> Bytecode.BRawId(scoped_var, is_var)

    (* TempId and Id are used with __GET__ and __SET__ *)
    |   TempId(_, _) -> Bytecode.BId(scoped_var, (string_of_scope found_scope))
    |   Id(_) -> Bytecode.BId(scoped_var, (string_of_scope found_scope))


(* return list of barith_atom *)
let rec bytecode_of_binop binop scope =
    match binop with
|   Binop(e1, op, e2) ->
        let pre_stmts, barith_list1 =
            bytecode_of_binop e1 scope in
        let post_stmts, barith_list2 =
            bytecode_of_binop e2 scope in
        let barith_list_acc =
            barith_list1 @ [Bytecode.BArith_Op(op)] @ barith_list2 in
        (pre_stmts @ post_stmts), barith_list_acc

|   Lit(x) ->
        [], [Bytecode.BArith_Atom(Bytecode.BLit(x))]
|   Var(var) ->
        let bid = bytecode_of_var var scope in

        [], [Bytecode.BArith_Atom(bid)]
(* A function call as a value in a binop statement *)
|   FuncCall(fid_expr, fexpr_list) ->
        (* Get statements for function call *)
        let pre_stmts =
            bytecode_of_funccall fid_expr fexpr_list scope in

        (* (* TODO: make code clean *) *)
        (* let prev_arg_scope = match arg_scope with *)
        (* |   None -> scope | _ -> arg_scope in *)

        (* Create new arg scope for arguments to an expr statement *)
        (* let new_arg_scope = (create_inner_scope prev_arg_scope "EXPRARG") in *)

        (* Temp (EXPR) arg variable for expr *)
        let temp_arg = "EXPR" ^ string_of_int !expr_counter in
        let temp_id = (TempId(temp_arg, None)) in

        (* Side effect! Increase global EXPR counter *)
        incr expr_counter;

        (* Assign temp (EXPR) arg = __RET__ *)
        let temp_asn = bytecode_of_asn (Var(temp_id)) (Var(__RET__)) scope
        in

        let _, barith_list = bytecode_of_binop (Var(temp_id)) scope
        in

        pre_stmts @ temp_asn, barith_list

|   Str(_) ->
        raise (Failure ("Strings do not support arithmetic operators"))


and bytecode_of_compare op e1 e2 scope =
    (* Converts a compare to a series of Logical && statements *)

    let op_str op = match op with
    |   Eq -> "-eq" | Gt -> "-gt" | Lt -> "-lt" | Gte -> "-ge" | Lte -> "-le"
    in

    let is_compare expr = match expr with
    |   Compare(_, _, _) -> true | _ -> false
    in

    (* Generate (e1 && (e2 && ...(ex && Lit(1)))) *)
    let rec gen_logical_tree op e1 e2 =
        if not (is_compare e2) then
            (* base case *)
            let args = [Str(op_str op); e1; e2] in
            let funccall = FuncCall(Var(Id("compare")), args) in
            let logical = Logical(And, funccall, Lit(1)) in

            logical
        else
            (* e2 is also a compare. recurse *)
            let next_op, next_e1, next_e2 = match e2 with
            |   Compare(op, e1, e2) -> op, e1, e2 in
            let args = [Str(op_str op); e1; next_e1] in
            let funccall = FuncCall(Var(Id("compare")), args) in

            Logical(And, funccall, (gen_logical_tree next_op next_e1 next_e2))

    in

    bytecode_of_expr (gen_logical_tree op e1 e2) scope


(* Returns stmt list , bexpr *)
and bytecode_of_expr expr scope = match expr with

|   Lit(x) -> [], Bytecode.BAtom(Bytecode.BLit(x))
|   Str(x) -> [], Bytecode.BAtom(Bytecode.BStr(x))
|   Var(var) ->
        (* See bytecode_of_var *)
        let bid = bytecode_of_var var scope in
        [], Bytecode.BAtom(bid)

(* |   Array(expr_list) -> *)
(*         let funccall = FuncCall(Var(BuiltinId("__ARRAY__ create", false)), expr_list) in *)
(*         let stmts, _ = bytecode_of_expr funccall scope in *)
(*         let _, return_id = bytecode_of_expr (Var(__RET__)) scope in *)
(*  *)
(*         stmts, return_id *)

|   Binop(e1, op, e2) ->
        (* See bytecode_of_binop *)
        let pre_stmts, barith_atom_list =
            bytecode_of_binop (Binop(e1, op, e2)) scope in
        pre_stmts, Bytecode.BArith_Expr(barith_atom_list)

|   Asn(id, e) ->
        (* See bytecode_of_asn *)
        let pre_stmts = bytecode_of_asn id e scope in
        let _, var_bexpr =  (bytecode_of_expr id scope) in
        pre_stmts, var_bexpr

(* RawFuncCall only for internal use *)
|   RawFuncCall(id, arg_list) ->
        let _, return_id = bytecode_of_expr (Var(__RET__)) scope in
        let bexpr_list =
            List.map (fun x -> Bytecode.BAtom(Bytecode.BRawString(x))) arg_list in

        [Bytecode.BFuncCall(id, bexpr_list)], return_id


|   FuncCall(id_expr, expr_list) ->
        (* See bytecode_of_funccall *)
        (* bytecode_of_funccall id_expr expr_list scope ~arg_scope:arg_scope, Bytecode.NoBexpr *)
        let _, return_expr = bytecode_of_expr (Var(__RET__)) scope in
        bytecode_of_funccall id_expr expr_list scope, return_expr

|   Dot(id_expr, id_field, is_asn) ->
        (* (* __GETCLASS__ *) *)
        (* let getclass_func_id = *)
        (*     Var(BuiltinId("f__GETCLASS__", false)) in *)
        (* let getclass_args = [id_expr] in *)
        (* let getclass_func = *)
        (*     FuncCall(getclass_func_id, getclass_args) in *)

        let field_name = id_of_var id_field in
        let is_asn_arg = if is_asn then Lit(1) else Lit(0) in

        (* __GETFIELD__ *)
        let getfield_func_id =
            Var(BuiltinId("f__GETFIELD__", false)) in
        let getfield_args =
            [id_expr; Var(BuiltinId(field_name, false)); is_asn_arg] in
        let getfield_func =
            FuncCall(getfield_func_id, getfield_args) in

        bytecode_of_expr getfield_func scope


|   Compare(op, e1, e2) ->
        bytecode_of_compare op e1 e2 scope

|   Logical(op, e1, e2) ->
        (* Initialize some temp variables *)
        let temp_id = BuiltinId("TEMP", true) in
        let temp_var = Var(temp_id) in
        let _, bexpr = bytecode_of_expr temp_var scope in
        (* let return_stmt = (bytecode_of_stmt (Return(temp_var)) None) *)
        let return_stmt = (Return(temp_var))
        in

        (* Generate temp function definition for both expressions *)
        let gen_func_def temp_fid expr =
            let fstmts =
                [Expr(Asn(Var(temp_id), expr))] @ [return_stmt]
            in
            bytecode_of_stmt (FuncDef((Id(temp_fid)), [], fstmts)) scope
        in

        let temp_fid1 = create_temp_func () in
        let temp_fid2 = create_temp_func () in

        (* Stmts: func1(){}, func2(){}, func1 || func2 *)
        let stmts =
            (gen_func_def (temp_fid1) e1) @
            (gen_func_def (temp_fid2) e2) @

            [Bytecode.BLogical(op,
                Bytecode.BFuncCall("__F" ^ (string_of_scope scope) ^ temp_fid1, []),
                Bytecode.BFuncCall("__F" ^ (string_of_scope scope) ^ temp_fid2, []))
                ] @


        (* To stay for now. TODO: change this in future *)
            [Bytecode.BRaw("[ $? -ne 0 ] && TEMP=i0")]
        in

        stmts, bexpr
|   RawExpr(s) ->
        [], Bytecode.BAtom(Bytecode.BRawString(s))


(* return bstmt list *)
and bytecode_of_funccall id_expr expr_list scope =
    let pre_stmts, id_bexpr = bytecode_of_expr id_expr scope in

    let num_args = List.length (id_expr::expr_list) in
    let arg_counter = ref !global_arg_counter in

    (* update global arg counter *)
    global_arg_counter := (num_args + !global_arg_counter);

    let temp_args_stmts =
        List.fold_left (fun acc expr ->
            let temp_arg = "ARG" ^ (string_of_int !arg_counter) in
            let bytecode_of_e = bytecode_of_asn (Var((BuiltinId(temp_arg, true)))) expr scope
                in
                    incr arg_counter;
                    acc @ bytecode_of_e
                ) [] (id_expr::expr_list)
    in

    (* Generate list of strings ARGn where start_i <= n < end_i *)
    let rec generate_temp_args start_i end_i =
        let item = ["ARG" ^ (string_of_int start_i)] in

        if start_i = end_i then
            []
        else
            item @ (generate_temp_args (start_i + 1) end_i)
    in

    let func_args_list =
        (* Use string_of_id instead of find_in_scope because func (temp) args
         * are not found in scope *)
        List.map (fun x -> Bytecode.BAtom(Bytecode.BRawId(x, true)))
        (generate_temp_args (!arg_counter - num_args) !arg_counter) in

    let funccall = Bytecode.BFuncCall("__FUNCCALL__", func_args_list) in
        pre_stmts @ temp_args_stmts @ [funccall]


(* Adds variable to scope
 * !! SIDE EFFECT !!*)
and _assign_id_ ?(expr_type = "") var bexpr scope =

    (* Add non-temp variable to scope hashtable *)
    (if not (is_temp var) then
        (_add_to_scope_ scope var));

    let bid = bytecode_of_var var scope in
    let inferred_type = match bexpr with
    |   Bytecode.BArith_Expr(_) -> "i"
    |   Bytecode.BAtom(Bytecode.BLit(_)) -> "i"
    |   Bytecode.BAtom(Bytecode.BRawString(_)) -> ""
    |   Bytecode.BAtom(Bytecode.BId(_, _)) -> "v" (* variable type *)
    |   Bytecode.BAtom(Bytecode.BRawId(_,_)) -> "v"
    |   _ -> "s"
    in

    let expr_type = if (expr_type = "") then inferred_type else expr_type
    in
        Bytecode.BAsn(bid, bexpr, expr_type, (string_of_scope scope))

(* Return list of Bytecode.bstmt *)
and bytecode_of_asn ?(expr_type = "") expr1 val_expr scope =


    (* var.x -> BAsn(BId(SCOPED__VAR__X), expr2, "", scope of var *)




    (* let get_rightmost dot_expr = *)
    (*     match dot_expr with *)
    (*     | Var(v) -> v *)
    (*     | Dot(e1, v) ->  *)


    (* if expr1 is Dot, then get *)
    let pre_stmts, var = match expr1 with
    | Dot(e1, v, is_asn) ->
            let temp_stmt = Asn(Var(BuiltinId("TEMPFUN", true)), Dot(e1, v, true)) in
            let temp_stmts, _ = bytecode_of_expr temp_stmt scope in
            (* let temp_stmts, temp_bexpr = bytecode_of_expr e1 scope in *)

            (* Can we assume temp_bexpr is always __RET__? *)
            temp_stmts, TempId("$TEMPFUN", None)

    | Var(v) -> [], v
    in




    (* let var_of_expr vexpr = match vexpr with *)
    (* | Var(v) -> v *)
    (* (* | Dot(e1, var) -> *) *)
    (* (*         bytecode_of_expr e1 *) *)
    (* (*         var_of_expr e1 *) *)
    (* | _ -> raise (Failure ("Invalid Assignment")) *)
    (* in *)

    (* let var = var_of_expr expr1 in *)

    match val_expr with
    (* |   Asn(id2, expr2) -> *)
    (*         (* string_of_id instead of find_in_scope because id2 is not defined *)
    (*          * yet, but guaranteed to be in scope *) *)
    (*         (* I sense possible bugs here... *) *)
    (*         let scoped_id2, _ = find_in_scope scope (var_of_expr id2) ~check:false in *)
    (*         (*...*) *)
    (*  *)
    (*         let batom_id2 = Bytecode.BAtom(Bytecode.BId(scoped_id2, (string_of_scope scope))) in *)
    (*  *)
    (*         pre_stmts @ *)
    (*         (bytecode_of_asn id2 expr2 scope ~expr_type:expr_type) @ *)
    (*             [_assign_id_ var batom_id2 scope ~expr_type:expr_type] *)

        (* Assign scoped_id <- __RET for FuncCall *)
    |   FuncCall(fid, fexpr_list) ->
            let _, return_id = bytecode_of_expr (Var(__RET__)) scope in
            pre_stmts @
                (bytecode_of_funccall fid fexpr_list scope)
                @ [_assign_id_ var return_id scope ~expr_type:expr_type]

        (* Otherwise, e is Lit, Id, or Binop *)
    |   e -> let pre_stmts2, bexpr = bytecode_of_expr e scope in
            pre_stmts @
            pre_stmts2 @ [_assign_id_ var bexpr scope ~expr_type:expr_type]


(* Return list of Bytecode.bstmt *)
and bytecode_of_stmt stmt scope = match stmt with
(* Expr *)
|   Expr(RawExpr(s)) -> [Bytecode.BRaw(s)]
|   Expr(e) ->
        (* Ignore the value of the expr if it is a standalone statement *)
        let stmts, _ = bytecode_of_expr e scope in

        stmts

|   FuncDef(fid, var_args_list, stmt_list) ->
        (* Assign function variable to function *)
        (* e.g. set helloworld=__F__helloworld *)
        let temp_fid, _ = find_in_scope scope fid ~check:false in
        let fasn = bytecode_of_asn (Var(fid)) (Str("__F" ^ temp_fid)) scope ~expr_type:"f" in

        (* Create inner scope with function name *)
        let inner_scope = create_inner_scope scope (id_of_var fid) in

        let scoped_fid, _ = find_in_scope scope fid in

        let var_of_varargs va = match va with
        |   ArgVar(v) -> v | Keyword(v, _) -> v in

        (* After the function definition, assign named var_args to $1, $2... *)
        let rec create_var_arg_stmts ?(i = 1) var_args_list =
            match var_args_list with
            |   [] -> []
            |   x::xs ->
                    let arg_name = "{" ^ string_of_int i ^ "}" in
                    let arg_i = (Var(BuiltinId(arg_name, true)))
                    in

                    (bytecode_of_asn (Var(var_of_varargs x)) arg_i inner_scope)
                    @ (create_var_arg_stmts xs ~i:(i+1))
        in
        let var_arg_stmts = create_var_arg_stmts var_args_list in

        (* Prepend func_stmts with var_arg_stmts *)
        let func_stmts =
            var_arg_stmts @
            bytecode_of_stmt_list ~scope:inner_scope stmt_list in

        let func_def = Bytecode.BFuncDef(scoped_fid, var_args_list, func_stmts) in

        func_def :: fasn

|   Return(expr) ->
        (* Assign __RET__ = expr and some boilerplate things *)
        bytecode_of_asn (Var(__RET__)) expr scope @
        [Bytecode.BReturn(string_of_scope scope)]

|   If(expr, stmt_list) ->
        let temp_id = BuiltinId("TEMPIF", true) in
        let scoped_id, _ = find_in_scope scope temp_id in
        let bstmt_list = bytecode_of_stmt_list stmt_list ~scope:scope in

        bytecode_of_asn (Var(temp_id)) expr scope @
        [Bytecode.BIf(scoped_id, bstmt_list)]

|   ClassDef(cid, stmts) ->
        (* First argument to class function is the class method *)
        (* TODO: rename method to something else *)
        let hack_TEMP = Expr(RawExpr("varname=\"$1\"")) in

        (* let eval_stmt =  *)
        (*     Expr(RawFuncCall("eval", ["\"__F__GLOBAL__Array__`__VAL__ $method` \"$@\"\""])) in *)

        let init_func_id =
            Var(BuiltinId("f__F__GLOBAL__Array__init $varname $2", false)) in
        let init_func =
            FuncCall(init_func_id, []) in
        let return_stmt =
            Return(init_func) in


        let new_stmts = [hack_TEMP] @ stmts @ [return_stmt] in

        let classfunc = FuncDef(cid, [], new_stmts) in

        bytecode_of_stmt classfunc scope

|   NoStmt ->
        []


and bytecode_of_stmt_list ?(scope = global_scope) stmt_list =
    List.fold_left (fun acc x -> acc @ (bytecode_of_stmt x scope)) [] stmt_list

(* Return list of bstmt *)
and bytecode_of_ast ast =
    Bytecode.BStmt_List(bytecode_of_stmt_list ast.stmt_list)
