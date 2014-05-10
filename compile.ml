open Ast

(* AST -> Bytecode *)

let create_inner_scope scope inner = match scope with
    Bytecode.BScope(s) -> Bytecode.BScope(s @ [inner])

(* Scoping *)
let string_of_scope scope = match scope with
    Bytecode.BScope(str_list) -> let concat = (fun acc x -> acc ^ "__" ^ x) in
        (List.fold_left concat "" str_list) ^ "__"

let string_of_id id scope = string_of_scope scope ^ id


(* return list of barith_atom *)
let rec bytecode_of_binop binop scope = match binop with
    Lit(x) -> [Bytecode.BArith_Atom(Bytecode.BLit(x))]
|   Id(id) -> let scoped_id = string_of_id id scope in
        [Bytecode.BArith_Atom(Bytecode.BId(scoped_id))]
|   Binop(e1, op, e2) -> 
        (bytecode_of_binop e1 scope) @ [Bytecode.BArith_Op(op)] @ 
            (bytecode_of_binop e2 scope)

(* lit,id,binop *)
let rec bytecode_of_expr expr scope = match expr with
    Lit(x) -> Bytecode.BAtom(Bytecode.BLit(x))
|   Id(id) -> let scoped_id = string_of_id id scope in
        Bytecode.BAtom(Bytecode.BId(scoped_id))
|   Binop(e1, op, e2) -> 
        Bytecode.BArith_Expr(bytecode_of_binop (Binop(e1, op, e2)) scope)

let rec bytecode_of_asn id expr scope =
    let scoped_id = string_of_id id scope in
    match expr with
        Asn(id2, expr2) ->
            let scoped_id2 = string_of_id id2 scope in
            
            (* TODO:should check sometime if id2 is lit (error) or Id(expected) *)
            (bytecode_of_asn id2 expr2 scope) @ 
                [Bytecode.BAsn(scoped_id, Bytecode.BAtom(Bytecode.BId(scoped_id2)))]
    |   e -> [Bytecode.BAsn(scoped_id, bytecode_of_expr e scope)]


and bytecode_of_stmt stmt scope = match stmt with
    Expr(e) -> (match e with
        (* Ignore expressions by themselves *)
        Lit(_) -> [] | Id(_) -> [] | Binop(_,_,_) -> []

        (* Only Asn is a valid expression as a statement *)
    |   Asn(id, e) -> bytecode_of_asn id e scope)

|   Funcdef(f, var_args_list, stmt_list) -> 
        let inner_scope = create_inner_scope scope f in
        [Bytecode.BFuncdef(f, var_args_list, 
            bytecode_of_stmt_list ~scope:inner_scope stmt_list)]


and bytecode_of_stmt_list ?(scope = Bytecode.global_scope) stmt_list = 
    List.fold_left (fun acc x -> acc @ (bytecode_of_stmt x scope)) [] stmt_list

(* Return list of bstmt *)
and bytecode_of_ast ast =
    Bytecode.BStmt_List(bytecode_of_stmt_list ast.stmt_list)
