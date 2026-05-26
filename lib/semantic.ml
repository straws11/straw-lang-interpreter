open Semantic_types

let rec thing = ()

and collect_statement sym_tbl (stmt: Ast.statement) = match stmt.kind with
    | Ast.VarDeclStmt (_, name, Some { kind = FunExpr (params, return_op, _body); _ }) ->
        let param_dts = List.map (fun p -> fst p) params in
        let sym = FunctionSymbol (param_dts, return_op) in
        insert_st sym_tbl name sym

    | Ast.VarDeclStmt (dt, name, _expr_op) ->
        let var_sym = VariableSymbol dt in
        insert_st sym_tbl name var_sym

    | Ast.FunDeclStmt (name, params, return_op, _body) ->
            let param_dts = List.map (fun p -> fst p) params in
            let sym = FunctionSymbol (param_dts, return_op) in
            insert_st sym_tbl name sym

    | _ -> ()

and collect_declarations ast =
    let rec loop st rem_stmts = match rem_stmts with
        | h :: t -> collect_statement st h; loop st t
        | [] -> ()
    in
    let symbol_tbl: symbol_table = Hashtbl.create 11 in
    loop symbol_tbl ast;
    print_st symbol_tbl;
    symbol_tbl


and type_check st ast = ()

and run_type_checking ast =
    let st = collect_declarations ast in
    type_check st ast

