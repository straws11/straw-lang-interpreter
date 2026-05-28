open Semantic_types

(* helpers *)
let types_match_exact t1 t2 = match t1, t2 with
    | Ast.TBoolean, Ast.TBoolean -> true
    | Ast.TInteger, Ast.TInteger -> true
    | Ast.TFloat, Ast.TFloat -> true
    | Ast.TString, Ast.TString -> true
    | Ast.TFunction, Ast.TFunction -> true
    | _ -> false

(* added compat checks for say floats and ints*)
let types_match t1 t2 =
    if types_match_exact t1 t2 then
        true
    else
        match t1, t2 with
        | Ast.TFloat, Ast.TInteger -> true
        | Ast.TInteger, Ast.TFloat -> true
        | _ -> false

let str_of_dt dt =
    match dt with
        | Ast.TBoolean -> "bool"
        | Ast.TString -> "str"
        | Ast.TInteger -> "int"
        | Ast.TFloat -> "float"
        | Ast.TFunction -> "fn"
        | Ast.TUnit -> "unit"

let create_new_scope outer_scope = { outer = outer_scope; tbl = Hashtbl.create 11 }

(* error *)
exception Type_mismatch_error of string * string * Lexing_types.position

exception Type_invalid_operator_error of string * string * string * Lexing_types.position

exception Type_invalid_un_operator_error of string * string * Lexing_types.position

exception Type_undeclared_error of string * Lexing_types.position

exception Type_custom_error of string * Lexing_types.position

    let () = Printexc.register_printer (function
        | Type_mismatch_error (found, expected, pos) ->
                Some (Printf.sprintf "TypeError: found %s but expected %s at %d:%d" found expected pos.line pos.column)

        | Type_invalid_un_operator_error (dt, op, pos) ->
                Some (Printf.sprintf "TypeError: operator '%s' not applicable for type %s at %d:%d" op dt pos.line pos.column)

        | Type_invalid_operator_error (op, dt, dt2, pos) ->
                Some (Printf.sprintf "TypeError: operator '%s' not applicable for types %s and %s at %d:%d" op dt dt2 pos.line pos.column)

        | Type_undeclared_error (name, pos) ->
                Some (Printf.sprintf "TypeError: variable %s not declared at %d:%d" name pos.line pos.column)

        | Type_custom_error (text, pos) ->
                Some (Printf.sprintf "TypeError: %s at %d:%d" text pos.line pos.column)

        | _ -> None
    )

let rec type_check_return st cur_return_type (ret: Ast.statement) =
    match ret.kind with
    | Ast.ReturnStmt expr_op ->
        let exp_type = match expr_op with
            | Some exp -> Some (type_check_expr st exp)
            | None -> None
        in
        begin match cur_return_type, exp_type with
            | Some return_type, Some ex ->
                if not (types_match_exact return_type ex) then
                    raise (Type_mismatch_error (str_of_dt ex, str_of_dt return_type, ret.pos))
                else
                    ()

            | Some return_type, None -> raise (Type_custom_error ("Missing return type of " ^ str_of_dt return_type, ret.pos))

            | None, Some x -> raise (Type_custom_error ("Return is of type unit but found expression of type " ^ str_of_dt x, ret.pos))
            | None, None -> ()
        end
    | _ -> failwith "Impossible"

and get_var_type st var: (Ast.data_type option) = match lookup_st st var with
    | Some VariableSymbol x -> Some x
    | Some FunctionSymbol (_param_dts, return_dt_op) -> return_dt_op
    | None -> None

and type_check_binary st (binary: Ast.expr) =
    match binary.kind with
    | Ast.Binary (exp1, op, exp2) ->
        let t1 = type_check_expr st exp1 in
        let t2 = type_check_expr st exp2 in
        if not (types_match t1 t2) then
            raise (Type_invalid_operator_error (Ast.string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
        else
            begin match t1 with
                | TUnit -> raise (Type_custom_error ("Cannot add unit types", binary.pos))
                | TFloat ->
                    begin match op with
                        | Add | Sub | Mul | Div -> Ast.TFloat
                        | _ -> Ast.TBoolean
                    end
                | TInteger ->
                        begin match op with
                        | Add | Sub | Mul -> Ast.TInteger
                        | Div -> Ast.TFloat
                        | _ -> Ast.TBoolean
                    end
                | TBoolean ->
                    begin match op with
                        | Ast.EqualOp | Ast.NotEqual -> Ast.TBoolean
                        | _ -> raise (Type_invalid_operator_error (Ast.string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
                    end

                | TFunction -> raise (Type_invalid_operator_error (Ast.string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))

                | TString ->
                    begin match op with
                        | Add -> Ast.TString
                        | Div | Sub | Mul -> raise (Type_invalid_operator_error (Ast.string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
                        | _ -> Ast.TBoolean
                    end
                end
    | _ -> failwith "Impossible"

and type_check_unary st (unary: Ast.expr) =
    match unary.kind with
    | Ast.Unary (op, exp) ->
        let exp_type = type_check_expr st exp in
        begin match op with
        | Ast.Not ->
            if not (exp_type = TBoolean) then
                raise (Type_invalid_un_operator_error (str_of_dt exp_type, Ast.string_of_unary_op op, unary.pos))
            else
                Ast.TBoolean

        | Ast.Negate ->
            begin match exp_type with
                | TInteger | TFloat as x -> x
                | _ -> raise (Type_invalid_un_operator_error (str_of_dt exp_type, Ast.string_of_unary_op op, unary.pos))
            end
        end
    | _ -> failwith "Impossible"

and type_check_logical st (logical: Ast.expr) =
    match logical.kind with
    | Ast.Logical (exp1, op, exp2) ->
        let t1 = type_check_expr st exp1 in
        let t2 = type_check_expr st exp2 in
        begin match t1, t2 with
            | Ast.TBoolean, Ast.TBoolean -> Ast.TBoolean
            (* | Ast.TBoolean, _ -> raise (Type_mismatch_error (str_of_dt t2, "logical", exp2.pos)) *)
            (* | _, Ast.TBoolean -> raise (Type_mismatch_error (str_of_dt t1, "logical", exp1.pos)) *)
            | _, _ -> raise (Type_invalid_operator_error (Ast.string_of_logical_op op, str_of_dt t1, str_of_dt t2, logical.pos))
        end

    | _ -> failwith "Impossible"

and type_check_call st (exp: Ast.expr) =
    let rec loop st exprs data_types = match exprs, data_types with
        | h :: t, dh :: dt ->
            let e = type_check_expr st h in
            if types_match_exact e dh then
                loop st t dt
            else
                raise (Type_mismatch_error (str_of_dt e, str_of_dt dh, exp.pos))

        | [], _h :: _t -> raise (Type_custom_error ("Too few arguments to function call", exp.pos))
        | _h :: _t, [] -> raise (Type_custom_error ("Too many arguments to function call", exp.pos))
        | [], [] -> ()
    in

    match exp.kind with
    | Call (expr, param_exprs) ->
            begin match expr with
            (* TODO: FunExpr should also be able to match `fn (str smth){}("hi")` *)
            | { kind = Variable x; _ } ->
                begin match lookup_st st x with
                    | Some FunctionSymbol (param_dts, ret_dt_op) -> (
                        loop st param_exprs param_dts;
                        begin match ret_dt_op with
                            | Some x -> x
                            | None -> TUnit
                        end
                        )
                    | Some VariableSymbol dt -> raise (Type_custom_error ("Variable of type " ^ str_of_dt dt ^ " not callable", exp.pos))
                    | _ -> raise (Type_custom_error ("Undefined variable not callable", exp.pos))
                end
            | x -> raise (Type_mismatch_error ("expression", "function", exp.pos))
            end

    | _ -> failwith "Impossible"



and type_check_expr st (exp: Ast.expr) = match exp.kind with
    | IntLit x -> Ast.TInteger
    | FloatLit x -> Ast.TFloat
    | BoolLit x -> Ast.TBoolean
    | StrLit x -> Ast.TString

    | Variable x ->
        begin match get_var_type st x with
            | Some y -> y
            | None -> raise (Type_undeclared_error (x, exp.pos))
        end

    (* TODO: nesting of this and typecheck params *)
    | Call (_, _) -> type_check_call st exp
    | Binary (_, _, _) -> type_check_binary st exp
    | Unary (_, _) -> type_check_unary st exp
    | Logical (_, _, _) -> type_check_logical st exp
    | Assign (var, exp) ->
            let var_type = match get_var_type st var with
                | Some x -> x
                | None -> raise (Type_undeclared_error (var, exp.pos))
            in
            let exp_type = type_check_expr st exp in
            if types_match_exact var_type exp_type then
                var_type
            else
                raise (Type_mismatch_error (str_of_dt exp_type, str_of_dt var_type, exp.pos))

    | FunExpr (params, dt_option, body) -> type_check_function_block st params dt_option body; Ast.TFunction
    | Group exp -> type_check_expr st exp

and type_check_statement st (cur_ret_type: Ast.data_type option) (stmt: Ast.statement) = match stmt.kind with
    | IfStmt (exp, body, else_body_op) ->
            let exp_type = type_check_expr st exp in
            begin match exp_type with
                | Ast.TBoolean -> ()
                | _ -> raise (Type_mismatch_error (str_of_dt exp_type, str_of_dt TBoolean, stmt.pos))
            end;
            type_check_block st cur_ret_type body;

            begin match else_body_op with
                | Some eb -> type_check_block st cur_ret_type eb
                | None -> ()
            end

    | WhileStmt (exp, body) ->
            let exp_type = type_check_expr st exp in
            begin match exp_type with
                | Ast.TBoolean -> ()
                | _ -> raise (Type_mismatch_error (str_of_dt exp_type, str_of_dt Ast.TBoolean, stmt.pos))
            end;

            type_check_block st cur_ret_type body;

    | ReturnStmt _ -> ignore (type_check_return st cur_ret_type stmt);

    | VarDeclStmt (dt, name, exp_op) ->
        begin match exp_op with
        | Some { kind = FunExpr (params, return_op, _body); _ } ->
            let param_dts = List.map (fun p -> fst p) params in
            let sym = FunctionSymbol (param_dts, return_op) in
            insert_st st name sym;

        | Some e -> let exp_type = type_check_expr st e in
            if types_match_exact dt exp_type then
                insert_st st name (VariableSymbol dt)
            else
                raise (Type_mismatch_error (str_of_dt exp_type, str_of_dt dt, e.pos))
        | None -> ()
        end;

    | FunDeclStmt (_name, params, dt_option, body) -> (* most already logged by first pass *)
            type_check_function_block st params dt_option body;

    | ExprStmt exp -> ignore (type_check_expr st exp);

    | BlockStmt body -> ignore (type_check_block st cur_ret_type body);

    | PrintStmt exp -> ignore (type_check_expr st exp);

and type_check_statement_list st ret_type stmts =
    let rec loop scope lst = match lst with
        | h :: t -> type_check_statement scope ret_type h; loop scope t
        | [] -> ()
    in
    loop st stmts

and type_check_function_block st params dt_option body =
    let rec loop st lst = match lst with
        | h :: t ->
                let sym = VariableSymbol (fst h) in
                insert_st st (snd h) sym;
                loop st t
        | [] -> ()
    in

    let inner_scope: scope = create_new_scope (Some st) in
    loop inner_scope params;
    type_check_statement_list inner_scope dt_option body

and type_check_block st ret_type body =
    let inner_scope: scope = create_new_scope (Some st) in
    type_check_statement_list inner_scope ret_type body

and type_check st ast = List.iter (type_check_statement st (Some TInteger)) ast

and collect_statement sym_tbl (stmt: Ast.statement) = match stmt.kind with
    | Ast.VarDeclStmt (_dt, name, Some { kind = FunExpr (params, return_op, _body); _ }) ->
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
    let global_scope: scope = { outer = None; tbl =Hashtbl.create 11 } in
    loop global_scope ast;
    print_st global_scope "Collected declarations for type-checking:";
    global_scope


and run_type_checking (ast: Ast.block) =
    let st = collect_declarations ast in
    type_check st ast;
    print_st st "after type checking st"

