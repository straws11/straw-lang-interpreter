open Semantic_types
open Exceptions

(* helpers *)
let rec types_match_exact t1 t2 = match t1, t2 with
    | Ast.TBoolean, Ast.TBoolean -> true
    | Ast.TInteger, Ast.TInteger -> true
    | Ast.TFloat, Ast.TFloat -> true
    | Ast.TString, Ast.TString -> true
    | Ast.TArray x, Ast.TArray y -> types_match_exact x y
    (* TODO: exact inner types aren't comparable for functions *)
    | Ast.TFunction, Ast.TFunction -> true
    | Ast.TStruct x, Ast.TStruct y -> x = y
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

let rec str_of_dt dt =
    match dt with
        | Ast.TBoolean -> "bool"
        | Ast.TString -> "str"
        | Ast.TInteger -> "int"
        | Ast.TFloat -> "float"
        | Ast.TArray x ->  str_of_dt x ^ "[]"
        | Ast.TFunction -> "fn"
        | Ast.TUnit -> "unit"
        | Ast.TStruct name -> name ^ " (struct)"

let create_new_scope outer_scope = { outer = outer_scope; tbl = Hashtbl.create 11 }

let safe_array_get arr i =
    if i >= 0 && i < Array.length arr then Some arr.(i) else None

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
    | Some FunctionSymbol (_param_dts, _return_dt_op) -> Some TFunction
    | Some StructSymbol _ -> Some (Ast.TStruct var)
    | None -> None

and type_check_struct_expression st (exp: Ast.expr) =
    match exp.kind with
    | StructExpr (type_name, expr_ht) ->
        let expected_members_ht = match lookup_st st type_name with
            | Some StructSymbol x -> Hashtbl.copy x
            | _ ->
                begin match get_var_type st type_name with
                | Some t -> raise (Type_mismatch_error (str_of_dt t, str_of_dt (TStruct type_name), exp.pos))
                | None -> raise (Type_undeclared_error (type_name, exp.pos))
                end
        in
        (* check all fields in the current expression's ht *)
        Hashtbl.iter (fun var_name expr ->
            let stored_mem_type = begin match Hashtbl.find_opt expected_members_ht var_name with
                | Some t -> t
                | None -> raise (Type_custom_error (
                    "Unknown field " ^ var_name
                    ^ " for type " ^ type_name, exp.pos
                    ))
                end
            in
            let field_dt = type_check_expr st expr in
            if not (types_match field_dt stored_mem_type) then
                raise (Type_mismatch_error (str_of_dt field_dt, str_of_dt stored_mem_type, exp.pos))
            else
                Hashtbl.remove expected_members_ht var_name
        ) expr_ht;

        (* missing fields *)
        if Hashtbl.length expected_members_ht > 0 then
            let missing_fields = String.concat ", "
                (Hashtbl.to_seq expected_members_ht
                |> Seq.map (fun (v, dt) -> str_of_dt dt ^ " " ^ v) |> List.of_seq)
            in
            raise (Type_custom_error ("Missing " ^ missing_fields
                ^ " from type " ^ type_name, exp.pos))
        else
            Ast.TStruct type_name

    | _ -> failwith "Impossible"


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

                | TFunction | TStruct _ ->
                    raise (Type_invalid_operator_error (Ast.string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))

                | TString ->
                    begin match op with
                        | Add -> Ast.TString
                        | Div | Sub | Mul -> raise (Type_invalid_operator_error (Ast.string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
                        | _ -> Ast.TBoolean
                    end
                | TArray dt -> raise (Type_invalid_operator_error (Ast.string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))

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



and type_check_array_content st (contents: Ast.expr array) =
    let rec loop idx ty rem = match rem with
        | h :: t ->
            if ty = h then
                loop (idx + 1) ty t
            else
                raise (Type_mismatch_error (str_of_dt h, str_of_dt ty, contents.(idx).pos))
        | [] -> ()
    in
    let dts = Array.map (type_check_expr st) contents in

    match safe_array_get dts 0 with
        | Some dt -> loop 0 dt (Array.to_list dts); Ast.TArray dt
        | None -> failwith "array empty, how should I get the type??"

and type_check_assignment st (exp: Ast.expr) = match exp.kind with
    | Ast.Assign (lhs, rhs) ->
        let rhs_t = type_check_expr st rhs in
        begin match lhs.kind with
        | Ast.Index (arr_expr, idx_expr) ->
            let arr_t = type_check_expr st arr_expr in
            let idx_t = type_check_expr st idx_expr in
            begin match arr_t, idx_t with
                | TArray dt, TInteger ->
                    if not (types_match_exact dt rhs_t) then
                        raise (Type_mismatch_error (str_of_dt rhs_t, str_of_dt dt, rhs.pos))
                    else
                        rhs_t
                | TArray _, _ -> raise (Type_custom_error ("Index type must be of type int", idx_expr.pos))
                | t, _ -> raise (Type_custom_error ("Cannot index into object of type " ^ str_of_dt t, arr_expr.pos))
            end
        | Ast.Variable x ->
            begin match lookup_st st x with
                | Some VariableSymbol dt ->
                    if types_match dt rhs_t then
                        rhs_t
                    else
                        raise (Type_mismatch_error (str_of_dt rhs_t, str_of_dt dt, rhs.pos))
                | Some FunctionSymbol (_, _) -> raise (Type_custom_error ("Cannot reassign function", rhs.pos))
                | _ -> raise (Type_undeclared_error (x, lhs.pos))
            end
        | _ -> raise (Type_custom_error ("Invalid assignment target", lhs.pos))
        end

    | _ -> failwith "Impossible"

and type_check_struct_access st (exp: Ast.expr) =
    match exp.kind with
    | StructAccess (expr, id) ->
        let type_name =
            begin match type_check_expr st expr with
                | TStruct x -> x
                | x -> raise (Type_mismatch_error (str_of_dt x, "a struct type", expr.pos))
            end
        in
        let fields_ht = match lookup_st st type_name with
            | Some StructSymbol ht -> ht
            | Some x -> raise (Type_custom_error ("Not a struct type", expr.pos))
            | _ -> raise (Type_custom_error ("Struct type doesn't exist", exp.pos))
        in
        begin match Hashtbl.find_opt fields_ht id with
            | Some dt -> dt
            | None -> raise (Type_custom_error ("Field " ^ id ^ " doesn't exist on type " ^ type_name, exp.pos))
        end

    | _ -> failwith "Impossible"

and type_check_expr st (exp: Ast.expr) = match exp.kind with
    | IntLit x -> Ast.TInteger
    | FloatLit x -> Ast.TFloat
    | BoolLit x -> Ast.TBoolean
    | StrLit x -> Ast.TString
    | FormattedStringLit (segments, vars) -> List.iter (fun x -> match type_check_expr st x with
            | TString -> ()
            | y -> raise (Type_mismatch_error (str_of_dt y, str_of_dt TString, x.pos))
        ) vars;
        TString

    | ArrayContent x -> type_check_array_content st x

    | Variable x ->
        print_endline (x);
        begin match get_var_type st x with
            | Some y -> y
            | None -> raise (Type_undeclared_error (x, exp.pos))
        end

    (* TODO: nesting of this and typecheck params *)
    | Call (_, _) -> type_check_call st exp

    | Index (exp1, exp2) ->
            let t1 = type_check_expr st exp1 in
            let t2 = type_check_expr st exp2 in
            begin match t1, t2 with
                | TArray dt, TInteger -> dt
                | TArray _, x -> raise (Type_custom_error ("Invalid expression type " ^ str_of_dt x ^ " for array index", exp2.pos))
                | x, _ -> raise (Type_custom_error ("Cannot index into value of type " ^ str_of_dt x, exp1.pos))
            end

    | StructAccess (expr, id) -> type_check_struct_access st exp

    | ArrayLength e ->
        begin match type_check_expr st e with
            | TArray _ -> TInteger
                | _ -> raise (Type_custom_error ("Length can only be invoked on type array", exp.pos))
        end

    | PostfixInc e | PostfixDec e ->
        begin match type_check_expr st e with
            | TInteger -> TInteger
            | x -> raise (Type_mismatch_error (str_of_dt x, str_of_dt TInteger, e.pos))
        end

    | Binary (_, _, _) -> type_check_binary st exp
    | Unary (_, _) -> type_check_unary st exp
    | Logical (_, _, _) -> type_check_logical st exp
    | Assign (_, _) -> type_check_assignment st exp
    | FunExpr (params, dt_option, body) -> type_check_function_block st params dt_option body; Ast.TFunction
    | StructExpr _ -> type_check_struct_expression st exp
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

    | StructDeclStmt (name, ht) -> print_endline "Struct doesn't have type check??";

    | ExprStmt exp -> ignore (type_check_expr st exp);

    | BlockStmt body -> ignore (type_check_block st cur_ret_type body);

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
    | VarDeclStmt (_dt, name, Some { kind = FunExpr (params, return_op, _body); _ }) ->
        let param_dts = List.map (fun p -> fst p) params in
        let sym = FunctionSymbol (param_dts, return_op) in
        insert_st sym_tbl name sym

    | VarDeclStmt (dt, name, _expr_op) ->
        let var_sym = VariableSymbol dt in
        insert_st sym_tbl name var_sym

    | FunDeclStmt (name, params, return_op, _body) ->
            let param_dts = List.map (fun p -> fst p) params in
            let sym = FunctionSymbol (param_dts, return_op) in
            insert_st sym_tbl name sym

    | StructDeclStmt (name, ht) ->
            let members = Hashtbl.copy ht in
            insert_st sym_tbl name (StructSymbol members)

    | _ -> ()

and collect_declarations ast =
    let rec loop st rem_stmts = match rem_stmts with
        | h :: t -> collect_statement st h; loop st t
        | [] -> ()
    in
    let global_scope: scope = { outer = None; tbl =Hashtbl.create 11 } in
    loop global_scope ast;
    global_scope


and inject_stdlib_symbols st =
    List.iter (fun (name, sym) -> insert_st st name sym) Stdlib.builtin_symbols

and run_type_checking (ast: Ast.block) =
    let st = collect_declarations ast in
    inject_stdlib_symbols st;
    print_st st "Collected declarations for type-checking:";
    type_check st ast;
    print_st st "after type checking st"

