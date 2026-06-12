open Semantic_types
open Exceptions
open Dbg_prints

(* helpers *)
let rec types_match_exact t1 t2 = match t1, t2 with
    | Ast.TBoolean, Ast.TBoolean -> true
    | Ast.TInteger, Ast.TInteger -> true
    | Ast.TFloat, Ast.TFloat -> true
    | Ast.TCharacter, Ast.TCharacter -> true
    | Ast.TString, Ast.TString -> true
    | Ast.TArray x, Ast.TArray y -> types_match_exact x y
    | Ast.TFunction (dt_list, return_dt), Ast.TFunction (bdt_list, breturn_dt) ->
        let match_statuses = (List.map2
            types_match_exact
            (return_dt :: dt_list)
            (breturn_dt :: bdt_list)
        ) in
        not (List.exists (fun x -> x = false) match_statuses)

    | Ast.TNamed x, Ast.TNamed y -> x = y
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

let is_enum st name = match lookup_st st name with
    | Some EnumSymbol _ -> true
    | _ -> false

let rec str_of_dt dt =
    match dt with
        | Ast.TBoolean -> "bool"
        | Ast.TString -> "str"
        | Ast.TCharacter -> "char"
        | Ast.TInteger -> "int"
        | Ast.TFloat -> "float"
        | Ast.TArray x ->  str_of_dt x ^ "[]"
        | Ast.TFunction (dts, return_dt)-> "fn ("
            ^ (String.concat ", " (List.map str_of_dt dts))
            ^ str_of_dt return_dt
            ^ ")"
        | Ast.TUnit -> "unit"
        | Ast.TImplicit -> "implicit, idk if this should ever display lol"
        | Ast.TNamed name -> name ^ " (named)"

let create_new_scope outer_scope = { outer = outer_scope; tbl = Hashtbl.create 11 }

let safe_array_get arr i =
    if i >= 0 && i < Array.length arr then Some arr.(i) else None

let rec type_check_return program_data cur_return_type (ret: Ast.statement) =
    match ret.kind with
    | Ast.ReturnStmt expr_op ->
        let exp_type = match expr_op with
            | Some exp -> Some (type_check_expr program_data exp)
            | None -> None
        in
        begin match cur_return_type, exp_type with
            | Ast.TUnit, Some x ->
                raise (Type_custom_error ("Return is of type unit but found expression of type " ^ str_of_dt x, ret.pos))

            | return_type, Some ex ->
                if not (types_match_exact return_type ex) then
                    raise (Type_mismatch_error (str_of_dt ex, str_of_dt return_type, ret.pos))
                else
                    ()

            | return_type, None -> raise (Type_custom_error ("Missing return type of " ^ str_of_dt return_type, ret.pos))
        end
    | _ -> failwith "Impossible"

and get_var_type program_data var: (Ast.data_type option) =
    match lookup_st program_data.current_file_scope var with
    | Some VariableSymbol x -> Some x
    | Some FunctionSymbol (param_dts, return_dt) ->
        Some (TFunction (param_dts, return_dt))
    | Some StructSymbol _ | Some EnumSymbol _ -> Some (Ast.TNamed var)
    | None -> None

and type_check_struct_expression program_data (exp: Ast.expr) =
    match exp.kind with
    | StructExpr (type_name, expr_ht) ->
        let expected_members_ht = match lookup_st program_data.current_file_scope type_name with
            | Some StructSymbol x -> Hashtbl.copy x
            | _ ->
                begin match get_var_type program_data type_name with
                | Some t -> raise (Type_mismatch_error (str_of_dt t, str_of_dt (TNamed type_name), exp.pos))
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
            let field_dt = type_check_expr program_data expr in
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
            Ast.TNamed type_name

    | _ -> failwith "Impossible"


and type_check_binary program_data (binary: Ast.expr) =
    match binary.kind with
    | Ast.Binary (exp1, op, exp2) ->
        let t1 = type_check_expr program_data exp1 in
        let t2 = type_check_expr program_data exp2 in
        if not (types_match t1 t2) then
            raise (Type_invalid_operator_error (string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
        else
            begin match t1 with
                | TUnit -> raise (Type_custom_error ("Cannot add unit types", binary.pos))
                | TFloat ->
                    begin match op with
                        | Add | Sub | Mul | Div -> Ast.TFloat
                        | Mod -> raise (Type_invalid_operator_error (string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
                        | _ -> Ast.TBoolean
                    end
                | TCharacter ->
                    begin match op with
                    | Add | Sub | Mul | Mod | Div ->
                            raise (Type_invalid_operator_error (
                                string_of_binary_op op,
                                str_of_dt t1, str_of_dt t2,
                                binary.pos)
                            )
                    | _ -> Ast.TBoolean
                    end
                | TInteger ->
                        begin match op with
                        | Add | Sub | Mul | Mod -> Ast.TInteger
                        | Div -> Ast.TFloat
                        | _ -> Ast.TBoolean
                    end
                | TBoolean ->
                    begin match op with
                        | Ast.EqualOp | Ast.NotEqual -> Ast.TBoolean
                        | _ -> raise (Type_invalid_operator_error (string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
                    end

                | TFunction (_, _) ->
                    raise (Type_invalid_operator_error (string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))

                | TNamed name ->
                    if is_enum program_data.current_file_scope name then
                        begin match op with
                            | EqualOp | NotEqual -> Ast.TBoolean
                            | _ -> raise (Type_invalid_operator_error (
                                string_of_binary_op op,
                                str_of_dt t1, str_of_dt t2,
                                binary.pos))
                        end
                    else (* is struct *)
                        raise (Type_invalid_operator_error (string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))

                | TImplicit -> failwith "Shouldn't happen"

                | TString ->
                    begin match op with
                        | Add -> Ast.TString
                        | Div | Sub | Mul | Mod -> raise (Type_invalid_operator_error (string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))
                        | _ -> Ast.TBoolean
                    end
                | TArray dt -> raise (Type_invalid_operator_error (string_of_binary_op op, str_of_dt t1, str_of_dt t2, binary.pos))

                end
    | _ -> failwith "Impossible"

and type_check_unary program_data (unary: Ast.expr) =
    match unary.kind with
    | Ast.Unary (op, exp) ->
        let exp_type = type_check_expr program_data exp in
        begin match op with
        | Ast.Not ->
            if not (exp_type = TBoolean) then
                raise (Type_invalid_un_operator_error (str_of_dt exp_type, string_of_unary_op op, unary.pos))
            else
                Ast.TBoolean

        | Ast.Negate ->
            begin match exp_type with
                | TInteger | TFloat as x -> x
                | _ -> raise (Type_invalid_un_operator_error (str_of_dt exp_type, string_of_unary_op op, unary.pos))
            end
        end
    | _ -> failwith "Impossible"

and type_check_logical program_data (logical: Ast.expr) =
    match logical.kind with
    | Ast.Logical (exp1, op, exp2) ->
        let t1 = type_check_expr program_data exp1 in
        let t2 = type_check_expr program_data exp2 in
        begin match t1, t2 with
            | Ast.TBoolean, Ast.TBoolean -> Ast.TBoolean
            (* | Ast.TBoolean, _ -> raise (Type_mismatch_error (str_of_dt t2, "logical", exp2.pos)) *)
            (* | _, Ast.TBoolean -> raise (Type_mismatch_error (str_of_dt t1, "logical", exp1.pos)) *)
            | _, _ -> raise (Type_invalid_operator_error (string_of_logical_op op, str_of_dt t1, str_of_dt t2, logical.pos))
        end

    | _ -> failwith "Impossible"

and type_check_call program_data (exp: Ast.expr) =
    let rec loop program_data exprs data_types = match exprs, data_types with
        | h :: t, dh :: dt ->
            let e = type_check_expr program_data h in
            if types_match_exact e dh then
                loop program_data t dt
            else
                raise (Type_mismatch_error (str_of_dt e, str_of_dt dh, exp.pos))

        | [], _h :: _t -> raise (Type_custom_error ("Too few arguments to function call", exp.pos))
        | _h :: _t, [] -> raise (Type_custom_error ("Too many arguments to function call", exp.pos))
        | [], [] -> ()
    in

    match exp.kind with
    | Call (expr, param_exprs) ->
        print_st program_data.current_file_scope "before calling type check";
        begin match expr with
        (* TODO: FunExpr should also be able to match `fn (str smth){}("hi")` *)
        | { kind = Variable x; _ } ->
            begin match lookup_st program_data.current_file_scope x with
                | Some FunctionSymbol (param_dts, ret_dt) ->
                    loop program_data param_exprs param_dts;
                    ret_dt

                | Some VariableSymbol dt -> raise (Type_custom_error ("Variable of type " ^ str_of_dt dt ^ " not callable", exp.pos))
                | _ -> raise (Type_custom_error ("Undefined variable not callable", exp.pos))
            end
        | x -> raise (Type_mismatch_error ("expression", "function", exp.pos))
        end

    | _ -> failwith "Impossible"



and type_check_array_content program_data (contents: Ast.expr array) =
    let rec loop idx ty rem = match rem with
        | h :: t ->
            if ty = h then
                loop (idx + 1) ty t
            else
                raise (Type_mismatch_error (str_of_dt h, str_of_dt ty, contents.(idx).pos))
        | [] -> ()
    in
    let dts = Array.map (type_check_expr program_data) contents in

    match safe_array_get dts 0 with
        | Some dt -> loop 0 dt (Array.to_list dts); Ast.TArray dt
        | None -> failwith "array empty, how should I get the type??"

and type_check_assignment program_data (exp: Ast.expr) = match exp.kind with
    | Ast.Assign (lhs, rhs) ->
        let rhs_t = type_check_expr program_data rhs in
        let lhs_t = type_check_expr program_data lhs in
        if types_match_exact lhs_t rhs_t then
            rhs_t
        else
            raise (Type_mismatch_error (str_of_dt rhs_t, str_of_dt lhs_t, exp.pos))
    | _ -> failwith "Impossible"

and type_check_struct_access program_data (exp: Ast.expr) =
    let sym_tbl = program_data.current_file_scope in
    match exp.kind with
    | FieldAccess (expr, id) ->
        let type_name =
            (* TODO: this is redundant i already have it*)
            begin match type_check_expr program_data expr with
                | TNamed x when not (is_enum sym_tbl x) -> x
                | x -> raise (Type_mismatch_error (str_of_dt x, "a struct type", expr.pos))
            end
        in
        let fields_ht = match lookup_st sym_tbl type_name with
            | Some StructSymbol ht -> ht
            | Some x -> raise (Type_custom_error ("Not a struct type", expr.pos))
            | _ -> raise (Type_custom_error ("Struct type doesn't exist", exp.pos))
        in
        begin match Hashtbl.find_opt fields_ht id with
            | Some dt -> dt
            | None -> raise (Type_custom_error ("Field " ^ id ^ " doesn't exist on type " ^ type_name, exp.pos))
        end

    | _ -> failwith "Impossible"

and type_check_enum_access program_data (expr: Ast.expr) =
    let sym_tbl = program_data.current_file_scope in
    match expr.kind with
    | FieldAccess (expr, id) ->
        let type_name =
            begin match type_check_expr program_data expr with
                | TNamed x when is_enum sym_tbl x -> x
                | x -> raise (Type_mismatch_error (str_of_dt x, "an enum type", expr.pos))
            end
        in
        let enum_members = match lookup_st sym_tbl type_name with
            | Some EnumSymbol members -> members
            | Some x -> raise (Type_custom_error ("Not an enum type", expr.pos))
            | _ -> raise (Type_custom_error ("Enum type doesn't exist", expr.pos))
        in
        begin match List.find_opt (fun x -> x = id) enum_members with
            (* TODO: check this logic *)
            | Some _ -> Ast.TNamed type_name
            | None -> raise (Type_custom_error ("Field " ^ id ^ " doesn't exist on type " ^ type_name, expr.pos))
        end

    | _ -> failwith "Impossible"

and type_check_expr program_data (exp: Ast.expr) = match exp.kind with
    | IntLit x -> Ast.TInteger
    | FloatLit x -> Ast.TFloat
    | BoolLit x -> Ast.TBoolean
    | StrLit x -> Ast.TString
    | CharLit x -> Ast.TCharacter
    | FormattedStringLit (segments, vars) -> List.iter (fun x -> match type_check_expr program_data x with
            | TString -> ()
            | y -> raise (Type_mismatch_error (str_of_dt y, str_of_dt TString, x.pos))
        ) vars;
        TString

    | ArrayContent x -> type_check_array_content program_data x

    | Variable x ->
        begin match get_var_type program_data x with
            | Some y -> y
            | None -> raise (Type_undeclared_error (x, exp.pos))
        end

    (* TODO: nesting of this and typecheck params *)
    | Call (_, _) -> type_check_call program_data exp

    | Index (exp1, exp2) ->
            let t1 = type_check_expr program_data exp1 in
            let t2 = type_check_expr program_data exp2 in
            begin match t1, t2 with
                | TArray dt, TInteger -> dt
                | TString, TInteger -> TCharacter
                | TArray _, x -> raise (Type_custom_error ("Invalid expression type " ^ str_of_dt x ^ " for array index", exp2.pos))
                | x, _ -> raise (Type_custom_error ("Cannot index into value of type " ^ str_of_dt x, exp1.pos))
            end

    | FieldAccess (expr, id) ->
        begin match type_check_expr program_data expr with
            | Ast.TNamed x when is_enum program_data.current_file_scope x -> type_check_enum_access program_data exp
            | Ast.TNamed x -> type_check_struct_access program_data exp
            | Ast.TArray _ | Ast.TString ->
                if id = "length" then
                    Ast.TInteger
                else
                    raise (Type_custom_error ("Cannot perform field access on array", expr.pos))
            | x -> raise (Type_custom_error ("Cannot perform field access on type " ^ str_of_dt x, expr.pos))
        end

    | PostfixInc e | PostfixDec e ->
        begin match type_check_expr program_data e with
            | TInteger -> TInteger
            | x -> raise (Type_mismatch_error (str_of_dt x, str_of_dt TInteger, e.pos))
        end

    | Binary (_, _, _) -> type_check_binary program_data exp
    | Unary (_, _) -> type_check_unary program_data exp
    | Logical (_, _, _) -> type_check_logical program_data exp
    | Assign (_, _) -> type_check_assignment program_data exp
    | FunExpr (params, dt, body) ->
        type_check_function_block program_data params dt body;
        let param_dts = List.map (fun p -> fst p) params in
        Ast.TFunction (param_dts, dt)
    | StructExpr _ -> type_check_struct_expression program_data exp
    | Group exp -> type_check_expr program_data exp

and type_check_statement program_data (cur_ret_type: Ast.data_type) (stmt: Ast.statement) =
    let sym_tbl = program_data.current_file_scope in
    match stmt.kind with
    | IfStmt (exp, body, else_body_op) ->
            let exp_type = type_check_expr program_data exp in
            begin match exp_type with
                | Ast.TBoolean -> ()
                | _ -> raise (Type_mismatch_error (str_of_dt exp_type, str_of_dt TBoolean, stmt.pos))
            end;
            type_check_block program_data cur_ret_type body;

            begin match else_body_op with
                | Some eb -> type_check_block program_data cur_ret_type eb
                | None -> ()
            end

    | WhileStmt (exp, body) ->
            let exp_type = type_check_expr program_data exp in
            begin match exp_type with
                | Ast.TBoolean -> ()
                | _ -> raise (Type_mismatch_error (str_of_dt exp_type, str_of_dt Ast.TBoolean, stmt.pos))
            end;

            type_check_block program_data cur_ret_type body;

    | ReturnStmt _ -> ignore (type_check_return program_data cur_ret_type stmt);

    | VarDeclStmt (dt, name, exp_op) ->
        begin match exp_op with
            | Some e ->
                let e_type = type_check_expr program_data e in
                (* insert appropriate symbol for the type (functions are different) *)
                begin match e_type with
                    | TFunction (param_dts, return_dt) ->
                        let sym = FunctionSymbol (param_dts, return_dt) in
                        insert_st sym_tbl name sym;
                    | _ ->
                        let sym = VariableSymbol e_type in
                        insert_st sym_tbl name sym;
                end;
                (* set the type for those that were implicit and typecheck those that were explicit *)
                begin match dt with
                    | TImplicit -> stmt.kind <- VarDeclStmt (e_type, name, exp_op);
                    | found_type -> if not (types_match_exact found_type e_type) then
                            raise (Type_mismatch_error (str_of_dt found_type, str_of_dt e_type, e.pos))
                end
            (* no explicit type on lhs, rhs must be present *)
            | None -> if dt = TImplicit then
                raise (Type_custom_error ("Type cannot be determined, be explicit", stmt.pos))
        end

    | FunDeclStmt (_name, params, dt, body) -> (* most already logged by first pass *)
            type_check_function_block program_data params dt body;

    | StructDeclStmt (name, ht) -> print_endline "Struct doesn't have type check??";

    | EnumDeclStmt (name, members) -> ()

    | ImportStmt s -> failwith "Importing not implemented"

    | BlockStmt body -> ignore (type_check_block program_data cur_ret_type body);

    | ExprStmt exp -> ignore (type_check_expr program_data exp);

and type_check_statement_list program_data ret_type stmts =
    let rec loop pg_data lst = match lst with
        | h :: t -> type_check_statement pg_data ret_type h; loop pg_data t
        | [] -> ()
    in
    loop program_data stmts

and type_check_function_block program_data params dt body =
    let rec loop program_data lst = match lst with
        | h :: t ->
                let sym = VariableSymbol (fst h) in
                insert_st program_data.current_file_scope (snd h) sym;
                loop program_data t
        | [] -> ()
    in

    let inner_scope: scope = create_new_scope (Some program_data.current_file_scope) in
    program_data.current_file_scope <- inner_scope;
    loop program_data params;
    type_check_statement_list program_data dt body

and type_check_block program_data ret_type body =
    let inner_scope: scope = create_new_scope (Some program_data.current_file_scope) in
    program_data.current_file_scope <- inner_scope;
    type_check_statement_list program_data ret_type body

and type_check program_data ast = List.iter (type_check_statement program_data TInteger) ast

and collect_statement program_data (stmt: Ast.statement) =
    let sym_tbl = program_data.current_file_scope in
    match stmt.kind with
    | VarDeclStmt (_dt, name, Some { kind = FunExpr (params, return_op, _body); _ }) ->
        let param_dts = List.map (fun p -> fst p) params in
        let sym = FunctionSymbol (param_dts, return_op) in
        insert_st sym_tbl name sym

    | VarDeclStmt (dt, name, expr_op) ->
        begin match dt with
        | TFunction (param_dts, return_dt) ->
            let sym = FunctionSymbol (param_dts, return_dt) in
            insert_st sym_tbl name sym
        | TImplicit -> ()
        | _ ->
            let var_sym = VariableSymbol dt in
            insert_st sym_tbl name var_sym
        end

    | FunDeclStmt (name, params, return_op, _body) ->
            let param_dts = List.map (fun p -> fst p) params in
            let sym = FunctionSymbol (param_dts, return_op) in
            insert_st sym_tbl name sym

    | StructDeclStmt (name, ht) ->
            let members = Hashtbl.copy ht in
            insert_st sym_tbl name (StructSymbol members)

    | EnumDeclStmt (name, members) ->
        insert_st sym_tbl name (EnumSymbol members)

    | _ -> ()

and collect_declarations ast =
    let rec loop program_data rem_stmts = match rem_stmts with
        | h :: t -> collect_statement program_data h; loop program_data t
        | [] -> ()
    in
    let global_scope: scope = { outer = None; tbl = Hashtbl.create 11 } in

    let pg_data: program_data = {
        current_file_scope = global_scope;
        modules = Hashtbl.create 3;
        import_tracking = Semantic_types.StringSet.empty
    } in

    loop pg_data ast;
    pg_data


and inject_stdlib_symbols program_data =
    List.iter (fun (name, sym) -> insert_st program_data name sym) Stdlib.builtin_symbols

and run_type_checking (ast: Ast.block) =
    let program_data = collect_declarations ast in
    let sym_tbl = program_data.current_file_scope in
    inject_stdlib_symbols sym_tbl;
    print_st sym_tbl "Collected declarations for type-checking:";
    type_check program_data ast;
    print_st sym_tbl "after type checking st";
    program_data

