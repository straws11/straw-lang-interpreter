open Semantic_types

(* helpers *)
let types_match t1 t2 = match t1, t2 with
    | Ast.TBoolean, Ast.TBoolean -> true
    | Ast.TNumber, Ast.TNumber -> true
    | Ast.TString, Ast.TString -> true
    | Ast.TFunction, Ast.TFunction -> true
    | _ -> false

let pretty_string_of_data_type dt =
    match dt with
        | Ast.TBoolean -> "bool"
        | Ast.TString -> "str"
        | Ast.TNumber -> "num"
        | Ast.TFunction -> "fn"

let create_new_scope outer_scope = { outer = outer_scope; tbl = Hashtbl.create 11 }

(* error *)
exception Type_mismatch_error of string * string * Lexing_types.position

exception Type_invalid_operator_error of string * string * Lexing_types.position

exception Type_undeclared_error of string * Lexing_types.position

exception Type_custom_error of string * Lexing_types.position

    let () = Printexc.register_printer (function
        | Type_mismatch_error (found, expected, pos) ->
                Some (Printf.sprintf "TypeError: found %s but expected %s at %d:%d" found expected pos.line pos.column)

        | Type_invalid_operator_error (dt, op, pos) ->
                Some (Printf.sprintf "TypeError: operator '%s' not applicable for type %s at %d:%d" op dt pos.line pos.column)

        | Type_undeclared_error (name, pos) ->
                Some (Printf.sprintf "TypeError: variable %s not declared at %d:%d" name pos.line pos.column)

        | Type_custom_error (text, pos) ->
                Some (Printf.sprintf "TypeError: %s at %d:%d" text pos.line pos.column)

        | _ -> None
    )

let rec thing = ()

and type_check_return st cur_return_type expr_op =
        let exp_type = match expr_op with
            | Some exp -> Some (type_check_expr st exp)
            | None -> None
        in
        begin match cur_return_type, exp_type with
            | Some return_type, Some ex ->
                if not (types_match return_type ex) then
                    raise (Type_mismatch_error (pretty_string_of_data_type ex, pretty_string_of_data_type return_type, ex.position))
                else
                    ()

            | Some return_type, None -> raise (Type_custom_error ("Missing return type of " ^ pretty_string_of_data_type return_type))

            | None, Some x -> raise (Type_custom_error ("Return is of type unit but found expression of type " ^ pretty_string_of_data_type x, x.pos))
            | None, None -> ()
        end

and get_var_type st var: (Ast.data_type option) = match lookup_st st var with
    | Some VariableSymbol x -> Some x
    | Some FunctionSymbol (_param_dts, return_dt_op) -> return_dt_op
    | None -> None

and type_check_binary st exp1 bin_op exp2 =
    let t1 = type_check_expr st exp1 in
    let t2 = type_check_expr st exp2 in
    if not (types_match t1 t2) then
        raise (Type_invalid_operator_error (pretty_string_of_data_type t1, Ast.string_of_binary_op bin_op))
    else
        match t1 with
            | TNumber ->
                begin match bin_op with
                    | Add | Sub | Mul | Div -> Ast.TNumber
                    | _ -> Ast.TBoolean
                end
            | TBoolean ->
                begin match bin_op with
                    | Ast.EqualOp | Ast.NotEqual -> Ast.TBoolean
                    | _ -> raise (Type_invalid_operator_error (pretty_string_of_data_type t1 , Ast.string_of_binary_op bin_op))
                end

            | TFunction -> raise (Type_invalid_operator_error (pretty_string_of_data_type t1, Ast.string_of_binary_op bin_op))

            | TString ->
                begin match bin_op with
                    | Add -> Ast.TString
                    | Div | Sub | Mul -> raise (Type_invalid_operator_error (pretty_string_of_data_type t1, Ast.string_of_binary_op bin_op))
                    | _ -> Ast.TBoolean
                end

and type_check_unary st op exp = let exp_type = type_check_expr st exp in
    match op with
    | Ast.Not ->
        if not (exp_type = TBoolean) then
            raise (Type_invalid_operator_error (pretty_string_of_data_type exp_type, Ast.string_of_unary_op op))
        else
            Ast.TBoolean

    | Ast.Negate ->
        if not (exp_type = TNumber) then
            raise (Type_invalid_operator_error (pretty_string_of_data_type exp_type, Ast.string_of_unary_op op))
        else
            Ast.TNumber


and type_check_expr st (exp: Ast.expr) = match exp.kind with
    | NumLit x -> Ast.TNumber
    | BoolLit x -> Ast.TBoolean
    | StrLit x -> Ast.TString

    | Variable x ->
        begin match get_var_type st x with
            | Some y -> y
            | None -> raise (Type_undeclared_error x)
        end

    | Call (exp, params) -> type_check_expr st exp (* TODO: nesting of this and typecheck params *)

    | Binary (exp1, bin_op, exp2) -> type_check_binary st exp1 bin_op exp2

    | Unary (un_op, exp) -> type_check_unary st un_op exp
    | Assign (var, exp) ->
            let var_type = match get_var_type st var with
                | Some x -> x
                | None -> raise (Type_undeclared_error var)
            in
            let exp_type = type_check_expr st exp in
            if types_match var_type exp_type then
                var_type
            else
                raise (Type_mismatch_error (pretty_string_of_data_type exp_type, pretty_string_of_data_type var_type))

    | FunExpr (params, dt_opt, body) -> type_check_function_block st params dt_opt body; Ast.TFunction
    | Group exp -> type_check_expr st exp

and type_check_statement st (cur_ret_type: Ast.data_type option) (stmt: Ast.statement) = match stmt.kind with
    | IfStmt (exp, body, else_body_op) ->
            let exp_type = type_check_expr st exp in
            begin match exp_type with
                | Ast.TBoolean -> ()
                | _ -> raise (Type_mismatch_error (pretty_string_of_data_type exp_type, pretty_string_of_data_type TBoolean))
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
                | _ -> raise (Type_mismatch_error (pretty_string_of_data_type exp_type, pretty_string_of_data_type Ast.TBoolean))
            end;

            type_check_block st cur_ret_type body;

    | ReturnStmt exp_op -> ignore (type_check_return st cur_ret_type exp_op);

    | VarDeclStmt (dt, name, exp_op) ->
        insert_st st name (VariableSymbol dt);
        begin match exp_op with
            | Some exp -> let exp_type = type_check_expr st exp in
                if types_match dt exp_type then
                    ()
                else
                    raise (Type_mismatch_error (pretty_string_of_data_type dt, pretty_string_of_data_type exp_type))
            | None -> ()
        end;

    | FunDeclStmt (_name, params, return_type_op, body) -> (* most already logged by first pass *)
            type_check_function_block st params return_type_op body;

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

and type_check st ast = type_check_block st (Some TNumber) ast

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
    let global_scope: scope = { outer = None; tbl =Hashtbl.create 11 } in
    loop global_scope ast;
    print_st global_scope;
    global_scope


and run_type_checking ast =
    let st = collect_declarations ast in
    type_check st ast;
    print_st st

