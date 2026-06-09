open Ast
open Interpret_types
open Exceptions

(* help *)
let enum_exists ctx enum_name = match Semantic_types.lookup_st ctx enum_name with
    | Some EnumSymbol _ -> true
    | _ -> false

let rec v_type_to_t_type v_var = match v_var with
    | VInteger _ -> TInteger
    | VFloat _ -> TFloat
    | VBoolean _ -> TBoolean
    | VString _ -> TString
    (* BUG: this next line is probably wrong, what if array is empty *)
    | VArray vals -> TArray (v_type_to_t_type vals.(0))
    | VFunction UserFunction (params, ret, _block, _env) ->
        let param_dts = List.map (fun p -> fst p) params in
        TFunction (param_dts, ret)
    | VFunction BuiltinFunction f -> raise (Runtime_error "unable to can")
    (* BUG: this next line is wrong *)
    | VStruct _ -> TNamed "blahblah"
    | VEnumMember (name, mem_name) -> TNamed name
    | VUnit -> failwith "Impossible"

let rec value_has_right_type ctx env v t =
    (* let rec struct_fields_match v_ht t_ht = *)
    (* in *)
    match v, t with
    | VInteger _, TInteger -> true
    | VFloat _, TFloat -> true
    | VBoolean _, TBoolean -> true
    | VString _, TString -> true
    | VArray x, TArray dt -> value_has_right_type ctx env x.(0) dt
    | VFunction UserFunction (params, ret, _block, _env), TFunction (dts, r) ->
        let contents = List.map2
            Semantic.types_match
            (ret :: (List.map fst params))
            (r :: dts)
        in
        not (List.exists (fun x -> x = false) contents)

    | VStruct x, TNamed y when not (enum_exists ctx y) -> true
        (* begin match lookup env y with *)
        (* | Some  *)
        (*     struct_fields_match x y *)
        (* end *)
    | VEnumMember (name, memb), TNamed type_name when enum_exists ctx type_name -> name = type_name
    | _, TImplicit -> failwith "Shouldn't allow implicit variables here!"
    | _ -> false

let types_match t1 t2 = match t1, t2 with
    | VBoolean _, VBoolean _ -> true
    | VInteger _, VInteger _ -> true
    | VFloat _, VFloat _ -> true
    | VString _, VString _ -> true
    | VFunction _, VFunction _ -> true
    | _ -> false

exception Return_exception of value (* used for returning *)
(* core *)

let rec apply_function ctx env (func: function_value) (args: value list) =
    let rec insert_params env params rem = match params, rem with
        | ph :: pt, h :: t -> insert env (snd ph) h; insert_params env pt t
        | [], [] -> ()
        | _ -> failwith "Impossible"
    in

    let rec loop env stmts = match stmts with
        | h :: t -> interpret_statement ctx env h; loop env t
        | [] -> ()
    in
    begin match func with
        | UserFunction (params, _ret, body, closure_env) ->
            let func_scope: environment = {
                outer = Some closure_env;
                tbl = Hashtbl.create 11
            } in
            insert_params func_scope params args;
            begin try
                loop func_scope body;
                VUnit
            with
                | Return_exception v -> v
            end
        | BuiltinFunction f -> f args
    end

and interpret_while ctx env expr body =
    let rec run_loop () =
        let e = interpret_expr ctx env expr in
        match e with
            | VBoolean true ->
                interpret_block ctx env body;
                run_loop ()
            | VBoolean false -> ()

            | _ -> raise (Runtime_error "Expression should be of type boolean")
    in
    run_loop ()

and interpret_if ctx env expr body else_body =
    match interpret_expr ctx env expr with
        | VBoolean b ->
            if b then
                interpret_block ctx env body
            else
                begin match else_body with
                | Some eb -> interpret_block ctx env eb;
                | None -> ()
                end

        | _ -> raise (Runtime_error "Expression should be of type boolean")

and interpret_index ctx env var idx =
    let arr = match var with
        | VArray vals -> vals
        | _ -> raise (Runtime_error "Shouldn't happen")
    in
    let idx_num = match idx with
        | VInteger x -> x
        | _ -> raise (Runtime_error "Shouldn't happen")
    in

    if idx_num < 0 || idx_num > (Array.length arr) then
        raise (Runtime_error ("Index " ^ string_of_int idx_num ^ " out of bounds of array with size " ^ string_of_int (Array.length arr)))
    else
        arr.(idx_num)

and interpret_assignment ctx env (lhs_expr: Ast.expr) (rhs_expr: Ast.expr) =
    let v = interpret_expr ctx env rhs_expr in
    match lhs_expr.kind with
        | Index (arr_expr, idx_expr) ->
            let arr = interpret_expr ctx env arr_expr in
            let idx = interpret_expr ctx env idx_expr in
            begin match arr, idx with
                | VArray content, VInteger x -> content.(x) <- v
                | _ -> raise (Runtime_error "Not valid indexing")
            end;
            v
        | Variable x -> update env x v; v
        | _ -> raise (Runtime_error "Invalid assignment target")

and interpret_f_string ctx env (expr: Ast.expr) =
    let rec loop rem_s rem_v = match rem_v with
        | h :: t -> (
                let var = interpret_expr ctx env h in
                begin match var with
                    | VString s ->
                        begin match rem_s with
                        | sh :: st -> sh ^ s ^ (loop st t)
                        | [] -> s
                        end
                    | _ -> raise (Runtime_error ("Variable must be of type 'str'"))
                end)
        | [] ->
            begin match rem_s with
            | h :: t -> h ^ (loop t [])
            | [] -> ""
            end
    in
    match expr.kind with
    | FormattedStringLit (segs, vars) ->
            VString (loop segs vars)

    | _ -> failwith "Impossible"


and interpret_struct_access ctx env (expr: Ast.expr) =
    match expr.kind with
    | FieldAccess (exp, id) ->
        let v = interpret_expr ctx env exp in
        begin match v with
            | VStruct ht ->
                begin match Hashtbl.find_opt ht id with
                    | Some v -> v
                    | _ -> failwith "Impossible"
                end
            | _ -> failwith "Impossible"
        end
    | _ -> failwith "Impossible"

and interpret_expr ctx env (expr: Ast.expr)  = match expr.kind with
    | IntLit x -> VInteger x
    | FloatLit x -> VFloat x
    | BoolLit x -> VBoolean x
    | StrLit x -> VString x
    | FormattedStringLit (_, _) -> interpret_f_string ctx env expr
    (* | EnumLit (name, mem_name) -> VEnumMember (name, mem_name) *)
    | ArrayContent x -> VArray (Array.map (interpret_expr ctx env) x)

    | Variable x -> begin match lookup env x with
        | Some Some v -> v
        | Some None -> raise (Runtime_error ("Variable " ^ x ^ " is uninitialized"))
        | _ -> raise (Runtime_error ("Unknown variable " ^ x))
        end

    | Call (fun_expr, expr_list) ->
        let f = interpret_expr ctx env fun_expr in
        let arg_vals = List.map (interpret_expr ctx env) expr_list in
        begin match f with
            | VFunction x -> apply_function ctx env x arg_vals
            | _ -> raise (Runtime_error "Not callable")
        end

    | Index (exp1, exp2) ->
            let var = interpret_expr ctx env exp1 in
            let idx = interpret_expr ctx env exp2 in
            interpret_index ctx env var idx

    | FieldAccess (expr, id) ->
        begin match expr.kind with
            | Variable name when enum_exists ctx name -> VEnumMember (name, id)
            | _ ->
                begin match interpret_expr ctx env expr with
                | VStruct _ -> interpret_struct_access ctx env expr
                | VArray content -> VInteger (Array.length content)
                | _ -> failwith "Shouldn't happen"
                end
        end

    | PostfixInc e ->
        begin match e.kind with
            | Variable name ->
                begin match interpret_expr ctx env e with
                | VInteger i ->
                    update env name (VInteger (i + 1));
                    VInteger (i + 1)
                | _ -> failwith "Shouldn't happen"
                end
            | x -> raise (Runtime_error "Invalid '++' for type - must be variable")
        end

    | PostfixDec e ->
        begin match e.kind with
            | Variable name ->
                begin match interpret_expr ctx env e with
                    | VInteger i ->
                        update env name (VInteger (i - 1));
                        VInteger (i - 1)
                    | _ -> failwith "Shouldn't happen"
                end
            | x -> raise (Runtime_error "Invalid '--' for type - must be variable")
        end

    | Binary (expr1, binary_op, expr2) ->
        let val1 = interpret_expr ctx env expr1 in
        let val2 = interpret_expr ctx env expr2 in
        interpret_binary ctx val1 binary_op val2

    | Unary (unary_op, expr) ->
        let v = interpret_expr ctx env expr in
        interpret_unary ctx unary_op v

    | Logical (expr1, logical_op, expr2) ->
        let val1 = interpret_expr ctx env expr1 in
        let val2 = interpret_expr ctx env expr2 in
        interpret_logical ctx val1 logical_op val2

    | Assign (expr1, expr2) ->
        interpret_assignment ctx env expr1 expr2

    | FunExpr (parameter_list, data_type, body) ->
        let fun_val = UserFunction (parameter_list, data_type, body, env) in
        VFunction fun_val

    | StructExpr (name, ht) ->
        let val_ht = Hashtbl.to_seq ht
            |> Seq.map (fun (name, ex) -> (name, interpret_expr ctx env ex))
            |> Hashtbl.of_seq
        in
        VStruct val_ht


    | Group expr -> interpret_expr ctx env expr

and interpret_binary ctx v1 op v2 =
    match op with
    | Add -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VInteger (x + y)
        | VFloat x, VFloat y -> VFloat (x +. y)
        | VFloat x, VInteger y -> VFloat (x +. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x +. y)
        | VString x, VString y -> VString (x ^ y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | Sub -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VInteger (x - y)
        | VFloat x, VFloat y -> VFloat (x -. y)
        | VFloat x, VInteger y -> VFloat (x -. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x -. y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | Mul -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VInteger (x * y)
        | VFloat x, VFloat y -> VFloat (x *. y)
        | VFloat x, VInteger y -> VFloat (x *. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x *. y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | Div -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VFloat (Float.of_int x /. (Float.of_int y))
        | VFloat x, VFloat y -> VFloat (x /. y)
        | VFloat x, VInteger y -> VFloat (x /. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x /. y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | NotEqual -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x != y)
        | VFloat x, VFloat y -> VBoolean (x != y)
        | VBoolean x, VBoolean y -> VBoolean (x != y)
        | VString x, VString y -> VBoolean (not (String.equal x y))
        | VEnumMember (name, variant), VEnumMember (nameb, variantb) ->
                if name <> nameb then
                    raise (Runtime_error "Invalid operator for types")
                else
                VBoolean (variant != variantb)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | EqualOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x = y)
        | VFloat x, VFloat y -> VBoolean (x = y)
        | VBoolean x, VBoolean y -> VBoolean (x = y)
        | VString x, VString y -> VBoolean (String.equal x y)
        | VEnumMember (name, variant), VEnumMember (nameb, variantb) ->
                if name <> nameb then
                    raise (Runtime_error "Invalid operator for types")
                else
                    VBoolean (variant = variantb)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | LessOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x < y)
        | VFloat x, VFloat y -> VBoolean (x < y)
        | VBoolean x, VBoolean y -> VBoolean (x < y)
        | VString x, VString y -> VBoolean (x < y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | LessEqualOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x <= y)
        | VFloat x, VFloat y -> VBoolean (x <= y)
        | VBoolean x, VBoolean y -> VBoolean (x <= y)
        | VString x, VString y -> VBoolean (x <= y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | GreaterOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x > y)
        | VFloat x, VFloat y -> VBoolean (x > y)
        | VBoolean x, VBoolean y -> VBoolean (x > y)
        | VString x, VString y -> VBoolean (x > y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | GreaterEqualOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x >= y)
        | VFloat x, VFloat y -> VBoolean (x >= y)
        | VBoolean x, VBoolean y -> VBoolean (x >= y)
        | VString x, VString y -> VBoolean (x >= y)
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

and interpret_unary ctx op v = match op with
    | Not -> begin match v with
        | VBoolean x -> VBoolean (not x)
        | _ -> raise (Runtime_error "Can only not booleans")
        end

    | Negate -> begin match v with
        | VInteger x -> VInteger (-x)
        | VFloat x -> VFloat (-.x)
        | _ -> raise (Runtime_error "Can only negate numbers")
        end

and interpret_logical ctx v1 op v2 = match op with
    | AndOp -> begin match v1 with
        (* result of and will be result of 2nd one, given first is true *)
        | VBoolean true -> v2
        | VBoolean false -> VBoolean (false) (* short circuit, don't eval 2nd one *)
        | _ -> failwith "Impossible"
        end
    | OrOp -> begin match v1, v2 with
        | VBoolean x, VBoolean y -> VBoolean (x || y)
        | _ -> failwith "Impossible"
        end

and interpret_statement ctx env (stmt: Ast.statement) = match stmt.kind with
    | IfStmt (expr, body, else_body) -> interpret_if ctx env expr body else_body

    | WhileStmt (expr, body) -> interpret_while ctx env expr body

    | ReturnStmt expr_op -> begin match expr_op with
        | Some expr -> raise (Return_exception (interpret_expr ctx env expr))
        | None -> raise (Return_exception VUnit)
    end

    | VarDeclStmt (dt, name, expr_option) ->
        begin match expr_option with
            | Some e -> let exp = interpret_expr ctx env e in
                if value_has_right_type ctx env exp dt then
                        insert env name exp
                else
                    raise (Runtime_error ("can this error be removed?Incompatible types "
                        ^ string_of_data_type (v_type_to_t_type exp)
                        ^ " and " ^ string_of_data_type dt))

            | None -> insert_empty env name;
            end

    | FunDeclStmt (name, parameter_list, data_type, body) ->
        let fun_val = UserFunction (parameter_list, data_type, body, env) in
        insert env name (VFunction fun_val);

    | StructDeclStmt (type_name, members_ht) -> ()

    (* TODO: this is where the env stuff is necessary too *)
    | BlockStmt body -> interpret_block ctx env body

    | EnumDeclStmt (name, members) -> ()

    (* TODO: don't think this one is right *)
    | ExprStmt expr -> ignore (interpret_expr ctx env expr);


and interpret_block ctx env (ast: block): unit =
    let rec loop scope rem = match rem with
        | h :: t -> (interpret_statement ctx scope h; loop scope t)
        | [] -> ()
    in

    let new_scope = {
            outer = Some env;
            tbl = Hashtbl.create 11;
    }
    in
    (* print_endline "enter block"; *)
    loop new_scope ast;
    (* print_endline "After block interpretation"; *)
    (* print_env new_scope *)

and collect_statement env (stmt: Ast.statement) = match stmt.kind with
    | VarDeclStmt (_, name, Some { kind = FunExpr (params, return_op, body); _ }) ->
        let func_val = UserFunction (params, return_op, body, env) in
        insert env name (VFunction func_val)

    | VarDeclStmt (dt, name, _expr_op) ->
        insert_empty env name (* insert that the var exists but ignore its type, we will deal later*)

    | FunDeclStmt (name, params, return_op, body) ->
            let func_val = UserFunction (params, return_op, body, env) in
            insert env name (VFunction func_val)

    | StructDeclStmt (type_name, ht) -> ()

    | EnumDeclStmt (name, members) -> ()

    | _ -> ()

and collect_declarations ast =
    let rec loop st rem_stmts = match rem_stmts with
        | h :: t -> collect_statement st h; loop st t
        | [] -> ()
    in
    let global_scope: environment = { outer = None; tbl = Hashtbl.create 11 } in
    loop global_scope ast;
    print_endline "After collection:";
    print_env global_scope;
    global_scope

and inject_stdlib st =
    List.iter (fun (name, vf) -> insert st name vf) Stdlib.builtin_functions;

and interpret ctx ast =
    let rec loop env rem = match rem with
        | h :: t -> interpret_statement ctx env h; loop env t
        | [] -> ()
    in
    let scope = collect_declarations ast in
    inject_stdlib scope;
    loop scope ast

