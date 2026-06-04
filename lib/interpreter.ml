open Ast
open Interpret_types
open Exceptions

(* help *)

let rec v_type_to_t_type v_var = match v_var with
    | VInteger _ -> TInteger
    | VFloat _ -> TFloat
    | VBoolean _ -> TBoolean
    | VString _ -> TString
    (* BUG: this next line is probably wrong, what if array is empty *)
    | VArray vals -> TArray (v_type_to_t_type vals.(0))
    | VFunction _ -> TFunction
    | VStruct _ -> TStruct
    | VUnit -> failwith "Impossible"

let rec value_has_right_type v t = match v, t with
    | VInteger _, TInteger -> true
    | VFloat _, TFloat -> true
    | VBoolean _, TBoolean -> true
    | VString _, TString -> true
    | VArray x, TArray dt -> value_has_right_type x.(0) dt
    | VFunction _, TFunction -> true
    | VStruct _, TStruct -> true
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

let rec apply_function env (func: function_value) (args: value list) =
    let rec insert_params env params rem = match params, rem with
        | ph :: pt, h :: t -> insert env (snd ph) h; insert_params env pt t
        | [], [] -> ()
        | _ -> failwith "Impossible"
    in

    let rec loop env stmts = match stmts with
        | h :: t -> interpret_statement env h; loop env t
        | [] -> ()
    in
    let func_scope: environment = { outer = Some env; tbl = Hashtbl.create 11 } in
    begin match func with
        | UserFunction (params, _ret, _body) -> insert_params func_scope params args;
        | BuiltinFunction _f -> ();
    end;
    print_env func_scope;
    match func with
        | UserFunction (_params, _ret, body) ->
            begin try
                loop func_scope body;
                VUnit
            with
                | Return_exception v -> v
            end
        | BuiltinFunction f -> f args

and interpret_while env expr body =
    let rec run_loop () =
        let e = interpret_expr env expr in
        match e with
            | VBoolean true ->
                interpret_block env body;
                run_loop ()
            | VBoolean false -> ()

            | _ -> raise (Runtime_error "Expression should be of type boolean")
    in
    run_loop ()

and interpret_if env expr body else_body =
    match interpret_expr env expr with
        | VBoolean b ->
            if b then
                interpret_block env body
            else
                begin match else_body with
                | Some eb -> interpret_block env eb;
                | None -> ()
                end

        | _ -> raise (Runtime_error "Expression should be of type boolean")

and interpret_index env var idx =
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

and interpret_assignment env (lhs_expr: Ast.expr) (rhs_expr: Ast.expr) =
    let v = interpret_expr env rhs_expr in
    match lhs_expr.kind with
        | Index (arr_expr, idx_expr) ->
            let arr = interpret_expr env arr_expr in
            let idx = interpret_expr env idx_expr in
            begin match arr, idx with
                | VArray content, VInteger x -> content.(x) <- v
                | _ -> raise (Runtime_error "Not valid indexing")
            end;
            v
        | Variable x -> update env x v; v
        | _ -> raise (Runtime_error "Invalid assignment target")

and interpret_f_string env (expr: Ast.expr) =
    let rec loop rem_s rem_v = match rem_v with
        | h :: t -> (
                let var = interpret_expr env h in
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



and interpret_expr env (expr: Ast.expr)  = match expr.kind with
    | IntLit x -> VInteger x
    | FloatLit x -> VFloat x
    | BoolLit x -> VBoolean x
    | StrLit x -> VString x
    | FormattedStringLit (_, _) -> interpret_f_string env expr
    | ArrayContent x -> VArray (Array.map (interpret_expr env) x)

    | Variable x -> begin match lookup env x with
        | Some Some v -> v
        | Some None -> raise (Runtime_error ("Variable " ^ x ^ " is uninitialized"))
        | _ -> raise (Runtime_error ("Unknown variable " ^ x))
        end

    | Call (fun_expr, expr_list) ->
        let f = interpret_expr env fun_expr in
        let arg_vals = List.map (interpret_expr env) expr_list in
        begin match f with
            | VFunction x -> apply_function env x arg_vals
            | _ -> raise (Runtime_error "Not callable")
        end

    | Index (exp1, exp2) ->
            let var = interpret_expr env exp1 in
            let idx = interpret_expr env exp2 in
            interpret_index env var idx

    | StructAccess (exp, id) -> raise (Runtime_error "Unimplemented")
    | ArrayLength e ->
        let arr = interpret_expr env e in
        begin match arr with
            | VArray content -> VInteger (Array.length content)
            | _ -> failwith "Shouldn't happen"
        end


    | Binary (expr1, binary_op, expr2) ->
        let val1 = interpret_expr env expr1 in
        let val2 = interpret_expr env expr2 in
        interpret_binary val1 binary_op val2

    | Unary (unary_op, expr) ->
        let v = interpret_expr env expr in
        interpret_unary unary_op v

    | Logical (expr1, logical_op, expr2) ->
        let val1 = interpret_expr env expr1 in
        let val2 = interpret_expr env expr2 in
        interpret_logical val1 logical_op val2

    | Assign (expr1, expr2) ->
        interpret_assignment env expr1 expr2

    | FunExpr (parameter_list, data_type, body) ->
        let fun_val = UserFunction (parameter_list, data_type, body) in
        VFunction fun_val

    | StructExpr (name, ht) ->
        let val_ht = Hashtbl.to_seq ht
            |> Seq.map (fun (name, ex) -> (name, interpret_expr env ex))
            |> Hashtbl.of_seq
        in
        VStruct val_ht


    | Group expr -> interpret_expr env expr

and interpret_binary v1 op v2 = match op with
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
        | _ -> raise (Runtime_error "Invalid operator for types")
        end

    | EqualOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x = y)
        | VFloat x, VFloat y -> VBoolean (x = y)
        | VBoolean x, VBoolean y -> VBoolean (x = y)
        | VString x, VString y -> VBoolean (String.equal x y)
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

and interpret_unary op v = match op with
    | Not -> begin match v with
        | VBoolean x -> VBoolean (not x)
        | _ -> raise (Runtime_error "Can only not booleans")
        end

    | Negate -> begin match v with
        | VInteger x -> VInteger (-x)
        | VFloat x -> VFloat (-.x)
        | _ -> raise (Runtime_error "Can only negate numbers")
        end

and interpret_logical v1 op v2 = match op with
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

and interpret_statement env (stmt: Ast.statement) = match stmt.kind with
    | IfStmt (expr, body, else_body) -> interpret_if env expr body else_body

    | WhileStmt (expr, body) -> interpret_while env expr body

    | ReturnStmt expr_op -> begin match expr_op with
        | Some expr -> raise (Return_exception (interpret_expr env expr))
        | None -> raise (Return_exception VUnit)
    end

    | VarDeclStmt (dt, name, expr_option) ->
        begin match expr_option with
            | Some e -> let exp = interpret_expr env e in
                if value_has_right_type exp dt then
                        insert env name exp
                else
                    raise (Runtime_error ("Incompatible types "
                        ^ string_of_data_type (v_type_to_t_type exp)
                        ^ " and " ^ string_of_data_type dt))

            | None -> insert_empty env name;
            end

    | FunDeclStmt (name, parameter_list, data_type, body) ->
        let fun_val = UserFunction (parameter_list, data_type, body) in
        insert env name (VFunction fun_val);

    | StructDeclStmt (type_name, members_ht) -> ()

        (* TODO: don't think this one is right *)
    | ExprStmt expr -> ignore (interpret_expr env expr);

    (* TODO: this is where the env stuff is necessary too *)
    | BlockStmt body -> interpret_block env body

and interpret_block env (ast: block): unit =
    let rec loop scope rem = match rem with
        | h :: t -> (interpret_statement scope h; loop scope t)
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
        let func_val = UserFunction (params, return_op, body) in
        insert env name (VFunction func_val)

    | VarDeclStmt (dt, name, _expr_op) ->
        insert_empty env name (* insert that the var exists but ignore its type, we will deal later*)

    | FunDeclStmt (name, params, return_op, body) ->
            let func_val = UserFunction (params, return_op, body) in
            insert env name (VFunction func_val)

    | StructDeclStmt (type_name, ht) -> ()

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

and interpret ast =
    let rec loop env rem = match rem with
        | h :: t -> interpret_statement env h; loop env t
        | [] -> ()
    in
    let scope = collect_declarations ast in
    inject_stdlib scope;
    loop scope ast

