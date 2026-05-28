open Ast
open Interpret_types

(* help *)

let v_type_to_t_type v_var = match v_var with
    | VInteger _ -> TInteger
    | VFloat _ -> TFloat
    | VBoolean _ -> TBoolean
    | VFunction _ -> TFunction
    | VString _ -> TString
    | VUnit -> failwith "Impossible"

let value_has_right_type v t = match v, t with
    | VInteger _, TInteger -> true
    | VFloat _, TFloat -> true
    | VBoolean _, TBoolean -> true
    | VString _, TString -> true
    | VFunction _, TFunction -> true
    | _ -> false

let types_match t1 t2 = match t1, t2 with
    | VBoolean _, VBoolean _ -> true
    | VInteger _, VInteger _ -> true
    | VFloat _, VFloat _ -> true
    | VString _, VString _ -> true
    | VFunction _, VFunction _ -> true
    | _ -> false

(* return None if variable doesn't exist, and Some if it does. the Some
   contains a Some x if there's an associated value, and None if
   there isn't yet
   *)
let lookup env var =
    let rec loop scope =
        let item = Hashtbl.find_opt scope.tbl var in
        match item with
            | Some Some x -> Some (Some x) (* found & has value *)
            | Some None -> Some None (* found but no assigned value *)
            | _ ->
                begin match scope.outer with
                    | Some e -> loop e
                    | None -> None
                end
    in
    loop env

let insert env name v = Hashtbl.replace env.tbl name (Some v)

let insert_empty env name = Hashtbl.replace env.tbl name None

let update env name new_v =
    let rec loop scope =
        match Hashtbl.find_opt scope.tbl name with
            | Some _ -> Hashtbl.replace scope.tbl name (Some new_v)
            | None ->
                begin match scope.outer with
                | Some e -> loop e
                | None -> ()
                end
    in loop env


(* error *)
exception Type_error of string

    let () = Printexc.register_printer (function
        | Type_error (s) -> Some (Printf.sprintf "TypeError: %s" s)
        | _ -> None
    )

exception Return_exception of value (* used for returning *)
(* core *)

let rec apply_function env (func: function_value) (args: value list) =
    (*let param_types = List.map (fun element -> fst element) func.params in
    List.iter2 (fun actual_val expected_type -> (
        if not (value_has_right_type actual_val expected_type) then
            raise (Type_error "Argument type doesn't match parameter type")
    )) args param_types;*)

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
    insert_params func_scope func.params args;
    print_env func_scope;
    try
        loop func_scope func.body;
        VUnit
    with
        | Return_exception v -> v

and interpret_while env expr body =
    let rec run_loop () =
        let e = interpret_expr env expr in
        match e with
            | VBoolean true ->
                interpret_block env body;
                run_loop ()
            | VBoolean false -> ()

            | _ -> raise (Type_error "Expression should be of type boolean")
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

        | _ -> raise (Type_error "Expression should be of type boolean")


and interpret_expr env (expr: Ast.expr)  = match expr.kind with
    | IntLit x -> VInteger x
    | FloatLit x -> VFloat x
    | BoolLit x -> VBoolean x
    | StrLit x -> VString x

    | Variable x -> begin match lookup env x with
        | Some Some v -> v
        | Some None -> raise (Type_error ("Variable " ^ x ^ " is uninitialized"))
        | _ -> raise (Type_error ("Unknown variable " ^ x))
        end

    | Call (fun_expr, expr_list) ->
        let f = interpret_expr env fun_expr in
        let arg_vals = List.map (interpret_expr env) expr_list in
        begin match f with
            | VFunction x -> apply_function env x arg_vals
            | _ -> raise (Type_error "Not callable")
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
        intepret_logical val1 logical_op val2


    | Assign (var_name, expr) ->
        let v = interpret_expr env expr in
        begin match lookup env var_name with
            | Some Some x when types_match v x -> ()
            (* TODO: here is where type should be checked but i don't have it rn *)
            | Some None -> ()
            | _ -> raise (Type_error "Variable doesn't exist, cannot assign")
        end;
        update env var_name v;
        v

    | FunExpr (parameter_list, data_type, body) ->
        let fun_val: function_value = {
            body = body;
            params = parameter_list;
            return_type = data_type
        } in
        VFunction fun_val

    | Group expr -> interpret_expr env expr

and interpret_binary v1 op v2 = match op with
    | Add -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VInteger (x + y)
        | VFloat x, VFloat y -> VFloat (x +. y)
        | VFloat x, VInteger y -> VFloat (x +. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x +. y)
        | VString x, VString y -> VString (x ^ y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | Sub -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VInteger (x - y)
        | VFloat x, VFloat y -> VFloat (x -. y)
        | VFloat x, VInteger y -> VFloat (x -. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x -. y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | Mul -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VInteger (x * y)
        | VFloat x, VFloat y -> VFloat (x *. y)
        | VFloat x, VInteger y -> VFloat (x *. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x *. y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | Div -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VFloat (Float.of_int x /. (Float.of_int y))
        | VFloat x, VFloat y -> VFloat (x /. y)
        | VFloat x, VInteger y -> VFloat (x /. Float.of_int y)
        | VInteger x, VFloat y -> VFloat (Float.of_int x /. y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | NotEqual -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x != y)
        | VFloat x, VFloat y -> VBoolean (x != y)
        | VBoolean x, VBoolean y -> VBoolean (x != y)
        | VString x, VString y -> VBoolean (not (String.equal x y))
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | EqualOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x = y)
        | VFloat x, VFloat y -> VBoolean (x = y)
        | VBoolean x, VBoolean y -> VBoolean (x = y)
        | VString x, VString y -> VBoolean (String.equal x y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | LessOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x < y)
        | VFloat x, VFloat y -> VBoolean (x < y)
        | VBoolean x, VBoolean y -> VBoolean (x < y)
        | VString x, VString y -> VBoolean (x < y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | LessEqualOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x <= y)
        | VFloat x, VFloat y -> VBoolean (x <= y)
        | VBoolean x, VBoolean y -> VBoolean (x <= y)
        | VString x, VString y -> VBoolean (x <= y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | GreaterOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x > y)
        | VFloat x, VFloat y -> VBoolean (x > y)
        | VBoolean x, VBoolean y -> VBoolean (x > y)
        | VString x, VString y -> VBoolean (x > y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | GreaterEqualOp -> begin match (v1, v2) with
        | VInteger x, VInteger y -> VBoolean (x >= y)
        | VFloat x, VFloat y -> VBoolean (x >= y)
        | VBoolean x, VBoolean y -> VBoolean (x >= y)
        | VString x, VString y -> VBoolean (x >= y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

and interpret_unary op v = match op with
    | Not -> begin match v with
        | VBoolean x -> VBoolean (not x)
        | _ -> raise (Type_error "Can only not booleans")
        end

    | Negate -> begin match v with
        | VInteger x -> VInteger (-x)
        | VFloat x -> VFloat (-.x)
        | _ -> raise (Type_error "Can only negate numbers")
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
                    raise (Type_error ("Incompatible types "
                        ^ string_of_data_type (v_type_to_t_type exp)
                        ^ " and " ^ string_of_data_type dt))
            | None -> insert_empty env name;
            end

    | FunDeclStmt (name, parameter_list, data_type, body) ->
        let fun_val: function_value = {
            body = body;
            params = parameter_list;
            return_type = data_type
        } in
        insert env name (VFunction fun_val);

        (* TODO: don't think this one is right *)
    | ExprStmt expr -> ignore (interpret_expr env expr);

    (* TODO: this is where the env stuff is necessary too *)
    | BlockStmt body -> interpret_block env body

    (* TODO: temp remove *)
    | PrintStmt expr ->
        let e = interpret_expr env expr in
        begin match e with
            | VBoolean b -> print_endline (string_of_bool b)
            | VInteger num -> print_endline (string_of_int num)
            | VFloat num -> print_endline (string_of_float num)
            | VString s -> print_endline s
            | VFunction _ -> raise (Type_error "Unimplemented")
            | VUnit -> raise (Type_error "Cannot print unit type")
        end


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
    print_endline "enter block";
    loop new_scope ast;
    print_endline "After block interpretation";
    print_env new_scope

and collect_statement env (stmt: Ast.statement) = match stmt.kind with
    | Ast.VarDeclStmt (_, name, Some { kind = FunExpr (params, return_op, body); _ }) ->
        let func_val = { params = params; body = body; return_type = return_op } in
        insert env name (VFunction func_val)

    | Ast.VarDeclStmt (dt, name, _expr_op) ->
        insert_empty env name (* insert that the var exists but ignore its type, we will deal later*)

    | Ast.FunDeclStmt (name, params, return_op, body) ->
            let func_val = { params = params; body = body; return_type = return_op } in
            insert env name (VFunction func_val)

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

and interpret ast =
    let rec loop env rem = match rem with
        | h :: t -> interpret_statement env h; loop env t
        | [] -> ()
    in
    let scope = collect_declarations ast in
    loop scope ast

