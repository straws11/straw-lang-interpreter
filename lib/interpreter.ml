open Ast
open Interpret_types

(* help *)

let v_type_to_t_type v_var = match v_var with
    | VNumber _ -> TNumber
    | VBoolean _ -> TBoolean
    | VFunction _ -> TFunction
    | VString _ -> TString

let value_has_right_type v t = match v, t with
    | VNumber _, TNumber -> true
    | VBoolean _, TBoolean -> true
    | VString _, TString -> true
    | VFunction _, TFunction -> true
    | _ -> false

let types_match t1 t2 = match t1, t2 with
    | VBoolean _, VBoolean _ -> true
    | VNumber _, VNumber _ -> true
    | VString _, VString _ -> true
    | VFunction _, VFunction _ -> true
    | _ -> false

(* env *)
(* TODO: not able to hold info about the data type if it's uninitialized. should store it here..*)
let environment: (string, value option) Hashtbl.t = Hashtbl.create 11

(* return None if variable doesn't exist, and Some if it does. the Some
   contains a Some x if there's an associated value, and None if
   there isn't yet
   *)
let lookup var =
    let item = Hashtbl.find_opt environment var in
    match item with
        | Some Some x -> Some (Some x) (* found & has value *)
        | Some None -> Some None
        | _ -> None

let insert name v = Hashtbl.replace environment name (Some v)

let insert_empty name = Hashtbl.replace environment name None

let update name new_v = Hashtbl.replace environment name (Some new_v)


(* error *)
exception Type_error of string

    let () = Printexc.register_printer (function
        | Type_error (s) -> Some (Printf.sprintf "TypeError: %s" s)
        | _ -> None
    )
(* core *)

let rec apply_function (func: function_value) (args: value list) =
    let param_types = List.map (fun element -> fst element) func.params in
    List.iter2 (fun actual_val expected_type -> (
        if not (value_has_right_type actual_val expected_type) then
            raise (Type_error "Argument type doesn't match parameter type")
    )) args param_types;

    interpret func.body;
    (* TODO: implement some resolution to the return value?? *)
    VNumber 3.4

and interpret_while expr body =
    let rec run_loop () =
        let e = interpret_expr expr in
        match e with
            | VBoolean b ->
                if b then (
                    interpret body;
                    run_loop ()
                )

            | _ -> raise (Type_error "Expression should be of type boolean")
    in
    run_loop ()

and interpret_if expr body else_body =
    match interpret_expr expr with
        | VBoolean b ->
            if b then
                interpret body
            else
                begin match else_body with
                | Some eb -> interpret eb;
                | None -> ()
                end

        | _ -> raise (Type_error "Expression should be of type boolean")


and interpret_expr expr  = match expr with
    | NumLit x -> VNumber x
    | BoolLit x -> VBoolean x
    | StrLit x -> VString x

    | Variable x -> begin match lookup x with
        | Some Some v -> v
        | Some None -> raise (Type_error ("Variable " ^ x ^ " is uninitialized"))
        | _ -> raise (Type_error ("Unknown variable " ^ x))
        end

    | Call (fun_expr, expr_list) ->
        let f = interpret_expr fun_expr in
        let arg_vals = List.map interpret_expr expr_list in
        begin match f with
            | VFunction x -> apply_function x arg_vals
            | _ -> raise (Type_error "Not callable")
        end

    | Binary (expr1, binary_op, expr2) ->
        let val1 = interpret_expr expr1 in
        let val2 = interpret_expr expr2 in
        interpret_binary val1 binary_op val2

    | Unary (unary_op, expr) ->
        let v = interpret_expr expr in
        interpret_unary unary_op v

    | Assign (var_name, expr) ->
        let v = interpret_expr expr in
        begin match lookup var_name with
            | Some Some x when types_match v x -> ()
            (* TODO: here is where type should be checked but i don't have it rn *)
            | Some None -> ()
            | _ -> raise (Type_error "Variable doesn't exist, cannot assign")
        end;
        insert var_name v;
        v

    | FunExpr (parameter_list, data_type, body) ->
        let fun_val: function_value = {
            body = body;
            params = parameter_list
        } in
        VFunction fun_val

    | Group expr -> interpret_expr expr

and interpret_binary v1 op v2 = match op with
    | Add -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VNumber (x +. y)
        | VString x, VString y -> VString (x ^ y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | Sub -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VNumber (x -. y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | Mul -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VNumber (x *. y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | Div -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VNumber (x /. y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | NotEqual -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VBoolean (x != y)
        | VBoolean x, VBoolean y -> VBoolean (x != y)
        | VString x, VString y -> VBoolean (not (String.equal x y))
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | EqualOp -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VBoolean (x = y)
        | VBoolean x, VBoolean y -> VBoolean (x = y)
        | VString x, VString y -> VBoolean (String.equal x y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | LessOp -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VBoolean (x < y)
        | VBoolean x, VBoolean y -> VBoolean (x < y)
        | VString x, VString y -> VBoolean (x < y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | LessEqualOp -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VBoolean (x <= y)
        | VBoolean x, VBoolean y -> VBoolean (x <= y)
        | VString x, VString y -> VBoolean (x <= y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | GreaterOp -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VBoolean (x > y)
        | VBoolean x, VBoolean y -> VBoolean (x > y)
        | VString x, VString y -> VBoolean (x > y)
        | _ -> raise (Type_error "Invalid operator for types")
        end

    | GreaterEqualOp -> begin match (v1, v2) with
        | VNumber x, VNumber y -> VBoolean (x >= y)
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
        | VNumber x -> VNumber (-.x)
        | _ -> raise (Type_error "Can only negate numbers")
        end

and interpret_statement stmt = match stmt with
    | IfStmt (expr, body, else_body) -> interpret_if expr body else_body

    | WhileStmt (expr, body) -> interpret_while expr body

    | ReturnStmt (expr_option) ->
        begin match expr_option with
            | Some x -> let _ = interpret_expr x in
                ()
            | None -> ()
        end


    | VarDeclStmt (dt, name, expr_option) ->
        begin match expr_option with
            | Some e -> let exp = interpret_expr e in
                if value_has_right_type exp dt then
                        insert name exp
                else
                    raise (Type_error ("Incompatible types "
                        ^ string_of_data_type (v_type_to_t_type exp)
                        ^ " and " ^ string_of_data_type dt))
            | None -> insert_empty name;
            end

    | FunDeclStmt (name, parameter_list, data_type, body) ->
        let fun_val: function_value = {
            body = body;
            params = parameter_list;
        } in
        insert name (VFunction fun_val);

        (* TODO: don't think this one is right *)
    | ExprStmt expr -> ignore (interpret_expr expr);

    (* TODO: this is where the env stuff is necessary too *)
    | BlockStmt body -> interpret body

    (* TODO: temp remove *)
    | PrintStmt expr ->
        let e = interpret_expr expr in
        begin match e with
            | VBoolean b -> print_endline (string_of_bool b)
            | VNumber num -> print_endline (string_of_float num)
            | VString s -> print_endline s
            | VFunction _ -> raise (Type_error "Unimplemented")
        end


and interpret (ast: block): unit =
    match ast with
        | h :: t -> interpret_statement h; interpret t
        | [] -> ()

