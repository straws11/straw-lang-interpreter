type function_value =
        | UserFunction of Ast.parameter list * Ast.data_type * Ast.block
        | BuiltinFunction of (value list -> value)

and value =
        | VInteger of int
        | VFloat of float
        | VBoolean of bool
        | VString of string
        | VArray of value array
        | VFunction of function_value
        | VStruct of (string, value) Hashtbl.t
        | VUnit

type environment = {
        outer: environment option;
        tbl: (string, value option) Hashtbl.t;
}

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


let rec string_of_param_list params =
    let rec loop acc rest = match rest with
        | (dt, id) :: t -> let str =
            Ast.string_of_data_type dt ^ " " in
            loop (str :: acc) t
        | [] -> String.concat "\n" (List.rev acc) ^ "\n"
    in
    "[\n" ^ loop [] params ^ "]\n"

let string_of_function f = match f with
        | UserFunction (params, return, _body) -> "function:" ^ string_of_param_list params ^ Ast.string_of_data_type return
        | BuiltinFunction (_what) -> "stdlib function"

let rec string_of_value v = match v with
        | VInteger f -> string_of_int f
        | VFloat f -> string_of_float f
        | VBoolean b -> string_of_bool b
        | VString s -> s
        | VArray vals -> "[" ^ (String.concat ", " (
                List.map string_of_value (Array.to_list vals)
                )) ^ "]"
        | VFunction f -> string_of_function f
        | VStruct ht -> "{"
                ^ String.concat ", " (
                        Hashtbl.to_seq ht
                        |> Seq.map (fun (name, v) -> name ^ "=" ^ string_of_value v)
                        |> List.of_seq
                ) ^ "}"
        | VUnit -> "unit value"

let string_of_value_option v = match v with
        | Some value -> string_of_value value
        | None -> "None"

let rec print_env env =
        let rec loop level scope =
                print_endline (String.make 3 '-' ^ "Environment " ^ string_of_int level ^ String.make 4 '-');
                Hashtbl.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_value_option v)) scope.tbl;
                print_endline (String.make 20 '-' ^ "\n");
                match scope.outer with
                        | Some e -> loop (level + 1) e
                        | None -> ()
        in
        print_endline ("Env Rn");
        loop 0 env

