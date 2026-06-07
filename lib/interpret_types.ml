type function_value =
    (* environment is captured for closures. should later optimize to store only relevant ones *)
    | UserFunction of Ast.parameter list * Ast.data_type * Ast.block * environment
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

and environment = {
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
    "[\n" ^ loop [] params ^ "],"

let rec string_of_function f = match f with
    | UserFunction (params, return, _body, _env) ->
        "function:" ^ string_of_param_list params
        ^ Ast.string_of_data_type return
    | BuiltinFunction (_what) -> "stdlib function"

and string_of_value v = match v with
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

and string_of_value_option v = match v with
    | Some value -> string_of_value value
    | None -> "None"

and string_of_env env =
    let rec loop level scope acc =
        match scope with
        | Some e ->
            let heading = String.make 3 '-'
                ^ "Environment "
                ^ string_of_int level
                ^ String.make 4 '-'
            in

            let tbl =
                Hashtbl.to_seq e.tbl
                |> Seq.map (fun (k, v) -> k ^ " -> " ^ string_of_value_option v)
                |> List.of_seq
                |> String.concat "\n"
            in

            let tail =  String.make 20 '-' ^ "\n" in
            loop (level + 1) e.outer ((String.concat "\n" [heading; tbl; tail]) :: acc)
        | None -> String.concat "\n" (List.rev acc)
    in
    loop 0 (Some env) []

and print_env env =
    print_endline (string_of_env env);
