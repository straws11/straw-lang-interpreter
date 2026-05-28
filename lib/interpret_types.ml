type function_value = {
        params: Ast.parameter list;
        return_type: Ast.data_type option;
        body: Ast.block;
}

type value =
        | VNumber of float
        | VBoolean of bool
        | VString of string
        | VFunction of function_value
        | VUnit

type environment = {
        outer: environment option;
        tbl: (string, value option) Hashtbl.t;
}

let rec string_of_param_list params =
    let rec loop acc rest = match rest with
        | (dt, id) :: t -> let str =
            Ast.string_of_data_type dt ^ " " in
            loop (str :: acc) t
        | [] -> String.concat "\n" (List.rev acc) ^ "\n"
    in
    "[\n" ^ loop [] params ^ "]\n"

let string_of_function f = "function:" ^ string_of_param_list f.params

let rec string_of_value v = match v with
        | VNumber f -> string_of_float f
        | VBoolean b -> string_of_bool b
        | VString s -> s
        | VFunction f -> string_of_function f
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

