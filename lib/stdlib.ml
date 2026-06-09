open Interpret_types
open Dbg_prints
open Exceptions

let rec val_to_str v = match v with
    | VString x -> x
    | VInteger x -> string_of_int x
    | VBoolean x -> string_of_bool x
    | VFloat x -> string_of_float x
    | VArray x -> "[" ^ (String.concat ", " (Array.to_list (Array.map val_to_str x))) ^ "]"
    | VFunction x -> begin match x with
        | UserFunction (params, return, _body, env) ->
            "fn ("
            ^ (String.concat ", " (List.map
                (fun (dt, name) -> dbg_string_of_data_type dt ^ " " ^ name)
            params))
            ^ ")"
            ^ " -> " ^ dbg_string_of_data_type return
            ^ "<env omitted>"

        | BuiltinFunction x -> "Stdlib function"
        end
    | VStruct x -> "{" ^ String.concat ", " (
        Hashtbl.to_seq x |> Seq.map (fun (k, v) -> k ^ " = " ^ val_to_str v) |> List.of_seq
        ) ^ "}"
    | VEnumMember (_, _) | VUnit -> ""

let print_fn params =
    begin match params with
        | [item] -> print_endline (val_to_str item)
        | []  -> print_endline ""
        | _ -> raise (Runtime_error "Too many arguments for print")
    end;
    VUnit

let str_fn params =
    match params with
        | [v] -> VString (val_to_str v)
        | _ -> raise (Runtime_error "Incorrect number of args for 'int_to_str'")

let input_fn params =
    match params with
    | [VString prompt] ->
        print_string prompt;
        flush stdout;
        VString (read_line ())
    | _ -> raise (Runtime_error "Missing parameter for 'input'")

let builtin_functions = [
    ("print", VFunction (BuiltinFunction print_fn));
    ("int_to_str", VFunction (BuiltinFunction str_fn));
    ("float_to_str", VFunction (BuiltinFunction str_fn));
    ("bool_to_str", VFunction (BuiltinFunction str_fn));
    (* ("func_to_str", VFunction (BuiltinFunction str_fn)); *)
    ("input", VFunction (BuiltinFunction input_fn));
]

let builtin_symbols = [
    ("print", Semantic_types.FunctionSymbol ([TString], TUnit));
    ("int_to_str", Semantic_types.FunctionSymbol ([TInteger], TString));
    ("float_to_str", Semantic_types.FunctionSymbol ([TFloat], TString));
    ("bool_to_str", Semantic_types.FunctionSymbol ([TBoolean], TString));
    (* ("func_to_str", Semantic_types.FunctionSymbol ([TFunction], TString)); *)
    ("input", Semantic_types.FunctionSymbol ([TString], TString))
]
