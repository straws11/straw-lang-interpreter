open Interpret_types

exception Runtime_stdlib_error of string

let () = Printexc.register_printer (function
    | Runtime_stdlib_error s -> Some (Printf.sprintf "RuntimeError: %s" s)
    | _ -> None
)

let rec val_to_str v = match v with
    | VString x -> x
    | VInteger x -> string_of_int x
    | VBoolean x -> string_of_bool x
    | VFloat x -> string_of_float x
    | VArray x -> "[" ^ (String.concat ", " (Array.to_list (Array.map val_to_str x))) ^ "]"
    | VFunction x -> "cannot print function yet.."
    | VUnit -> ""

let print_fn params =
    begin match params with
        | [item] -> print_endline (val_to_str item)
        | []  -> print_endline ""
        | _ -> raise (Runtime_stdlib_error "Too many arguments for print")
    end;
    VUnit

let str_fn params =
    match params with
        | [v] -> VString (val_to_str v)
        | _ -> raise (Runtime_stdlib_error "Incorrect number of args for 'int_to_str'")

let builtin_functions = [
    ("print", VFunction (BuiltinFunction print_fn));
    ("int_to_str", VFunction (BuiltinFunction str_fn));
]

let builtin_symbols = [
    ("print", Semantic_types.FunctionSymbol ([TString], Some TUnit));
    ("int_to_str", Semantic_types.FunctionSymbol ([TInteger], Some TString));
]
