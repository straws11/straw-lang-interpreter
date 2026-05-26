type symbol =
    | VariableSymbol of Ast.data_type
    | FunctionSymbol of Ast.data_type list * Ast.data_type option

type symbol_table = (string, symbol) Hashtbl.t

let insert_st sym_tbl key value = Hashtbl.replace sym_tbl key value

let string_of_symbol sym = match sym with
    | VariableSymbol dt -> "VarSym(" ^ Ast.string_of_data_type dt ^ ")"
    | FunctionSymbol (params, dt_op) -> "FunSym("
        ^ "[" ^ String.concat "," (List.map Ast.string_of_data_type params)
        ^ "],"
        ^ begin match dt_op with
            | Some dt -> Ast.string_of_data_type dt
            | None -> "unit"
            end
        ^ ")"

let print_st st = Hashtbl.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_symbol v)) st
