type symbol =
    | VariableSymbol of Ast.data_type
    | FunctionSymbol of Ast.data_type list * Ast.data_type
    | StructSymbol of (string, Ast.data_type) Hashtbl.t
    | EnumSymbol of string list

type symbol_table = (string, symbol) Hashtbl.t

type scope = {
    outer: scope option;
    tbl: symbol_table;
}

type program_data = {
    current_file_scope: scope;
    modules: symbol_table list;
}


let insert_st cur_scope key value = Hashtbl.replace cur_scope.tbl key value

let lookup_st cur_scope key =
    let rec loop scope =
        let item = Hashtbl.find_opt scope.tbl key in
        match item with
            | Some x -> item
            | None ->
                begin match scope.outer with
                    | Some x -> loop x
                    | None -> None
                end
    in
    loop cur_scope

let string_of_binary_op op = match op with
    | Ast.Add -> "+"
    | Ast.Sub -> "-"
    | Ast.Mul -> "*"
    | Ast.Div -> "/"
    | Ast.Mod -> "%"
    | Ast.EqualOp -> "=="
    | Ast.NotEqual -> "!="
    | Ast.LessOp -> "<"
    | Ast.LessEqualOp -> "<="
    | Ast.GreaterOp -> ">"
    | Ast.GreaterEqualOp -> ">="

and string_of_unary_op op = match op with
    | Ast.Not -> "!"
    | Ast.Negate -> "-"

and string_of_logical_op op = match op with
    | Ast.AndOp -> "and"
    | Ast.OrOp -> "or"

