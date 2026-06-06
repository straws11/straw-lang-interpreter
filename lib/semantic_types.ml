type symbol =
    | VariableSymbol of Ast.data_type
    | FunctionSymbol of Ast.data_type list * Ast.data_type
    | StructSymbol of (string, Ast.data_type) Hashtbl.t

type symbol_table = (string, symbol) Hashtbl.t

type scope = {
    outer: scope option;
    tbl: symbol_table;
}

let string_of_symbol sym = match sym with
    | VariableSymbol dt -> "VarSym(" ^ Ast.string_of_data_type dt ^ ")"
    | FunctionSymbol (params, dt) -> "FunSym("
        ^ "[" ^ String.concat "," (List.map Ast.string_of_data_type params)
        ^ "],"
        ^ Ast.string_of_data_type dt
        ^ ")"
    | StructSymbol members -> "StructSymbol("
        ^ String.concat ", " (
            Hashtbl.to_seq members
            |> Seq.map (fun (name, dt) -> Ast.string_of_data_type dt ^ " " ^ name )
            |> List.of_seq
        )
        ^ ")"

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


let print_st st title =
    let rec loop level scope =
        print_endline (String.make 3 '-' ^ "Environment " ^ string_of_int level ^ String.make 4 '-');
        Hashtbl.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_symbol v ^ "\n")) scope.tbl;
        print_endline (String.make 20 '-' ^ "\n");
        match scope.outer with
            | Some s -> loop (level + 1) s
            | None -> ()
    in
    print_endline title;
    loop 0 st
