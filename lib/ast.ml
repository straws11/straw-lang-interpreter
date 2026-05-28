type data_type =
    | TInteger
    | TFloat
    | TBoolean
    | TString
    | TFunction
    | TUnit

type binary_op =
    | Add
    | Sub
    | Mul
    | Div
    | NotEqual
    | EqualOp
    | LessOp
    | LessEqualOp
    | GreaterOp
    | GreaterEqualOp

type unary_op =
    | Not
    | Negate

type expr_kind =
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool
    | StrLit of string

    | Variable of string
    | Call of expr * expr list

    | Binary of expr * binary_op * expr
    | Unary of unary_op * expr

    | Assign of string * expr
    | FunExpr of parameter list * data_type option * block

    | Group of expr


and statement_kind =
    | IfStmt of expr * block * block option (* conditional_expr then_block else_block *)
    | WhileStmt of expr * block
    | ReturnStmt of expr option
    | VarDeclStmt of data_type * string * expr option
    | FunDeclStmt of string * parameter list * data_type option * block
    | ExprStmt of expr (* example foo(1,2) or print(x) are expressions but they are used as statements ofc *)
    | BlockStmt of block
    (* TODO: temp remove *)
    | PrintStmt of expr

and expr = {
    kind: expr_kind;
    pos: Lexing_types.position;
}

and statement = {
    kind: statement_kind;
    pos: Lexing_types.position;
}

and parameter = data_type * string

and block = statement list


(* stringify *)
let indent n = String.make (n * 2) ' '

let string_of_data_type dt = match dt with
    | TInteger -> "TInteger"
    | TFloat -> "TFloat"
    | TBoolean -> "TBoolean"
    | TString -> "TString"
    | TFunction -> "TFunction"
    | TUnit -> "TUnit"

let string_of_binary_op op = match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | EqualOp -> "=="
    | NotEqual -> "!="
    | LessOp -> "<"
    | LessEqualOp -> "<="
    | GreaterOp -> ">"
    | GreaterEqualOp -> ">="

let string_of_unary_op op = match op with
    | Not -> "!"
    | Negate -> "-"

let rec string_of_param_list depth params =
    let rec loop acc rest = match rest with
        | (dt, id) :: t -> let str =
            indent (depth + 1)  ^ string_of_data_type dt ^ " " ^ id in
            loop (str :: acc) t
        | [] -> String.concat "\n" (List.rev acc) ^ "\n"
    in
    indent depth ^ "[\n" ^ loop [] params ^ indent (depth) ^ "]\n"

let rec string_of_expr depth expr =
    let ind = indent (depth + 1) in

    indent depth ^ match expr.kind with
    | IntLit x -> "IntLit(" ^ string_of_int x ^ ")"
    | FloatLit x -> "FloatLit(" ^ string_of_float x ^ ")"
    | BoolLit x -> "BoolLit(" ^ string_of_bool x ^ ")"
    | StrLit x -> "StrLit(" ^ x ^ ")"
    | Variable x -> "Variable(" ^ x ^ ")"

    | Call (expr, expr_list) -> "Call(\n"
        ^ string_of_expr (depth + 1) expr ^ ",\n"
        ^ ind ^ "["
        ^ String.concat ", " (List.map (string_of_expr 0) expr_list) ^ "]\n"
        ^ indent depth ^ ")"

    | Binary (expr1, bin_op, expr2) -> "Binary(\n"
        ^ string_of_expr (depth + 1) expr1 ^ "\n"
        ^ ind ^ string_of_binary_op bin_op ^ "\n"
        ^ string_of_expr (depth + 1) expr2 ^ "\n"
        ^ indent depth ^ ")"

    | Unary (un_op, expr) -> "Unary(\n"
        ^ string_of_unary_op un_op
        ^ string_of_expr (depth + 1) expr ^ "\n"
        ^ indent depth ^ ")"

    | Assign (s, e) -> "Assign(\n"
        ^ ind ^ s ^ "\n"
        ^ string_of_expr (depth + 1) e ^ "\n"
        ^ indent depth ^ ")"

    | FunExpr (params, return_type, b) -> "FunExpr(\n"
        ^ string_of_param_list (depth + 1) params
        ^ (match return_type with
            | Some rt -> ind ^ string_of_data_type rt ^ "\n"
            | None -> "")
        ^ string_of_block (depth + 1) b ^ "\n"
        ^ indent depth ^ ")"

    | Group x -> "Group(\n"
        ^ string_of_expr (depth + 1) x ^ "\n"
        ^ ind ^ ")"


and string_of_block depth block =
    let ind = indent depth in
    ind ^ "[\n"
    ^ String.concat ",\n" (List.map (string_of_statement (depth + 1)) block) ^ "\n"
    ^ ind ^ "]"

and string_of_statement depth stmt =
    let ind = indent depth in

    ind ^
    match stmt.kind with
        | IfStmt (e, b, bo) -> "If(\n"
            ^ string_of_expr (depth + 1) e ^ ",\n"
            ^ indent (depth + 1) ^ "Then("
            ^ string_of_block (depth + 1) b
            ^ ")"
            ^ begin match bo with
                | Some x -> ",\n" ^ indent (depth + 1) ^ "Else("
                    ^ string_of_block (depth + 1) x
                    ^ ")\n"
                | None -> "\n"
                end
            ^ ind ^ ")"

        | WhileStmt (e, b) -> "While(\n"
            ^ string_of_expr (depth + 1) e ^ "\n"
            ^ string_of_block (depth + 1) b ^ "\n"
            ^ ind ^ ")"

        | ReturnStmt eo -> "Return(\n"
            ^ begin match eo with
                | Some e -> string_of_expr (depth + 1) e ^ "\n"
                | None -> ""
            end
            ^ ind ^ ")"

        | VarDeclStmt (dt, s, eo) -> "VarDecl(\n"
            ^ indent (depth + 1) ^ string_of_data_type dt ^ " " ^ s ^ "\n"
            ^ begin match eo with
                | Some e -> string_of_expr (depth + 1) e ^ "\n"
                | None -> ""
                end
            ^ ind ^ ")"

        | FunDeclStmt (name, params, return_type, block) -> "FunDeclStmt(\n"
            ^ indent (depth + 1) ^ name ^ ",\n"
            ^ string_of_param_list (depth + 1) params
            ^ (match return_type with
                | Some rt -> indent (depth + 1) ^ string_of_data_type rt ^ ",\n"
                | None -> "")
            ^ string_of_block (depth + 1) block ^ "\n"
            ^ ind ^ ")"


        | ExprStmt e -> "ExprStmt(\n"
            ^ string_of_expr (depth + 1) e ^ "\n"
            ^ ind ^ ")"

        | BlockStmt b -> "BlockStmt(\n"
            ^ string_of_block (depth + 1) b ^ "\n"
            ^ ind ^ ")"

        | PrintStmt e -> "PrintStmt(\n"
            ^ string_of_expr (depth + 1) e ^ "\n"
            ^ ind ^ ")"

