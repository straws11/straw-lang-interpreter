type data_type =
    | TNumber
    | TBoolean
    | TString

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

type expr =
    | NumLit of float
    | BoolLit of bool
    | StrLit of string

    | Variable of string
    | Call of expr * expr list

    | Binary of expr * binary_op * expr
    | Unary of unary_op * expr

    | Assign of string * expr

    | Group of expr


type block = statement list

and statement =
    | IfStmt of expr * block * block option (* conditional_expr then_block else_block *)
    | WhileStmt of expr * block
    | ReturnStmt of expr option
    | VarDeclStmt of data_type * string * expr option
    | ExprStmt of expr (* example foo(1,2) or print(x) are expressions but they are used as statements ofc *)
    | BlockStmt of block

(* stringify *)
let indent n = String.make (n * 2) ' '
let string_of_data_type dt = match dt with
    | TNumber -> "TNumber"
    | TBoolean -> "TBoolean"
    | TString -> "TString"

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

let rec string_of_expr depth expr =
    let ind = indent depth in

    ind ^ match expr with
    | NumLit x -> "NumLit(" ^ string_of_float x ^ ")"
    | BoolLit x -> "BoolLit(" ^ string_of_bool x ^ ")"
    | StrLit x -> "StrLit(" ^ x ^ ")"
    | Variable x -> "Variable(" ^ x ^ ")"

    | Call (expr, expr_list) -> "Call(\n"
        ^ string_of_expr (depth + 1) expr ^ "\n"
        ^ indent (depth + 1) ^ "[\n"
        ^ String.concat ",\n" (List.map (string_of_expr (depth + 2)) expr_list) ^ "\n"
        ^ indent (depth + 1) ^ "]\n"
        ^ ind ^ ")"

    | Binary (expr1, bin_op, expr2) -> "Binary(\n"
        ^ string_of_expr (depth + 1) expr1 ^ "\n"
        ^ indent (depth + 1) ^ string_of_binary_op bin_op ^ "\n"
        ^ string_of_expr (depth + 1) expr2 ^ "\n"
        ^ ind ^ ")"

    | Unary (un_op, expr) -> "Unary(\n"
        ^ string_of_unary_op un_op
        ^ string_of_expr (depth + 1) expr ^ "\n"
        ^ ind ^ ")"

    | Assign (s, e) -> "Assign(\n"
        ^ indent (depth + 1) ^ s ^ "\n"
        ^ string_of_expr (depth + 1) e ^ "\n"
        ^ ind ^ ")"

    | Group x -> "Group(\n"
        ^ string_of_expr (depth + 1) x ^ "\n"
        ^ ind ^ ")"


let rec string_of_block depth block =
    let ind = indent depth in
    "[\n"
    ^ String.concat ",\n" (List.map (string_of_statement (depth + 1)) block) ^ "\n"
    ^ ind ^ "]"

and string_of_statement depth stmt =
    let ind = indent depth in

    ind ^
    match stmt with
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
            ^ indent (depth + 1) ^ string_of_block (depth + 1) b ^ "\n"
            ^ ind ^ ")"

        | ReturnStmt eo -> "Return(\n"
            ^ begin match eo with
                | Some e -> string_of_expr (depth + 1) e ^ "\n"
                | None -> ""
            end
            ^ ind ^ "\n)"

        | VarDeclStmt (dt, s, eo) -> "VarDecl(\n"
            ^ indent (depth + 1) ^ string_of_data_type dt ^ " " ^ s ^ "\n"
            ^ begin match eo with
                | Some e -> string_of_expr (depth + 1) e ^ "\n"
                | None -> ""
                end
            ^ ind ^ ")"

        | ExprStmt e -> "ExprStmt(\n"
            ^ string_of_expr (depth + 1) e ^ "\n"
            ^ ind ^ ")"

        | BlockStmt b -> "BlockStmt(\n"
            ^ string_of_block (depth + 1) b ^ "\n"
            ^ ind ^ ")"

