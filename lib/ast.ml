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

    | Binary of expr * binary_op * expr
    | Unary of unary_op * expr

    | Group of expr

type statement =
    | IfStmt of expr * block * block option (* conditional_expr then_block else_block *)
    | ForStmt
    | WhileStmt of expr * block
    | ReturnStmt of expr
    | AssignStmt of string * expr
    | VarDeclStmt of data_type * string * expr option

type block = statement list


(* stringify *)

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

let rec string_of_expr expr = match expr with
    | NumLit x -> string_of_float x
    | BoolLit x -> string_of_bool x
    | StrLit x -> x
    | Variable x -> x
    | Binary (expr1, bin_op, expr2) -> "(" ^
            string_of_binary_op bin_op ^ " " ^ string_of_expr expr1 ^ " " ^ string_of_expr expr2
            ^ ")"
    | Unary (un_op, expr) -> "(" ^
        string_of_unary_op un_op ^ " " ^ string_of_expr expr
        ^ ")"
    | Group x -> "(" ^ string_of_expr x ^ ")"


let rec string_of_block block = "[" ^ String.concat ", " (List.map string_of_statement block) ^ "]"

and string_of_statement stmt = match stmt with
    | IfStmt e, b, bo -> "If(" ^ string_of_expr e ^ ", Then(" ^ string_of_block b ^ ")"
        ^ begin match bo with
            | Some x -> ", Else(" ^ string_of_block b ^ ")"
            | None -> ""
            end
        ^ ")"
    | WhileStmt e, b -> "While(" ^ string_of_expr e ^ string_of_block b ^ ")"
    | ReturnStmt e -> "Return(" ^ string_of_expr e ^ ")"
    | VarDeclStmt dt, s, eo -> "VarDecl(" ^ string_of_data_type dt ^ s
        ^ begin match eo with
            | Some e -> string_of_expr e
            | None -> ""
            end
        ^ ")"
    | AssignStmt s, e -> "Assign(" ^ s ^ string_of_expr e ^ ")"

