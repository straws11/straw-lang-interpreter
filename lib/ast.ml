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

type body = expr list

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
