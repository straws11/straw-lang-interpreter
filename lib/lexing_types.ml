type position = {
    line: int;
    column: int;
}

type token_kind =
    (* single char *)
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBrack
    | RBrack
    | Comma
    | Dot
    | Plus
    | Semicolon
    | Slash
    | Star

    (* one or two char tokens *)
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Minus
    | Arrow

    (* literals *)
    | Identifier of string
    | String of string
    | FloatPoint of float
    | Integer of int
    | Boolean of bool

    (* keywords *)
    | And
    | Fn
    | For
    | If
    | Else
    | Or
    | Return
    | Int
    | Float
    | Str
    | Bool
    | Func
    | While
    (* TODO: temp, remove *)
    | Print

    | EOF


type token = {
    kind: token_kind;
    pos: position;
}

let string_of_token token_type = match token_type with
    | LParen -> "LParen"
    | RParen -> "RParen"
    | LBrace -> "LBrace"
    | RBrace -> "RBrace"
    | LBrack -> "LBrack"
    | RBrack -> "RBrack"
    | Comma -> "Comma"
    | Dot -> "Dot"
    | Plus -> "Plus"
    | Semicolon -> "Semicolon"
    | Slash -> "Slash"
    | Star -> "Star"
    | Bang -> "Bang"
    | BangEqual -> "BangEqual"
    | Equal -> "Equal"
    | EqualEqual -> "EqualEqual"
    | Greater -> "Greater"
    | GreaterEqual -> "GreaterEqual"
    | Less -> "Less"
    | LessEqual -> "LessEqual"
    | Minus -> "Minus"
    | Arrow -> "Arrow"
    | Identifier x -> "Identifier(" ^ x ^ ")"
    | String x -> "String(" ^ x ^ ")"
    | Integer x -> "Integer(" ^ string_of_int x ^ ")"
    | FloatPoint x -> "FloatPoint(" ^ string_of_float x ^ ")"
    | Boolean x -> "Boolean(" ^ string_of_bool x ^ ")"
    | And -> "And"
    | Fn -> "Fn"
    | For -> "For"
    | If -> "If"
    | Else -> "Else"
    | Or -> "Or"
    | Return -> "Return"
    | Int -> "Int"
    | Float -> "Float"
    | Str -> "Str"
    | Bool -> "Bool"
    | Func -> "Func"
    | While -> "While"
    | Print -> "Print"
    | EOF -> "EOF"

let string_of_token_list token_list =
    String.concat " " (List.map string_of_token token_list)

module StringMap = Map.Make(String)
let reserved_words = StringMap.of_seq @@ List.to_seq [
    ("and", And);
    ("fn", Fn);
    ("for", For);
    ("if", If);
    ("else", Else);
    ("or", Or);
    ("return", Return);
    ("int", Int);
    ("float", Float);
    ("str", Str);
    ("bool", Bool);
    ("func", Func);
    ("while", While);
    ("print", Print);
]
