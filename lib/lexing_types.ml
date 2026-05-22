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
    | Comma
    | Dot
    | Minus
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

    (* literals *)
    | Identifier of string
    | String of string
    | Number of float
    | Boolean of bool

    (* keywords *)
    | And
    | Fn
    | For
    | If
    | Else
    | Or
    | Return
    | Num
    | Str
    | Bool
    | While

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
    | Comma -> "Comma"
    | Dot -> "Dot"
    | Minus -> "Minus"
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
    | Identifier x -> "Identifier(" ^ x ^ ")"
    | String x -> "String(" ^ x ^ ")"
    | Number x -> "Number(" ^ string_of_float x ^ ")"
    | Boolean x -> "Boolean(" ^ string_of_bool x ^ ")"
    | And -> "And"
    | Fn -> "Fn"
    | For -> "For"
    | If -> "If"
    | Else -> "Else"
    | Or -> "Or"
    | Return -> "Return"
    | Num -> "Num"
    | Str -> "Str"
    | Bool -> "Bool"
    | While -> "While"
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
    ("num", Num);
    ("str", Str);
    ("bool", Bool);
    ("while", While);
]
