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

    (* keywords *)
    | And
    | Else
    | Fun
    | For
    | If
    | Or
    | Return
    | Var
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
    | And -> "And"
    | Else -> "Else"
    | Fun -> "Fun"
    | For -> "For"
    | If -> "If"
    | Or -> "Or"
    | Return -> "Return"
    | Var -> "Var"
    | While -> "While"
    | EOF -> "EOF"

let string_of_token_list token_list =
    String.concat " " (List.map string_of_token token_list)

module StringMap = Map.Make(String)
let reserved_words = StringMap.of_seq @@ List.to_seq [
    ("and", And);
    ("else", Else);
    ("fun", Fun);
    ("for", For);
    ("if", If);
    ("or", Or);
    ("return", Return);
    ("var", Var);
    ("while", While);
]
