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
    | MinusMinus
    | PlusPlus
    | Arrow

    (* literals *)
    | Identifier of string
    | String of string
    | FormattedString of string list * token list
    | FloatPoint of float
    | Integer of int
    | Boolean of bool

    (* keywords *)
    | And
    | Fn
    | For
    | While
    | If
    | Else
    | Or
    | Return
    | Int
    | Float
    | Str
    | Bool
    | Func
    | Let
    | Struct
    | Enum

    | EOF


and token = {
    kind: token_kind;
    pos: position;
}

let rec string_of_token token_type = match token_type with
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
    | MinusMinus -> "MinusMinus"
    | PlusPlus -> "PlusPlus"
    | Arrow -> "Arrow"
    | Identifier x -> "Identifier(" ^ x ^ ")"
    | String x -> "String(" ^ x ^ ")"
    | FormattedString (segs, vars) ->
            "FString("
            ^ (String.concat ", " segs)
            ^ " with "
            ^ (String.concat ", " (List.map (fun t -> string_of_token t.kind) vars))
            ^ ")"
    | Integer x -> "Integer(" ^ string_of_int x ^ ")"
    | FloatPoint x -> "FloatPoint(" ^ string_of_float x ^ ")"
    | Boolean x -> "Boolean(" ^ string_of_bool x ^ ")"
    | And -> "And"
    | Fn -> "Fn"
    | For -> "For"
    | While -> "While"
    | If -> "If"
    | Else -> "Else"
    | Or -> "Or"
    | Return -> "Return"
    | Int -> "Int"
    | Float -> "Float"
    | Str -> "Str"
    | Bool -> "Bool"
    | Func -> "Func"
    | Let -> "Let"
    | Struct -> "Struct"
    | Enum -> "Enum"
    | EOF -> "EOF"

let string_of_token_list token_list =
    String.concat " " (List.map string_of_token token_list)

module StringMap = Map.Make(String)
let reserved_words = StringMap.of_seq @@ List.to_seq [
    ("and", And);
    ("fn", Fn);
    ("for", For);
    ("while", While);
    ("if", If);
    ("else", Else);
    ("or", Or);
    ("return", Return);
    ("int", Int);
    ("float", Float);
    ("str", Str);
    ("bool", Bool);
    ("func", Func);
    ("let", Let);
    ("struct", Struct);
    ("enum", Enum);
]
