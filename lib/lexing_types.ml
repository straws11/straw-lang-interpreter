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
    | Percent

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
    | Character of char
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
    | Char
    | Str
    | Bool
    | Func
    | Let
    | Struct
    | Enum
    | Import

    | EOF


and token = {
    kind: token_kind;
    pos: position;
}

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
    ("char", Char);
    ("str", Str);
    ("bool", Bool);
    ("func", Func);
    ("let", Let);
    ("struct", Struct);
    ("enum", Enum);
    ("import", Import);
]
