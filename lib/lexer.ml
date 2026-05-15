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
    | Number of int

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

