(* helpers *)
let string_of_rev_char_list chars = chars |> List.rev |> List.to_seq |> String.of_seq

let is_alpha char = let code = Char.code char in
    (code >= 65 && code <= 90)
    || (code >= 97 && code <= 122)
    || (code = 95)

(* core *)
type t = {
    src: string;
    mutable pos: int;
    mutable line: int;
    mutable col: int
}

let create src =
    {src; pos = 0; line = 1; col = 0}

let peek t =
    if t.pos >= String.length t.src then None
    else Some t.src.[t.pos]

let peek_next t =
    if (t.pos + 1) >= String.length t.src then None
    else Some t.src.[t.pos + 1]

let advance_opt t =
    if t.pos >= String.length t.src then None
    else
        let c = t.src.[t.pos] in
        t.pos <- t.pos + 1;
        if c = '\n' then (
            t.line <- t.line + 1;
            t.col <- 0
        ) else (
            t.col <- t.col + 1
        );
        Some c

let advance t =
    let c = t.src.[t.pos] in
    t.pos <- t.pos + 1;
    if c = '\n' then (
        t.line <- t.line + 1;
        t.col <- 0
    ) else (
        t.col <- t.col + 1
    );
    c

let number cur_char lexer =
    let rec loop acc = match peek lexer with
        | Some '0'..'9' -> loop (advance lexer :: acc)
        | Some '.' -> begin match peek_next lexer with
            | Some '0'..'9' -> loop (advance lexer :: acc)
            | _ -> failwith "TODO better error but can't end num on dot"
            end
        | _ -> acc
    in
    let accumulated = loop [cur_char] in
    Lexing_types.Number (accumulated |> string_of_rev_char_list |> float_of_string)

let str lexer =
    let rec loop acc = match advance_opt lexer with
        | Some '"' -> acc
        | Some x -> loop (x :: acc)
        | None -> failwith "TODO better error but incomplete string"
    in
    let accumulated = loop [] in
    Lexing_types.String (string_of_rev_char_list accumulated)

let identifier cur_char lexer =
    let rec loop acc = match peek lexer with
        | Some '0'..'9' -> loop (advance lexer :: acc)
        | Some x -> (if is_alpha x then
                    loop (advance lexer :: acc)
                else
                    acc)
        | None -> acc
    in
    let accumulated = loop [cur_char] in
    let ident = string_of_rev_char_list accumulated in

    match Lexing_types.StringMap.find_opt ident Lexing_types.reserved_words with
        | Some tok -> tok
        | None -> Identifier ident



let rec next_token lexer = match advance_opt lexer with
    | Some x -> (match x with
        (* ignore whitespace *)
        | ' ' | '\n' | '\r' | '\t' -> next_token lexer
        | '(' -> Lexing_types.LParen
        | ')' -> Lexing_types.RParen
        | '{' -> Lexing_types.LBrace
        | '}' -> Lexing_types.RBrace
        | ',' -> Lexing_types.Comma
        | '.' -> Lexing_types.Dot
        | '-' -> Lexing_types.Minus
        | '+' -> Lexing_types.Plus
        | ';' -> Lexing_types.Semicolon
        | '/' -> Lexing_types.Slash
        | '*' -> Lexing_types.Star
        | '!' -> begin match peek lexer with
            | Some '=' -> Lexing_types.BangEqual
            | _ -> Lexing_types.Bang
            end
        | '=' -> begin match peek lexer with
            | Some '=' -> Lexing_types.EqualEqual
            | _ -> Lexing_types.Equal
            end
        | '>' -> begin match peek lexer with
            | Some '=' -> Lexing_types.GreaterEqual
            | _ -> Lexing_types.Greater
            end
        | '<' -> begin match peek lexer with
            | Some '=' -> Lexing_types.LessEqual
            | _ -> Lexing_types.Less
            end

        | '0'..'9' -> number x lexer
        | '"' -> str lexer
        | _ -> (if is_alpha x then identifier x lexer
                else failwith "No valid token start")
        )
    | None -> Lexing_types.EOF
