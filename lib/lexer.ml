(* data *)

type t = {
    src: string;
    mutable pos: int;
    mutable line: int;
    mutable col: int
}

let create src =
    {src; pos = 0; line = 1; col = 0}

(* helpers *)
let string_of_rev_char_list chars = chars |> List.rev |> List.to_seq |> String.of_seq

let is_alpha char = let code = Char.code char in
    (code >= 65 && code <= 90)
    || (code >= 97 && code <= 122)
    || (code = 95)

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

let match_char lexer expected = match peek lexer with
    | Some x when x = expected ->
        ignore (advance lexer);
        true
    | _ -> false

let rec skip_single_comment lexer = match peek lexer with
    | Some '\n' -> ignore (advance lexer);
    | None -> ()
    | _ -> ignore (advance lexer); skip_single_comment lexer

let rec skip_multi_comment lexer = match peek lexer with
    | Some '*' -> ignore (advance lexer);
        begin match peek lexer with
            | Some '/' -> ignore (advance lexer);
        | None -> ()
        | _ -> skip_multi_comment lexer
        end
    | None -> ()
    | _ -> ignore (advance lexer); skip_multi_comment lexer

(* core *)

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
    match List.find_opt (fun x -> x = '.') accumulated with
        | Some _ -> Lexing_types.FloatPoint (accumulated |> string_of_rev_char_list |> float_of_string)
        | None -> Lexing_types.Integer (accumulated |> string_of_rev_char_list |> int_of_string)

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
    if ident = "true" then
        Lexing_types.Boolean true
    else if ident = "false" then
        Lexing_types.Boolean false
    else
        match Lexing_types.StringMap.find_opt ident Lexing_types.reserved_words with
            | Some tok -> tok
            | None -> Identifier ident



let next_token lexer =
    let rec next_token_kind lexer = match advance_opt lexer with
        | Some x -> (match x with
            (* ignore whitespace *)
            | ' ' | '\n' | '\r' | '\t' -> next_token_kind lexer
            | '(' -> Lexing_types.LParen
            | ')' -> Lexing_types.RParen
            | '{' -> Lexing_types.LBrace
            | '}' -> Lexing_types.RBrace
            | '[' -> Lexing_types.LBrack
            | ']' -> Lexing_types.RBrack
            | ',' -> Lexing_types.Comma
            | '.' -> Lexing_types.Dot
            | '+' -> Lexing_types.Plus
            | ';' -> Lexing_types.Semicolon
            | '/' -> if match_char lexer '/' then
                    (skip_single_comment lexer;
                    next_token_kind lexer)
                else if match_char lexer '*' then
                    (skip_multi_comment lexer;
                    next_token_kind lexer)
                else
                    Lexing_types.Slash

            | '*' -> Lexing_types.Star
            | '!' -> if match_char lexer '=' then
                    Lexing_types.BangEqual
                else
                    Lexing_types.Bang

            | '=' -> if match_char lexer '=' then
                    Lexing_types.EqualEqual
                else
                    Lexing_types.Equal

            | '>' -> if match_char lexer '=' then
                    Lexing_types.GreaterEqual
                else
                    Lexing_types.Greater

            | '<' -> if match_char lexer '=' then
                    Lexing_types.LessEqual
                else
                    Lexing_types.Less

            | '-' -> if match_char lexer '>' then
                    Lexing_types.Arrow
                else
                    Lexing_types.Minus

            | '0'..'9' -> number x lexer
            | '"' -> str lexer
            | _ -> (if is_alpha x then identifier x lexer
                    else failwith "No valid token start")
            )
        | None -> Lexing_types.EOF
    in
    let kind = next_token_kind lexer in
    let token : Lexing_types.token = {
        Lexing_types.kind = kind;
        Lexing_types.pos = {
            Lexing_types.line = lexer.line;
            Lexing_types.column = lexer.col
            }
        } in
    token
