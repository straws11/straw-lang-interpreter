open Lexing_types
open Ast
(*
----EBNF----

    primary = NUMBER | STRING | BOOLEAN | "(" expr ")"
    unary = ( "!" | "-" ) unary
        | primary;
    factor = unary ( ( "/" | "*" ) unary )*;
    term = factor ( ( "+" | "-" ) factor )*;
    comparison = term ( ( ">" | ">=" | "<" | "<=" | "!=" | "==" ) term )*;
    expr = comparison;
    body = expr*;
*)

(* data *)
type t = {
    tokens: Lexing_types.token array;
    mutable pos: int;
}

let create tokens = { tokens; pos = 0 }

exception Parse_error of string * Lexing_types.position


(* helper *)
let get_comparison_op tok = match tok with
    | BangEqual -> Some NotEqual
    | EqualEqual -> Some EqualOp
    | Less -> Some LessOp
    | LessEqual -> Some LessEqualOp
    | Greater -> Some GreaterOp
    | GreaterEqual -> Some GreaterEqualOp
    | _ -> None

let get_term_op tok = match tok with
    | Plus -> Some Add
    | Minus -> Some Sub
    | _ -> None

let get_factor_op tok = match tok with
    | Slash -> Some Div
    | Star -> Some Mul
    | _ -> None

let get_unary_op tok = match tok with
    | Minus -> Some Negate
    | Bang -> Some Not
    | _ -> None

let peek parser = if parser.pos >= Array.length parser.tokens then
        None
    else
        Some parser.tokens.(parser.pos).kind

let advance parser = if parser.pos >= Array.length parser.tokens then
        None
    else
        let tok = parser.tokens.(parser.pos) in
        parser.pos <- parser.pos + 1;
        Some tok.kind

let expect parser expected msg =
    (* Not sure why we would expect any toks with vals cause we need the vals.. *)
    let token_matches actual expected = begin
        match actual, expected with
            | Identifier _, Identifier _ -> true
            | String _, String _ -> true
            | Number _, Number _ -> true
            | Boolean _, Boolean _ -> true
            | _ -> actual = expected
        end
    in
    match peek parser with
        | Some tok when token_matches tok expected -> ignore (advance parser);

        | Some tok ->
                let token_pos = parser.tokens.(parser.pos).pos in
                raise (Parse_error (msg, token_pos))
        | None ->
                let token_pos = parser.tokens.(parser.pos).pos in
                raise (Parse_error ("Unexpected end of input", token_pos))

let starts_primary tok = match tok with
    | Identifier _ | String _ | Boolean _ | Number _ | LParen -> true
    | _ -> false

let starts_expr tok = match tok with
    | Bang | Minus -> true
    | x -> starts_primary tok

(* core *)

(*
    primary = NUMBER | STRING | BOOLEAN | "(" expr ")"
*)
let rec parse_primary parser =
    print_endline ("<primary>");
    print_endline (parser.tokens.(parser.pos).kind |> string_of_token);
    match peek parser with
    | Some tok -> ignore (advance parser);
        begin match tok with
        | String x -> StrLit x
        | Number x -> NumLit x
        | Boolean x -> BoolLit x
        | Identifier x -> Variable x
        | LParen ->
            let expr = parse_expr parser in
            expect parser RParen "Expected ')' after expression";
            Group expr

        | _ -> failwith "Invalid token"
        end (* match on tok.kind *)
    | None -> failwith "Unexpected end of input"

(*
    unary = ( "!" | "-" ) unary
        | primary
*)
and parse_unary parser =
    print_endline ("<unary>");
    match peek parser with
    | Some tok ->
            if starts_primary tok then
                parse_primary parser
            else
                begin match get_unary_op tok with
                    | Some op ->
                            ignore (advance parser);
                            let un = parse_unary parser in
                            Unary (op, un)

                    | None -> failwith "Expected unary op but found TODO this"
                end
    | None -> failwith "Unexpected end of input. Expected unary."

(*
    factor = unary ( ( "/" | "*" ) unary )*
*)
and parse_factor parser =
    print_endline ("<factor>");
    let unary = parse_unary parser in
    parse_factor_tail parser unary

and parse_factor_tail parser left =
    match peek parser with
        | Some tok -> begin match get_factor_op tok with
            | Some op ->
                    ignore (advance parser);
                    let right = parse_unary parser in
                    parse_factor_tail parser (Binary (left, op, right))
            | None -> left
            end
        | None -> left

(*
    term = factor ( ( "+" | "-" ) factor )*
*)
and parse_term parser =
    print_endline ("<term>");
    let factor = parse_factor parser in
    parse_term_tail parser factor

and parse_term_tail parser left =
    match peek parser with
        | Some tok -> begin match get_term_op tok with
            | Some op ->
                    ignore (advance parser);
                    let right = parse_factor parser in
                    parse_term_tail parser (Binary (left, op, right))
            | None -> left
            end
        | None -> left

(*
    comparison = term ( ( ">" | ">=" | "<" | "<=" | "!=" | "==" ) term )*
*)
and parse_comparison parser =
    print_endline ("<comparison>");
    let term = parse_term parser in
    parse_comparison_tail parser term

and parse_comparison_tail parser left =
    match peek parser with
        | Some tok -> begin match get_comparison_op tok with
            | Some op ->
                ignore (advance parser);
                let right = parse_term parser in
                parse_comparison_tail parser (Binary (left, op, right))
            | None -> left (* there is a token but it's not a comp op *)
            end
        | None -> left (* there is no token at all *)


(*
    expr = comparison;
*)
and parse_expr parser =
    print_endline ("next expr parsing..");
    parse_comparison parser

(*
    body = expr*
 *)
and parse_body parser =
    let rec loop ast = match peek parser with
        | Some tok when starts_expr tok ->
                let expr = parse_expr parser in
                loop (expr :: ast)
        | Some tok when tok != Lexing_types.EOF -> failwith "Invalid token. Expected start of expression"
        | _ ->
                List.rev ast
    in
        loop []

let parse parser = parse_body parser
