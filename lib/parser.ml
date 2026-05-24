open Lexing_types
open Ast
(*
----EBNF----

    function_params = [ data_type IDENTIFIER ( "," data_type IDENTIFIER )* ]
    function_expr = "fn" "(" function_params ")" "->" data_type block
    function_decl = "fn" IDENTIFIER "(" function_params ")" "->" data_type block
    primary = NUMBER | STRING | BOOLEAN | IDENTIFIER | function_expr | "(" expr ")"
    expr_list = expr ( "," expr )*
    call = primary ( "(" expr_list ")" )*
    unary = ( "!" | "-" ) unary | call
    factor = unary ( ( "/" | "*" ) unary )*
    term = factor ( ( "+" | "-" ) factor )*
    comparison = term ( ( ">" | ">=" | "<" | "<=" | "!=" | "==" ) term )*
    assignment = IDENTIFIER "=" assignment | comparison
    expr = assignment
    block = "{" ( statement )* "}"
    if = "if" expr block [ "else" block ]
    for_initializer = [ assignment | declaration ]
    for_condition = [ expr ]
    for_increment = [ expr ]
    for = "for" "(" for_initializer ";" for_condition ";" for_increment ")" block
    while = "while" expr block
    return = "return" [ expr ]
    data_type = ( num | bool | str | func )
    declaration = data_type IDENTIFIER [ "=" expr ]
    print = "print" "(" expr ")"
    statement = ( if | for | while | return | declaration | function_decl | expr_stmt | print )
    body = ( statement )*
*)

(* data *)
type t = {
    tokens: Lexing_types.token array;
    mutable pos: int;
}

let create tokens = { tokens; pos = 0 }

exception Parse_error of string * Lexing_types.position

    let () = Printexc.register_printer (function
        | Parse_error (s, pos) -> Some (Printf.sprintf "ParseError: %s at %d:%d" s pos.line pos.column)
        | _ -> None
    )

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

let peek_next parser = if parser.pos + 1 >= Array.length parser.tokens then
        None
    else
        Some parser.tokens.(parser.pos + 1).kind

let advance parser = if parser.pos >= Array.length parser.tokens then
        None
    else
        let tok = parser.tokens.(parser.pos) in
        parser.pos <- parser.pos + 1;
        Some tok.kind

(* peek and advance if expected, else nothing *)
let consume parser expected = match peek parser with
    | Some x when x = expected ->
            ignore (advance parser);
            true
    | _ -> false


let get_err_pos parser = parser.tokens.(parser.pos).pos

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
                raise (Parse_error (msg, get_err_pos parser))
        | None ->
                raise (Parse_error ("Unexpected end of input", get_err_pos parser))

let expect_id parser = match advance parser with
    | Some Identifier x -> x
    | _ -> raise (Parse_error ("Expected identifier", get_err_pos parser))

(* TODO: should this be called starts_call?? *)
let starts_primary tok = match tok with
    | Identifier _ | String _ | Boolean _ | Number _ | Fn | LParen -> true
    | _ -> false

let starts_declaration tok = match tok with
    | Num | Str | Bool | Func -> true
    | _ -> false

let starts_assignment parser = match peek parser with
    | Some Identifier _ -> peek_next parser = Some Equal
    | _ -> false

let starts_expr parser = match peek parser with
    | Some x when starts_primary x -> true
    | Some _ when starts_assignment parser -> true
    | Some Bang | Some Minus -> true
    | _ -> false

(* core *)

(*
    function_params = [ data_type IDENTIFIER ( "," data_type IDENTIFIER )* ]
*)
let rec parse_function_params parser =
    let rec loop acc =
        if consume parser Comma then
            let dt = parse_data_type parser in
            let id = expect_id parser in
            loop ((dt, id) :: acc)
        else
            List.rev acc
    in

    match peek parser with
        | Some x when starts_declaration x ->
                let dt = parse_data_type parser in
                let id = expect_id parser in
                loop [(dt, id)]
        | _ -> []

(*
    function_expr = "fn" "(" function_params ")" "->" data_type block
*)
and parse_function_expr parser =
    expect parser Fn "Shouldn't happen";
    expect parser LParen "Expected '('";
    let params = parse_function_params parser in
    expect parser RParen "Unclosed function params";
    expect parser Arrow "Expected '->' for return type";
    let dt = parse_data_type parser in
    let block = parse_block parser in
    FunExpr (params, dt, block)

(*
    function_decl = "fn" IDENTIFIER "(" function_params ")" "->" data_type block
*)
and parse_function_decl parser =
    expect parser Fn "Shouldn't happen";
    let id = expect_id parser in
    expect parser LParen "Expected '('";
    let params = parse_function_params parser in
    expect parser RParen "Unclosed function params";
    expect parser Arrow "Expect '->' for return type";
    let dt = parse_data_type parser in
    let block = parse_block parser in
    FunDeclStmt (id, params, dt, block)


(*
    primary = NUMBER | STRING | BOOLEAN | IDENTIFIER | function_expr | "(" expr ")"
*)
and parse_primary parser =
    print_endline ("got a primary" ^ string_of_token parser.tokens.(parser.pos).kind );
    match peek parser with
        | Some Fn -> parse_function_expr parser
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

            | x -> raise (Parse_error ("Expected literal or variable, found " ^ string_of_token x,
                    get_err_pos parser))
            end (* match on tok.kind *)
        | None -> failwith "Unexpected end of input"

(*
    expr_list = expr ( "," expr )*
*)
and parse_expr_list parser =
    let rec loop acc =
        print_endline ("uno" ^ string_of_token parser.tokens.(parser.pos).kind);
        if (consume parser Comma) then(
            print_endline ("dos" ^ string_of_token parser.tokens.(parser.pos).kind);
            match peek parser with
                | Some x when starts_expr parser -> loop (parse_expr parser :: acc)
                | _ -> raise (Parse_error ("Expected expression", get_err_pos parser))
        )else(
            print_endline ("nothing");
            acc)
    in
    match peek parser with
        | Some x when starts_expr parser -> List.rev (loop [parse_expr parser])
        | _ -> []


(*
    call = primary ( "(" expr_list ")" )*
*)
and parse_call parser =
    let primary = parse_primary parser in
    parse_call_tail parser primary

and parse_call_tail parser inner =
    match peek parser with
        | Some LParen -> let expr_list =
            ignore (advance parser);
            print_endline ("going to parse expr list");
            parse_expr_list parser in
            expect parser RParen "Unclosed function call";
            parse_call_tail parser (Call (inner, expr_list))
        | _ -> inner

(*
    unary = ( "!" | "-" ) unary
        | call
*)
and parse_unary parser =
    match peek parser with
    | Some tok ->
        begin match get_unary_op tok with
            | Some op ->
                    ignore (advance parser);
                    let un = parse_unary parser in
                    Unary (op, un)
            | None -> parse_call parser
        end
    | None -> failwith "TODO Unexpected end of input. Expected unary."

(*
    factor = unary ( ( "/" | "*" ) unary )*
*)
and parse_factor parser =
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
    assignment = IDENTIFIER "=" assignment | comparison
*)
and parse_assignment parser =
    match peek parser with
        | Some Identifier id ->
            begin match peek_next parser with
                | Some Equal ->
                    ignore (advance parser);
                    ignore (advance parser);
                    let assignment = parse_assignment parser in
                    Assign (id, assignment)
                | _ -> parse_comparison parser
                end
        | _ -> parse_comparison parser

(*
    expr = assignment
*)
and parse_expr parser = parse_assignment parser
    (* match peek parser with *)
    (*     | Some Identifier _ -> begin match peek_next parser with *)
    (*         | Some Equal -> parse_assignment parser *)
    (*         | _ -> parse_comparison parser *)
    (*         end *)
    (*     | _ -> parse_comparison parser *)

(*
    block = "{" ( statement )* "}"
*)
and parse_block parser =
    print_endline ("parsing the stmt block");
    let rec loop acc = match peek parser with
            | Some RBrace -> acc
            | Some x -> let stmt = parse_statement parser in
                loop (stmt :: acc)
            | None -> raise (Parse_error ("Block not closed", get_err_pos parser))
    in

    expect parser LBrace "Expected '{'";

    let body = List.rev (loop []) in

    expect parser RBrace "Expected '}'";
    body (* TODO: this needs some wrapper?? *)


(*
    if = "if" expr "then" block [ "else" block ]
*)
and parse_if parser =
    expect parser If "Expected start of 'if'";
    let expr = parse_expr parser in
    let then_body = parse_block parser in
    match peek parser with
        | Some Else -> ignore (advance parser);
            let else_body = parse_block parser in
            IfStmt (expr, then_body, Some else_body)

        | _ -> IfStmt (expr, then_body, None)

(*
    for_initializer = [ assignment | declaration ]
*)
and parse_for_initializer parser =
    match peek parser with
        | Some x when starts_declaration x -> Some (parse_declaration parser)
        | Some x when starts_assignment parser -> Some (ExprStmt (parse_assignment parser))
        | _ -> None

(*
    for_condition = [ expr ]
*)
and parse_for_condition parser =
    if starts_expr parser then
        Some (parse_expr parser)
    else
        None

(*
    for_increment = [ expr ]
*)
and parse_for_increment parser =
    if starts_expr parser then
        Some (parse_expr parser)
    else
        None

(*
    for = "for" "(" for_initializer ";" for_condition ";" for_increment ")" block
*)
and parse_for parser =
    expect parser For "Shouldn't be a problem";
    expect parser LParen "Expected '('";

    let for_init = parse_for_initializer parser in
    expect parser Semicolon "Expected ';'";

    let for_cond = parse_for_condition parser in
    expect parser Semicolon "Expected ';'";

    let for_inc = parse_for_increment parser in
    expect parser RParen "Expected ')'";

    let body = parse_block parser in
    print_endline ("do we get here");

    (* now we turn the for into a while:
       finit
       while (fcond) {
        body
        finc
       }
    *)
    let while_body = match for_inc with
        | Some inc -> body @ [ExprStmt inc]
        | None -> body
    in
    let while_cond = match for_cond with
        | Some cond -> cond
        | None -> BoolLit true
    in

    let block = match for_init with
            | Some init -> [init]
            | None -> []
    in

    let block' = block @ [WhileStmt (while_cond, while_body)] in

    BlockStmt (block')


(*
    while = "while" expr "do" block
*)
and parse_while parser =
    expect parser While "This shouldn't ever happen";
    let expr = parse_expr parser in
    WhileStmt (expr, parse_block parser)

(*
    return = "return" [ expr ]
*)
and parse_return parser =
    expect parser Return "This shouldn't ever happen";
    match peek parser with
        | Some x when starts_expr parser ->
            let expr = parse_expr parser in
            ReturnStmt (Some expr)

        | _ -> ReturnStmt None
(*
    data_type = ( num | bool | str | func )
*)
and parse_data_type parser =
    match advance parser with
        | Some Num -> TNumber
        | Some Bool -> TBoolean
        | Some Str -> TString
        | Some Func -> TFunction
        | _ -> raise (Parse_error ("Data type expected", get_err_pos parser))

(*
    declaration = data_type IDENTIFIER [ "=" expr ]
*)
and parse_declaration parser =
    let data_type = parse_data_type parser in
    let id = expect_id parser in
    let init = match peek parser with
        | Some Equal ->
            ignore (advance parser);
            Some (parse_expr parser)
        | _ -> None
    in

    VarDeclStmt (data_type, id, init)

(* TODO: remove, this will be part of std lib
    print = "print" "(" expr ")"
*)

and parse_print parser =
    expect parser Print "Shouldn't happen";
    expect parser LParen "Expected '('";
    let e = parse_expr parser in
    expect parser RParen "Unclosed print, expected ')'";
    PrintStmt e


(*
    statement = ( if | for | while | return | declaration | function_decl | expr_stmt | print )
 *)
and parse_statement parser = match peek parser with
    | Some If -> parse_if parser
    | Some For -> parse_for parser
    | Some While -> parse_while parser
    | Some Return -> parse_return parser
    | Some Fn -> parse_function_decl parser
    | Some Print -> parse_print parser
    | Some x when starts_declaration x -> parse_declaration parser
    | Some x when starts_expr parser -> ExprStmt (parse_expr parser)
    | _ -> raise (Parse_error ("Expected statement", get_err_pos parser))

(*
    body = ( statement )*
 *)
and parse_body parser =
    let rec loop ast =
        if not (peek parser = Some EOF) then (
            let stmt = parse_statement parser in
            loop (stmt :: ast))
        else
            ast
    in
        List.rev (loop [])

let parse parser = parse_body parser
