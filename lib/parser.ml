open Lexing_types
open Ast
(*
----EBNF----

    function_params = [ data_type IDENTIFIER ( "," data_type IDENTIFIER )* ]
    function_expr = "fn" "(" function_params ")" [ "->" data_type ] block
    function_decl = "fn" IDENTIFIER "(" function_params ")" [ "->" data_type ] block
    array_content = "[" [ expr ( "," expr )* ]"]"
    primary = INTEGER | FLOAT | STRING | BOOLEAN | IDENTIFIER | array_content | function_expr | "(" expr ")"
    expr_list = expr ( "," expr )*
    postfix = primary ( "(" expr_list ")" | "[" expr "]" | "." IDENTIFIER )*
    unary = ( "!" | "-" ) unary | postfix
    factor = unary ( ( "/" | "*" ) unary )*
    term = factor ( ( "+" | "-" ) factor )*
    comparison = term ( ( ">" | ">=" | "<" | "<=" | "!=" | "==" ) term )*
    logic_and = comparison ( "and" comparison )*
    logic_or = logic_and ( "or" logic_and )*
    assignment = logic_or [ "=" assignment ]
    expr = assignment
    block = "{" ( statement )* "}"
    if = "if" expr block [ "else" block ]
    for_initializer = [ assignment | declaration ]
    for_condition = [ expr ]
    for_increment = [ expr ]
    for = "for" "(" for_initializer ";" for_condition ";" for_increment ")" block
    while = "while" expr block
    return = "return" [ expr ]
    data_type = ( int | float | bool | str | func ) ( [ "[" "]" ] ) *
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


let get_token_pos parser = parser.tokens.(parser.pos).pos

let expect parser expected msg =
    (* Not sure why we would expect any toks with vals cause we need the vals.. *)
    let token_matches actual expected = begin
        match actual, expected with
            | Identifier _, Identifier _ -> true
            | String _, String _ -> true
            | Integer _, Integer _ -> true
            | FloatPoint _, FloatPoint _ -> true
            | Boolean _, Boolean _ -> true
            | _ -> actual = expected
        end
    in
    match peek parser with
        | Some tok when token_matches tok expected -> ignore (advance parser);

        | Some tok ->
                raise (Parse_error (msg, get_token_pos parser))
        | None ->
                raise (Parse_error ("Unexpected end of input", get_token_pos parser))

let expect_id parser = match advance parser with
    | Some Identifier x -> x
    | _ -> raise (Parse_error ("Expected identifier", get_token_pos parser))

(* TODO: should this be called starts_call?? *)
let starts_primary tok = match tok with
    | Identifier _ | String _ | Boolean _ | Integer _ | FloatPoint _ | Fn | LBrack | LParen -> true
    | _ -> false

let starts_declaration tok = match tok with
    | Int | Float | Str | Bool | Func -> true
    | _ -> false

let starts_expr parser = match peek parser with
    | Some x when starts_primary x -> true
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
        | Some x when x = RParen -> []
        | _ -> raise (Parse_error ("Expected type for formal argument", get_token_pos parser))

(*
    function_expr = "fn" "(" function_params ")" [ "->" data_type ] block
*)
and parse_function_expr parser : expr =
    let position = get_token_pos parser in
    expect parser Fn "Shouldn't happen";
    expect parser LParen "Expected '('";
    let params = parse_function_params parser in
    expect parser RParen "Unclosed function params";
    let dt = if consume parser Arrow then
        Some (parse_data_type parser)
    else
        None
    in
    let block = parse_block parser in
    { kind = FunExpr (params, dt, block); pos = position }

(*
    function_decl = "fn" IDENTIFIER "(" function_params ")" [ "->" data_type ] block
*)
and parse_function_decl parser =
    let position = get_token_pos parser in
    expect parser Fn "Shouldn't happen";
    let id = expect_id parser in
    expect parser LParen "Expected '('";
    let params = parse_function_params parser in
    expect parser RParen "Unclosed function params";
    let dt = if consume parser Arrow then
        Some (parse_data_type parser)
    else
        None
    in
    let block = parse_block parser in
    { kind = FunDeclStmt (id, params, dt, block); pos = position }

(*
    array_content = "[" [ expr ( "," expr )* ] "]"
*)
and parse_array_content parser =
    let rec loop acc =
        if consume parser Comma then
            let p = parse_expr parser in
            loop (p :: acc)
        else
            List.rev acc
    in

    if starts_expr parser then
        let ex = parse_expr parser in
        let contents = loop [ex] in
        expect parser RBrack "Unclosed array";
        ArrayContent (Array.of_list contents)
    else
        raise (Parse_error ("Expected array initializer", get_token_pos parser))

(*
    primary = INTEGER | FLOAT | STRING | BOOLEAN | IDENTIFIER | array_content | function_expr | "(" expr ")"
*)
and parse_primary parser =
    match peek parser with
        | Some Fn -> parse_function_expr parser
        | Some tok ->
            let position = get_token_pos parser in
            ignore (advance parser);
            { kind = begin match tok with
            | String x -> StrLit x
            | Integer x -> IntLit x
            | FloatPoint x -> FloatLit x
            | Boolean x -> BoolLit x
            | Identifier x -> Variable x
            | LBrack -> parse_array_content parser
            | LParen ->
                let expr = parse_expr parser in
                expect parser RParen "Expected ')' after expression";
                Group expr

            | x -> raise (Parse_error ("Expected literal or variable, found " ^ string_of_token x,
                    get_token_pos parser))
            end;
            pos = position }
        | None -> raise (Parse_error ("Unexpected end of input", get_token_pos parser))

(*
    expr_list = expr ( "," expr )*
*)
and parse_expr_list parser =
    let rec loop acc =
        if (consume parser Comma) then(
            match peek parser with
                | Some x when starts_expr parser -> loop (parse_expr parser :: acc)
                | _ -> raise (Parse_error ("Expected expression", get_token_pos parser))
        )else
            acc
    in
    match peek parser with
        | Some x when starts_expr parser -> List.rev (loop [parse_expr parser])
        | _ -> []


(*
    postfix = primary ( "(" expr_list ")" | "[" expr "]" | "." IDENTIFIER )*
*)
and parse_postfix parser =
    let primary = parse_primary parser in
    parse_postfix_tail parser primary

and parse_postfix_tail parser inner =
    match peek parser with
        | Some LParen -> let position = get_token_pos parser in
            let expr_list =
            ignore (advance parser);
            parse_expr_list parser in
            expect parser RParen "Unclosed function call";
            parse_postfix_tail parser ({ kind = Call (inner, expr_list); pos = position })
        | Some LBrack ->
            let position = get_token_pos parser in
            let expr = ignore (advance parser);
            parse_expr parser in
            expect parser RBrack "Unclosed array index";
            parse_postfix_tail parser ({ kind = Index (inner, expr); pos = position })
        | Some Dot ->
                let position = get_token_pos parser in
                let id = ignore (advance parser); expect_id parser in
                parse_postfix_tail parser ({ kind = StructAccess (inner, id); pos = position })
        | _ -> inner

(*
    unary = ( "!" | "-" ) unary
        | postfix
*)
and parse_unary parser : Ast.expr =
    match peek parser with
    | Some tok ->
        begin match get_unary_op tok with
            | Some op ->
                    let position = get_token_pos parser in
                    ignore (advance parser);
                    let un = parse_unary parser in
                    { kind = Unary (op, un); pos = position }
            | None -> parse_postfix parser
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
                    let position = get_token_pos parser in
                    ignore (advance parser);
                    let right = parse_unary parser in
                    parse_factor_tail parser ({ kind = Binary (left, op, right); pos = position })
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
                    let position = get_token_pos parser in
                    ignore (advance parser);
                    let right = parse_factor parser in
                    parse_term_tail parser ({ kind = Binary (left, op, right); pos = position })
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
                let position = get_token_pos parser in
                ignore (advance parser);
                let right = parse_term parser in
                parse_comparison_tail parser ({ kind = Binary (left, op, right); pos = position })
            | None -> left (* there is a token but it's not a comp op *)
            end
        | None -> left (* there is no token at all *)

(*
    logic_and = comparison ( "and" comparison )*
*)
and parse_logic_and parser =
    let comp = parse_comparison parser in
    parse_logic_and_tail parser comp

and parse_logic_and_tail parser left =
    match peek parser with
        | Some And ->
                let position = get_token_pos parser in
                ignore (advance parser);
                let right = parse_comparison parser in
                parse_logic_and_tail parser { kind = Logical (left, AndOp, right); pos = position}
        | _ -> left

(*
    logic_or = logic_and ( "or" logic_and )*
*)
and parse_logic_or parser =
    let l_and = parse_logic_and parser in
    parse_logic_or_tail parser l_and

and parse_logic_or_tail parser left =
    match peek parser with
        | Some Or ->
                let position = get_token_pos parser in
                ignore (advance parser);
                let right = parse_logic_and parser in
                parse_logic_or_tail parser { kind = Logical (left, OrOp, right); pos = position }
        | _ -> left

(*
    assignment = logic_or [ "=" assignment ]
*)
and parse_assignment parser: Ast.expr =
    let lhs = parse_logic_or parser in
    if peek parser = Some Equal then
        let position = get_token_pos parser in
        ignore (advance parser);
        let rhs = parse_assignment parser in
        match lhs.kind with
        | Variable _ | Index (_, _) -> { kind = Assign (lhs, rhs); pos = position }
        | _ -> raise (Parse_error ("Cannot assign to expr of this kind", lhs.pos))
    else
        lhs

(*
    expr = assignment
*)
and parse_expr parser = parse_assignment parser

(*
    block = "{" ( statement )* "}"
*)
and parse_block parser =
    let rec loop acc = match peek parser with
            | Some RBrace -> acc
            | Some x -> let stmt = parse_statement parser in
                loop (stmt :: acc)
            | None -> raise (Parse_error ("Block not closed", get_token_pos parser))
    in

    expect parser LBrace "Expected '{'";

    let body = List.rev (loop []) in

    expect parser RBrace "Expected '}'";
    body


(*
    if = "if" expr "then" block [ "else" block ]
*)
and parse_if parser =
    let position = get_token_pos parser in
    expect parser If "Expected start of 'if'";
    let expr = parse_expr parser in
    let then_body = parse_block parser in
    match peek parser with
        | Some Else -> ignore (advance parser);
            let else_body = parse_block parser in
            { kind = IfStmt (expr, then_body, Some else_body); pos = position }

        | _ -> { kind = IfStmt (expr, then_body, None); pos = position }

(*
    for_initializer = [ assignment | declaration ]
*)
and parse_for_initializer parser =
    match peek parser with
        | Some x when starts_declaration x -> Some (parse_declaration parser)
        | Some x when starts_expr parser -> Some ({
            kind = ExprStmt (parse_assignment parser); pos = get_token_pos parser
            })
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
    let position = get_token_pos parser in
    expect parser For "Shouldn't be a problem";
    expect parser LParen "Expected '('";

    let for_init = parse_for_initializer parser in
    expect parser Semicolon "Expected ';'";

    let for_cond = parse_for_condition parser in
    expect parser Semicolon "Expected ';'";

    let for_inc = parse_for_increment parser in
    expect parser RParen "Expected ')'";

    let body = parse_block parser in

    (* now we turn the for into a while:
       finit
       while (fcond) {
        body
        finc
       }
    *)
    let while_body = match for_inc with
        | Some inc -> body @ [{ kind = ExprStmt inc; pos = position }]
        | None -> body
    in
    let while_cond = match for_cond with
        | Some cond -> cond
        | None -> { kind = BoolLit true; pos = position }
    in

    let block = match for_init with
            | Some init -> [init]
            | None -> []
    in

    let block' = block @ [{ kind = WhileStmt (while_cond, while_body); pos = position }] in

    { kind = BlockStmt (block'); pos = position }


(*
    while = "while" expr block
*)
and parse_while parser =
    let position = get_token_pos parser in
    expect parser While "This shouldn't ever happen";
    let expr = parse_expr parser in
    { kind = WhileStmt (expr, parse_block parser); pos = position }

(*
    return = "return" [ expr ]
*)
and parse_return parser =
    let unit_return_pos = get_token_pos parser in
    expect parser Return "This shouldn't ever happen";
    match peek parser with
        | Some x when starts_expr parser ->
            let position = get_token_pos parser in
            let expr = parse_expr parser in
            { kind = ReturnStmt (Some expr); pos = position }

        | _ -> { kind = ReturnStmt None; pos = unit_return_pos }
(*
    data_type = ( int | float | bool | str | func ) ( [ "[" "]" ] ) *
*)
and parse_data_type parser =
    let rec loop inner = match peek parser with
        | Some LBrack ->
            ignore (advance parser);
            expect parser RBrack "Unexpected '['";
            TArray (loop inner)
        | _ -> inner
    in

    let base_type = match advance parser with
        | Some Int -> TInteger
        | Some Float -> TFloat
        | Some Bool -> TBoolean
        | Some Str -> TString
        | Some Func -> TFunction
        | _ -> raise (Parse_error ("Data type expected", get_token_pos parser))
    in
    loop base_type

(*
    declaration = data_type IDENTIFIER [ "=" expr ]
*)
and parse_declaration parser =
    let data_type = parse_data_type parser in
    let position = get_token_pos parser in
    let id = expect_id parser in
    let init = match peek parser with
        | Some Equal ->
            ignore (advance parser);
            Some (parse_expr parser)
        | _ -> None
    in

    { kind = VarDeclStmt (data_type, id, init); pos = position }

(* TODO: remove, this will be part of std lib
    print = "print" "(" expr ")"
*)

and parse_print parser =
    let position = get_token_pos parser in
    expect parser Print "Shouldn't happen";
    expect parser LParen "Expected '('";
    let e = parse_expr parser in
    expect parser RParen "Unclosed print, expected ')'";
    { kind = PrintStmt e; pos = position }


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
    | Some x when starts_expr parser -> {
            kind = ExprStmt (parse_expr parser); pos = get_token_pos parser
        }
    | _ -> raise (Parse_error ("Expected statement", get_token_pos parser))

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
