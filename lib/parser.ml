open Lexing_types
open Ast
open Exceptions
(*
----EBNF----

    function_params = [ data_type IDENTIFIER ( "," data_type IDENTIFIER )* ]
    function_expr = "func" "(" function_params ")" [ "->" data_type ] block
    function_decl = "func" IDENTIFIER "(" function_params ")" [ "->" data_type ] block
    struct_decl = "struct" IDENTIFIER "{" data_type IDENTIFIER ( "," data_type IDENTIFIER )* "}"
    array_content = "[" [ expr ( "," expr )* ]"]"
    struct_expr = IDENTIFIER "{" [ IDENTIFIER "=" expr ( "," IDENTIFIER "=" expr )* ] "}"
    primary = ( INTEGER | FLOAT | STRING | FORMATTED_STRING | BOOLEAN
            | IDENTIFIER | array_content | function_expr | struct_expr
            | "(" expr ")"
        )
    expr_list = expr ( "," expr )*
    postfix = primary ( "(" expr_list ")" | "[" expr "]" | "." IDENTIFIER )* [ "++" | "--" ]
    unary = ( "!" | "-" ) unary | postfix
    factor = unary ( ( "/" | "*" ) unary )*
    term = factor ( ( "+" | "-" ) factor )*
    comparison = term ( ( ">" | ">=" | "<" | "<=" | "!=" | "==" ) term )*
    logic_and = comparison ( "and" comparison )*
    logic_or = logic_and ( "or" logic_and )*
    assignment = logic_or [ "=" assignment ]
    expr = assignment
    enum_decl = "enum" IDENTIFIER "{" IDENTIFIER ( "," IDENTIFIER )* "}"
    block = "{" ( statement )* "}"
    if = "if" expr block ( "else" "if" expr block )* [ "else" block ]
    for_initializer = [ assignment | declaration ]
    for_condition = [ expr ]
    for_increment = [ expr ]
    for = "for" "(" for_initializer ";" for_condition ";" for_increment ")" block
    while = "while" expr block
    return = "return" [ expr ]
    function_type = "fn" "(" [ data_type ( "," data_type )* ] ")" [ "->" data_type ]
    builtin_data_type = ( int | float | bool | str | function_type )
    data_type = ( builtin_data_type | IDENTIFIER ) ( [ "[" "]" ] )*
    implicit_declaration = "let" IDENTIFIER "=" expr
    declaration = data_type IDENTIFIER [ "=" expr ]
    statement = ( if | for | while | return | declaration
                | implicit_declaration | function_decl | struct_decl
                | enum_decl | expr_stmt
            )
    body = ( statement )*
*)

(* data *)
type t = {
    tokens: Lexing_types.token array;
    mutable pos: int;
}

let create tokens = { tokens; pos = 0 }

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

let retreat parser =
    parser.pos <- parser.pos - 1

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
    | Identifier _ | String _ | FormattedString (_, _) | Boolean _ | Integer _ | FloatPoint _ | Func | LBrace | LBrack | LParen -> true
    | _ -> false

let starts_data_type parser = match peek parser with
    | Some Int | Some Float | Some Str | Some Bool | Some Fn | Some Let -> true
    (* struct and enum var decl starts with 2 identifiers, the type then name OR identifier then '[' for array *)
    | Some Identifier _ ->
        begin match peek_next parser with
            | Some Identifier _ | Some LBrack -> true
            | _ -> false
        end
    | _ -> false

let starts_var_decl parser = match peek parser with
    | Some Let -> true
    | _ -> starts_data_type parser

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

    if starts_data_type parser then
        let dt = parse_data_type parser in
        let id = expect_id parser in
        loop [(dt, id)]
    else if peek parser = Some RParen then
        []
    else
        raise (Parse_error ("Expected type for formal argument", get_token_pos parser))

(*
    function_expr = "func" "(" function_params ")" [ "->" data_type ] block
*)
and parse_function_expr parser : expr =
    let position = get_token_pos parser in
    expect parser Func "Shouldn't happen";
    expect parser LParen "Expected '('";
    let params = parse_function_params parser in
    expect parser RParen "Unclosed function params";
    let dt = if consume parser Arrow then
        parse_data_type parser
    else
        TUnit
    in
    let block = parse_block parser in
    { kind = FunExpr (params, dt, block); pos = position }

(*
    function_decl = "func" IDENTIFIER "(" function_params ")" [ "->" data_type ] block
*)
and parse_function_decl parser =
    let position = get_token_pos parser in
    expect parser Func "Shouldn't happen";
    let id = expect_id parser in
    expect parser LParen "Expected '('";
    let params = parse_function_params parser in
    expect parser RParen "Unclosed function params";
    let dt = if consume parser Arrow then
        parse_data_type parser
    else
        TUnit
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

and parse_formatted_string parser segments variables =
    let rec loop (rem: token list): Ast.expr list = match rem with
        | h :: t -> begin match h.kind with
            | Identifier x -> ({ kind = Variable x; pos = h.pos } :: loop t)
            | _ -> raise (Parse_error ("Only variables accepted in formatted string", get_token_pos parser))
            end
        | [] -> []
    in
    FormattedStringLit (segments, loop variables)

(*
    struct_expr = IDENTIFIER "{" IDENTIFIER "=" expr ( "," IDENTIFIER "=" expr )* "}"
*)
and parse_struct_expr parser =
    let rec loop entries = match peek parser with
        | Some Comma ->
                ignore (advance parser);
                let id = expect_id parser in
                expect parser Equal "Expected '='";
                let expr = parse_expr parser in
                loop ((id, expr) :: entries)
        | Some _ | None -> List.rev entries
    in

    let name = expect_id parser in
    expect parser LBrace "Expected '{' for struct instantiation";
    let id = expect_id parser in
    expect parser Equal "Expected '='";
    let expr = parse_expr parser in
    let content = loop [(id, expr)] in
    expect parser RBrace "Expected '}' for struct instantiation end";
    let ht = Hashtbl.of_seq (List.to_seq content) in
    StructExpr (name, ht)

(*
    primary = INTEGER | FLOAT | STRING | FORMATTED_STRING | BOOLEAN | IDENTIFIER | array_content | function_expr | struct_expr | "(" expr ")"
*)
and parse_primary parser =
    match peek parser with
        | Some Func -> parse_function_expr parser
        | Some tok ->
            let position = get_token_pos parser in
            ignore (advance parser);
            { kind = begin match tok with
            | String x -> StrLit x
            | FormattedString (segs, vars) -> parse_formatted_string parser segs vars
            | Integer x -> IntLit x
            | FloatPoint x -> FloatLit x
            | Boolean x -> BoolLit x
            | Identifier x -> begin match peek parser with
                | Some LBrace ->
                    ignore (retreat parser);
                    parse_struct_expr parser
                | _ -> Variable x
                end
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
    postfix = primary ( "(" expr_list ")" | "[" expr "]" | "." IDENTIFIER )* [ "++" | "--" ]
*)
and parse_postfix parser: Ast.expr =
    let primary = parse_primary parser in
    let p = parse_postfix_tail parser primary in
    match peek parser with
        | Some PlusPlus ->
                let position = get_token_pos parser in
                ignore (advance parser);
                { kind = PostfixInc p; pos = position }

        | Some MinusMinus ->
                let position = get_token_pos parser in
                ignore (advance parser);
                { kind = PostfixDec p; pos = position }

        | _ -> p

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
                parse_postfix_tail parser ({ kind = FieldAccess (inner, id); pos = position })
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
    enum_decl = "enum" IDENTIFIER "{" IDENTIFIER ( "," IDENTIFIER )* "}"
*)
and parse_enum_decl parser =
    let rec loop acc = match peek parser with
        | Some Comma ->
            ignore (advance parser);
            loop (expect_id parser :: acc)
        | _ -> List.rev acc
    in

    let position = get_token_pos parser in
    expect parser Enum "shouldn't happen";
    let enum_type_name = expect_id parser in
    expect parser LBrace "Expected '{' for enum declaration";
    let id = expect_id parser in
    let members = loop [id] in
    expect parser RBrace "Expected '}' for enum declaration close";
    { kind = EnumDeclStmt (enum_type_name, members); pos = position }

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
    if = "if" expr block ( "else" "if" expr block )* [ "else" block ]
*)
and parse_if parser =
    let rec loop (): block option =
        match peek parser with
        | Some Else -> ignore (advance parser);
            begin match peek parser with
            | Some If ->
                let position = get_token_pos parser in
                ignore (advance parser);
                let expr = parse_expr parser in
                let body = parse_block parser in
                Some [{ kind = IfStmt (expr, body, loop ()); pos = position}]
            (* else body *)
            | Some _ -> Some (parse_block parser)
            | None -> failwith "End of input?"
            end
        (* no more else or else if *)
        | Some x -> None
        | None -> failwith "End of input"
    in

    let position = get_token_pos parser in
    expect parser If "Expected start of 'if'";
    let expr = parse_expr parser in
    let then_body = parse_block parser in
    { kind = IfStmt (expr, then_body, loop ()); pos = position }

(*
    for_initializer = [ assignment | declaration | implicit_declaration ]
*)
and parse_for_initializer parser =
    if peek parser = Some Let then
        Some (parse_implicit_declaration parser)

    else if starts_var_decl parser then
        Some (parse_declaration parser)

    else if starts_expr parser then
        Some ({
            kind = ExprStmt (parse_assignment parser); pos = get_token_pos parser
        })
    else
        None

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
    function_type = "fn" "(" [ data_type ( "," data_type )* ] ")" [ "->" data_type ]
*)
and parse_function_type parser =
    let rec loop acc = match peek parser with
        | Some Comma ->
            ignore (advance parser);
            let dt = parse_data_type parser in
            loop (dt :: acc)
        | Some _ -> List.rev acc
        | None -> raise (Parse_error ("Unexpected end of input", get_token_pos parser))
    in

    expect parser Fn "Expected 'fn' keyword";
    expect parser LParen "Expected opening '('";
    let sig_params = if starts_data_type parser then
        let dt = parse_data_type parser in
        loop [dt]
    else
        []
    in
    expect parser RParen "Expected closing ')'";
    let return_dt = if consume parser Arrow then parse_data_type parser else TUnit in
    TFunction (sig_params, return_dt)

(*
    builtin_data_type = ( int | float | bool | str | function_type )
*)
and parse_builtin_data_type parser =
    match advance parser with
        | Some Int -> Some TInteger
        | Some Float -> Some TFloat
        | Some Bool -> Some TBoolean
        | Some Str -> Some TString
        | Some Fn -> ignore (retreat parser);
            Some (parse_function_type parser)
        | _ -> ignore (retreat parser); None

(*
    data_type = ( builtin_data_type | IDENTIFIER ) ( [ "[" "]" ] ) *
*)
and parse_data_type parser =
    let rec loop inner = match peek parser with
        | Some LBrack ->
            ignore (advance parser);
            expect parser RBrack "Unexpected '['";
            TArray (loop inner)
        | _ -> inner
    in

    match parse_builtin_data_type parser with
    (* Some builtin was found *)
    | Some dt -> loop dt
    (*No builtin, let's check other id option*)
    | None ->
        begin match peek parser with
            | Some Identifier named -> ignore (advance parser); loop (TNamed named)
            | Some _ | None -> raise (Parse_error ("Data type expected", get_token_pos parser))
        end

(*
    struct_decl = "struct" IDENTIFIER "{" data_type IDENTIFIER ( "," data_type IDENTIFIER )* "}"
*)
and parse_struct_decl parser =
    let rec loop acc = match peek parser with
        | Some Comma ->
            ignore (advance parser);
            let dt = parse_data_type parser in
            let field_name = expect_id parser in
            loop ((field_name, dt) :: acc)
        | Some _ -> List.rev acc
        | None -> raise (Parse_error ("Unexpected end of input", get_token_pos parser))
    in

    let position = get_token_pos parser in
    expect parser Struct "Shouldn't happen";
    let struct_name = expect_id parser in
    expect parser LBrace "Expected '{' for struct body";
    let dt = parse_data_type parser in
    let field_name = expect_id parser in
    let contents = loop [(field_name, dt)] in
    expect parser RBrace "Expected '}' for struct body end";
    let ht = Hashtbl.of_seq (List.to_seq contents) in
    { kind = StructDeclStmt (struct_name, ht); pos = position }

(*
    implicit_declaration = "let" IDENTIFIER "=" expr
*)
and parse_implicit_declaration parser =
    let position = get_token_pos parser in
    expect parser Let "Expected 'let'";
    let id = expect_id parser in
    expect parser Equal "Expected '='. Implicit variables must be assigned a value";
    let expr = parse_expr parser in
    { kind = VarDeclStmt (TImplicit, id, Some expr); pos = position}

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

(*
    statement = ( if | for | while | return | declaration | implicit_declaration | function_decl | struct_decl | enum_decl | expr_stmt )
 *)
and parse_statement parser = match peek parser with
    | Some If -> parse_if parser
    | Some For -> parse_for parser
    | Some While -> parse_while parser
    | Some Return -> parse_return parser
    | Some Func -> parse_function_decl parser
    | Some Struct -> parse_struct_decl parser
    | Some Let -> parse_implicit_declaration parser
    | Some Enum -> parse_enum_decl parser
    | Some _ when starts_data_type parser -> parse_declaration parser
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
