open Lexing_types
open Ast
open Semantic_types
open Interpret_types

let lexing_enabled = ref false
let parser_enabled = ref false
let semantic_enabled = ref false
let interpreter_enabled = ref false

let enable_all () =
    lexing_enabled := true;
    parser_enabled := true;
    semantic_enabled := true;
    interpreter_enabled := true

let enable_lexing () = lexing_enabled := true
let disable_lexing () = lexing_enabled := false

let enable_parser () = parser_enabled := true
let disable_parser () = parser_enabled := false

let enable_semantic () = semantic_enabled := true
let disable_semantic () = semantic_enabled := false

let enable_interpreter () = interpreter_enabled := true
let disable_interpreter () = interpreter_enabled := false

let run_lexing_print f =
    if !lexing_enabled then
        f ()
    else
        ()

let run_parser_print f =
    if !parser_enabled then
        f ()
    else
        ()

let run_semantic_print f =
    if !semantic_enabled then
        f ()
    else
        ()

let dbg_print_lex str = run_lexing_print(fun () ->
    print_endline("[DEBUG]: " ^ str)
)

let dbg_print_parser str = run_parser_print (fun () ->
    print_endline ("[DEBUG]: " ^ str)
)

let run_interpreter_print f =
    if !interpreter_enabled then
        f ()
    else
        ()


(* Lexer *)
let rec dbg_string_of_token token_type = match token_type with
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
    | Percent -> "Percent"
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
    | Character x -> "Character(" ^ String.make 1 x ^ ")"
    | String x -> "String(" ^ x ^ ")"
    | FormattedString (segs, vars) ->
            "FString("
            ^ (String.concat ", " segs)
            ^ " with "
            ^ (String.concat ", " (List.map (fun (t: token) -> dbg_string_of_token t.kind) vars))
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
    | Char -> "Char"
    | Bool -> "Bool"
    | Func -> "Func"
    | Let -> "Let"
    | Struct -> "Struct"
    | Enum -> "Enum"
    | Import -> "Import"
    | EOF -> "EOF"

let dbg_string_of_token_list token_list =
    String.concat "\n" (List.map dbg_string_of_token token_list)

let dbg_print_token_list token_list = run_lexing_print (fun () ->
    let tok_kinds = List.map (fun (x: Lexing_types.token) -> x.kind) token_list in
    print_endline ("[" ^ dbg_string_of_token_list tok_kinds ^  "]")
)

(* Ast *)
let indent depth = String.make (depth * 2) ' '

let line depth s = indent depth ^ s

let block depth lines = String.concat "\n" lines

let render_list depth render xs = xs |> List.map (render depth) |> String.concat ",\n"

let rec dbg_string_of_data_type dt = match dt with
    | TInteger -> "TInteger"
    | TFloat -> "TFloat"
    | TBoolean -> "TBoolean"
    | TCharacter -> "TCharacter"
    | TString -> "TString"
    | TArray d -> "TArray of " ^ dbg_string_of_data_type d
    | TNamed name -> "TNamed of " ^ name
    | TFunction (dts, return) -> "TFunction("
        ^ String.concat ", " (List.map dbg_string_of_data_type dts)
        ^ ") -> " ^ dbg_string_of_data_type return
    | TUnit -> "TUnit"
    | TImplicit -> "TImplicit"

and dbg_string_of_param depth (dt, id) =
    line depth (dbg_string_of_data_type dt ^ " " ^ id)

and dbg_string_of_param_list depth params =
    block depth (
        line depth "[" ::
        (List.map (dbg_string_of_param (depth + 1)) params) @
        [line depth "]"]
    )

and dbg_string_of_expr depth (expr: expr) =
    match expr.kind with
    | IntLit x -> line depth ("IntLit(" ^ string_of_int x ^ ")")
    | FloatLit x -> line depth ("FloatLit(" ^ string_of_float x ^ ")")
    | BoolLit x -> line depth ("BoolLit(" ^ string_of_bool x ^ ")")
    | StrLit x -> line depth ("StrLit(" ^ x ^ ")")
    | CharLit x -> line depth ("CharLit(" ^ String.make 1 x ^ ")")
    | FormattedStringLit (segments, vars) ->
            block depth [
                line depth "FStringLit(";
                line depth (String.concat ", " segments);
                line depth (String.concat ", " (List.map (dbg_string_of_expr (depth + 1)) vars));
            ]
    | ArrayContent contents ->
        block depth [
                line depth "ArrayContent(";
                line depth (String.concat ",\n" (Array.to_list (Array.map (dbg_string_of_expr (depth + 1)) contents)));
                line depth ")";
        ]

    | StructExpr (name, ht) ->
        block depth [
            line depth "StructExpr(";
            line depth name;
            line depth (String.concat ", "
                (Hashtbl.to_seq ht |> Seq.map (fun (k, v) -> k ^ "=" ^ (dbg_string_of_expr 0 v)) |> List.of_seq)
            );
            line depth ")"
        ]

    | Variable x -> line depth ("Variable(" ^ x ^ ")")

    | Call (callee, args) ->
        block depth (
            [
                line depth "Call(";
                dbg_string_of_expr (depth + 1) callee;
                line (depth + 1) "Args[";
            ]
            @
            List.map (dbg_string_of_expr (depth + 2)) args
            @
            [
                line (depth + 1) "]";
                line depth ")";
            ]
        )

    | Index (e, y) ->
        block depth [
            line depth "Index(";
            dbg_string_of_expr (depth + 1) e;
            dbg_string_of_expr (depth + 1) y;
            line depth ")"
        ]

    | FieldAccess (e, id) ->
        block depth [
            line depth "FieldAccess(";
            dbg_string_of_expr (depth + 1) e;
            line depth id;
            line depth ")"
        ]

    | PostfixInc e ->
        block depth [
            line depth "PosfixInc(";
            dbg_string_of_expr (depth + 1) e;
            line depth ")";
        ]

    | PostfixDec e ->
        block depth [
            line depth "PosfixDec(";
            dbg_string_of_expr (depth + 1) e;
            line depth ")";
        ]

    | Unary (op, e) ->
        block depth [
            line depth ("Unary(" ^ string_of_unary_op op);
            dbg_string_of_expr (depth + 1) e;
            line depth ")"
        ]

    | Binary (lhs, op, rhs) ->
        block depth [
            line depth ("Binary(" ^ string_of_binary_op op);
            dbg_string_of_expr (depth + 1) lhs;
            dbg_string_of_expr (depth + 1) rhs;
            line depth ")"
        ]

    | Logical (lhs, op, rhs) ->
        block depth [
            line depth ("Logical(" ^ string_of_logical_op op);
            dbg_string_of_expr (depth + 1) lhs;
            dbg_string_of_expr (depth + 1) rhs;
            line depth ")"
        ]

    | Assign (e1, e2) ->
        block depth [
            line depth "Assign(";
            dbg_string_of_expr (depth + 1) e1;
            dbg_string_of_expr (depth + 1) e2;
            line depth ")"
        ]

    | FunExpr (params, return_type, body) ->
        block depth (
            [
                line depth "FunExpr(";
                dbg_string_of_param_list (depth + 1) params;
                line (depth + 1) ("ReturnType(" ^ dbg_string_of_data_type return_type ^ ")");
                string_of_block (depth + 1) body;
                line depth ")";
            ]
        )
    | Group e ->
        block depth [
            line depth "Group(";
            dbg_string_of_expr (depth + 1) e;
            line depth ")"
        ]

and string_of_block depth stmts =
    block depth (
        line depth "Block[" ::
        (List.map (dbg_string_of_statement (depth + 1)) stmts)
        @
        [line depth "]"]
    )

and dbg_string_of_statement depth stmt =
    match stmt.kind with
    | ExprStmt e ->
        block depth [
            line depth "ExprStmt(";
            dbg_string_of_expr (depth + 1) e;
            line depth ")";
        ]

    | ReturnStmt eo ->
        block depth (
            line depth "Return(" ::
            (match eo with
            | Some e -> [dbg_string_of_expr (depth + 1) e]
            | None -> [])
            @
            [line depth ")"]
        )

    | WhileStmt (cond, body) ->
        block depth [
            line depth "While(";
            dbg_string_of_expr (depth + 1) cond;
            string_of_block (depth + 1) body;
            line depth ")";
        ]

    | IfStmt (cond, then_block, else_block) ->
        block depth (
            [
                line depth "If(";
                dbg_string_of_expr (depth + 1) cond;
                line (depth + 1) "Then";
                string_of_block (depth + 2) then_block;
            ]
            @
            (match else_block with
            | Some b ->
                [
                    line (depth + 1) "Else";
                    string_of_block (depth + 2) b;
                ]
            | None -> [])
            @
            [line depth ")"]
        )

    | VarDeclStmt (dt, name, init) ->
        block depth (
            [
                line depth ("VarDecl(" ^ dbg_string_of_data_type dt ^ " " ^ name);
            ]
            @
            (match init with
            | Some e -> [dbg_string_of_expr (depth + 1) e]
            | None -> [])
            @
            [line depth ")"]
        )

    | FunDeclStmt (name, params, return_type, body) ->
        block depth (
            [
                line depth ("FunDecl(" ^ name);
                dbg_string_of_param_list (depth + 1) params;
                line (depth + 1) ("ReturnType(" ^ dbg_string_of_data_type return_type ^ ")");
            ]
        )
    | StructDeclStmt (name, ht) ->
        block depth [
            line depth ("StructDecl(" ^ name);
            dbg_string_of_param_list (depth + 1) (ht
                |> Hashtbl.to_seq
                |> Seq.map (fun (x, y) -> (y, x))
                |> List.of_seq
            );
            line depth ")";
        ]

    | ImportStmt mod_name ->
        block depth [
            line depth ("ImportStmt(" ^ mod_name ^ ")");
        ]

    | BlockStmt b ->
        string_of_block depth b

    | EnumDeclStmt (name, members) ->
        block depth ([
            line depth ("EnumDecl(" ^ name);
        ]
        @
            List.map (line depth) members
        @
        [line depth ")"]
        )

let dbg_print_ast ast = run_parser_print (fun () ->
    let rec loop rem = match rem with
        | h :: t -> dbg_string_of_statement 0 h ^ "\n" ^ loop t
        | [] -> ""
    in
    print_endline (loop ast);
)


(* Semantics *)
let dbg_string_of_symbol sym = match sym with
    | VariableSymbol dt -> "VarSym(" ^ dbg_string_of_data_type dt ^ ")"
    | FunctionSymbol (params, dt) -> "FunSym("
        ^ "[" ^ String.concat "," (List.map dbg_string_of_data_type params)
        ^ "],"
        ^ dbg_string_of_data_type dt
        ^ ")"

    | StructSymbol members -> "StructSymbol("
        ^ String.concat ", " (
            Hashtbl.to_seq members
            |> Seq.map (fun (name, dt) -> dbg_string_of_data_type dt ^ " " ^ name )
            |> List.of_seq
        )
        ^ ")"

    | EnumSymbol members -> "EnumSymbol(" ^ String.concat ", " members ^ ")"

let print_st (st: scope) title = run_semantic_print (fun () ->
    let rec loop level (scope: scope) =
        print_endline (String.make 3 '-' ^ "Environment " ^ string_of_int level ^ String.make 4 '-');
        Hashtbl.iter (fun k v -> print_endline (k ^ " -> " ^ dbg_string_of_symbol v ^ "\n")) scope.tbl;
        print_endline (String.make 20 '-' ^ "\n");
        match scope.outer with
            | Some s -> loop (level + 1) s
            | None -> ()
    in
    print_endline title;
    loop 0 st
)

(* Interpreter *)

let rec dbg_string_of_param_list params =
    let rec loop acc rest = match rest with
        | (dt, id) :: t -> let str =
            dbg_string_of_data_type dt ^ " " in
            loop (str :: acc) t
        | [] -> String.concat "\n" (List.rev acc) ^ "\n"
    in
    "[\n" ^ loop [] params ^ "],"

let rec dbg_string_of_function f = match f with
    | UserFunction (params, return, _body, _env) ->
        "function:" ^ dbg_string_of_param_list params
        ^ dbg_string_of_data_type return
    | BuiltinFunction (_what) -> "stdlib function"

and dbg_string_of_value v = match v with
    | VInteger f -> string_of_int f
    | VFloat f -> string_of_float f
    | VBoolean b -> string_of_bool b
    | VCharacter x -> String.make 1 x
    | VString s -> s
    | VArray vals -> "[" ^ (String.concat ", " (
        List.map dbg_string_of_value (Array.to_list vals)
        )) ^ "]"
    | VFunction f -> dbg_string_of_function f
    | VStruct ht -> "{"
        ^ String.concat ", " (
            Hashtbl.to_seq ht
            |> Seq.map (fun (name, v) -> name ^ "=" ^ dbg_string_of_value v)
            |> List.of_seq
        ) ^ "}"
    | VEnumMember (name, member_name) -> name ^ "." ^ member_name
    | VUnit -> "unit value"

and dbg_string_of_value_option v = match v with
    | Some value -> dbg_string_of_value value
    | None -> "None"

and dbg_string_of_env env =
    let rec loop level scope acc =
        match scope with
        | Some e ->
            let heading = String.make 3 '-'
                ^ "Environment "
                ^ string_of_int level
                ^ String.make 4 '-'
            in

            let tbl =
                Hashtbl.to_seq e.tbl
                |> Seq.map (fun (k, v) -> k ^ " -> " ^ dbg_string_of_value_option v)
                |> List.of_seq
                |> String.concat "\n"
            in

            let tail =  String.make 20 '-' ^ "\n" in
            loop (level + 1) e.outer ((String.concat "\n" [heading; tbl; tail]) :: acc)
        | None -> String.concat "\n" (List.rev acc)
    in
    loop 0 (Some env) []

and dbg_print_env env title = run_interpreter_print (fun () ->
    print_endline title;
    print_endline (dbg_string_of_env env);
)
