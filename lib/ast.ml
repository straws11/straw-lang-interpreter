type unary_op =
    | Not
    | Negate

type logical_op =
    | AndOp
    | OrOp

type binary_op =
    | Add
    | Sub
    | Mul
    | Div
    | NotEqual
    | EqualOp
    | LessOp
    | LessEqualOp
    | GreaterOp
    | GreaterEqualOp

type data_type =
    | TInteger
    | TFloat
    | TBoolean
    | TString
    | TArray of data_type
    (* don't resolve the full type yet, just keep the expressions *)
    | TFunction of expr list * expr option
    | TStruct of string
    | TUnit

and expr_kind =
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool
    | StrLit of string
    | FormattedStringLit of string list * expr list
    | ArrayContent of expr array
    | StructExpr of string * (string, expr) Hashtbl.t

    | Variable of string
    | Call of expr * expr list
    | Index of expr * expr
    | StructAccess of expr * string
    | ArrayLength of expr

    | PostfixInc of expr
    | PostfixDec of expr

    | Binary of expr * binary_op * expr
    | Unary of unary_op * expr
    | Logical of expr * logical_op * expr

    | Assign of expr * expr
    | FunExpr of parameter list * data_type option * block

    | Group of expr


and statement_kind =
    | IfStmt of expr * block * block option (* conditional_expr then_block else_block *)
    | WhileStmt of expr * block
    | ReturnStmt of expr option
    | VarDeclStmt of data_type * string * expr option
    | FunDeclStmt of string * parameter list * data_type option * block
    | StructDeclStmt of string * (string, data_type) Hashtbl.t
    | ExprStmt of expr (* example foo(1,2) or print(x) are expressions but they are used as statements ofc *)
    | BlockStmt of block

and expr = {
    kind: expr_kind;
    pos: Lexing_types.position;
}

and statement = {
    kind: statement_kind;
    pos: Lexing_types.position;
}

and parameter = data_type * string

and block = statement list


(* stringify *)
let indent depth = String.make (depth * 2) ' '

let line depth s = indent depth ^ s

let block depth lines = String.concat "\n" lines

let render_list depth render xs = xs |> List.map (render depth) |> String.concat ",\n"

let rec string_of_data_type dt = match dt with
    | TInteger -> "TInteger"
    | TFloat -> "TFloat"
    | TBoolean -> "TBoolean"
    | TString -> "TString"
    | TArray d -> "TArray of " ^ string_of_data_type d
    | TStruct name -> "TStruct of " ^ name
    | TFunction (exprs, return)-> "TFunction("
        ^ String.concat ", " (List.map (string_of_expr 0) exprs)
        ^ ") -> " ^ begin match return with | Some x -> string_of_expr 0 x | None -> "unit" end
    | TUnit -> "TUnit"

and string_of_binary_op op = match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | EqualOp -> "=="
    | NotEqual -> "!="
    | LessOp -> "<"
    | LessEqualOp -> "<="
    | GreaterOp -> ">"
    | GreaterEqualOp -> ">="

and string_of_unary_op op = match op with
    | Not -> "!"
    | Negate -> "-"

and string_of_logical_op op = match op with
    | AndOp -> "and"
    | OrOp -> "or"

and string_of_param depth (dt, id) =
    line depth (string_of_data_type dt ^ " " ^ id)

and string_of_param_list depth params =
    block depth (
        line depth "[" ::
        (List.map (string_of_param (depth + 1)) params) @
        [line depth "]"]
    )

and string_of_expr depth expr =
    match expr.kind with
    | IntLit x -> line depth ("IntLit(" ^ string_of_int x ^ ")")
    | FloatLit x -> line depth ("FloatLit(" ^ string_of_float x ^ ")")
    | BoolLit x -> line depth ("BoolLit(" ^ string_of_bool x ^ ")")
    | StrLit x -> line depth ("StrLit(" ^ x ^ ")")
    | FormattedStringLit (segments, vars) ->
            block depth [
                line depth "FStringLit(";
                line depth (String.concat ", " segments);
                line depth (String.concat ", " (List.map (string_of_expr (depth + 1)) vars));
            ]
    | ArrayContent contents ->
        block depth [
                line depth "ArrayContent(";
                line depth (String.concat ",\n" (Array.to_list (Array.map (string_of_expr (depth + 1)) contents)));
                line depth ")";
        ]

    | StructExpr (name, ht) ->
        block depth [
            line depth "StructExpr(";
            line depth name;
            line depth (String.concat ", "
                (Hashtbl.to_seq ht |> Seq.map (fun (k, v) -> k ^ "=" ^ (string_of_expr 0 v)) |> List.of_seq)
            );
            line depth ")"
        ]

    | Variable x -> line depth ("Variable(" ^ x ^ ")")

    | Call (callee, args) ->
        block depth (
            [
                line depth "Call(";
                string_of_expr (depth + 1) callee;
                line (depth + 1) "Args[";
            ]
            @
            List.map (string_of_expr (depth + 2)) args
            @
            [
                line (depth + 1) "]";
                line depth ")";
            ]
        )

    | Index (e, y) ->
        block depth [
            line depth "Index(";
            string_of_expr (depth + 1) e;
            string_of_expr (depth + 1) y;
            line depth ")"
        ]

    | StructAccess (e, id) ->
        block depth [
            line depth "StructAccess(";
            string_of_expr (depth + 1) e;
            line depth id;
            line depth ")"
        ]

    | ArrayLength e ->
        block depth [
            line depth "ArrayLength(";
            string_of_expr (depth + 1) e;
            line depth ")";
        ]

    | PostfixInc e ->
        block depth [
            line depth "PosfixInc(";
            string_of_expr (depth + 1) e;
            line depth ")";
        ]

    | PostfixDec e ->
        block depth [
            line depth "PosfixDec(";
            string_of_expr (depth + 1) e;
            line depth ")";
        ]

    | Unary (op, e) ->
        block depth [
            line depth ("Unary(" ^ string_of_unary_op op);
            string_of_expr (depth + 1) e;
            line depth ")"
        ]

    | Binary (lhs, op, rhs) ->
        block depth [
            line depth ("Binary(" ^ string_of_binary_op op);
            string_of_expr (depth + 1) lhs;
            string_of_expr (depth + 1) rhs;
            line depth ")"
        ]

    | Logical (lhs, op, rhs) ->
        block depth [
            line depth ("Logical(" ^ string_of_logical_op op);
            string_of_expr (depth + 1) lhs;
            string_of_expr (depth + 1) rhs;
            line depth ")"
        ]

    | Assign (e1, e2) ->
        block depth [
            line depth "Assign(";
            string_of_expr (depth + 1) e1;
            string_of_expr (depth + 1) e2;
            line depth ")"
        ]

    | FunExpr (params, return_type, body) ->
        block depth (
            [
                line depth "FunExpr(";
                string_of_param_list (depth + 1) params;
            ]
            @
            (match return_type with
            | Some rt ->
                [line (depth + 1) ("ReturnType(" ^ string_of_data_type rt ^ ")")]
            | None -> [])
            @
            [
                string_of_block (depth + 1) body;
                line depth ")";
            ]
        )
    | Group e ->
        block depth [
            line depth "Group(";
            string_of_expr (depth + 1) e;
            line depth ")"
        ]

and string_of_block depth stmts =
    block depth (
        line depth "Block[" ::
        (List.map (string_of_statement (depth + 1)) stmts)
        @
        [line depth "]"]
    )

and string_of_statement depth stmt =
    match stmt.kind with
    | ExprStmt e ->
        block depth [
            line depth "ExprStmt(";
            string_of_expr (depth + 1) e;
            line depth ")";
        ]

    | ReturnStmt eo ->
        block depth (
            line depth "Return(" ::
            (match eo with
            | Some e -> [string_of_expr (depth + 1) e]
            | None -> [])
            @
            [line depth ")"]
        )

    | WhileStmt (cond, body) ->
        block depth [
            line depth "While(";
            string_of_expr (depth + 1) cond;
            string_of_block (depth + 1) body;
            line depth ")";
        ]

    | IfStmt (cond, then_block, else_block) ->
        block depth (
            [
                line depth "If(";
                string_of_expr (depth + 1) cond;
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
                line depth ("VarDecl(" ^ string_of_data_type dt ^ " " ^ name);
            ]
            @
            (match init with
            | Some e -> [string_of_expr (depth + 1) e]
            | None -> [])
            @
            [line depth ")"]
        )

    | FunDeclStmt (name, params, return_type, body) ->
        block depth (
            [
                line depth ("FunDecl(" ^ name);
                string_of_param_list (depth + 1) params;
            ]
            @
            (match return_type with
            | Some rt ->
                [line (depth + 1) ("ReturnType(" ^ string_of_data_type rt ^ ")")]
            | None -> [])
            @
            [
                string_of_block (depth + 1) body;
                line depth ")";
            ]
        )
    | StructDeclStmt (name, ht) ->
        block depth [
            line depth ("StructDecl(" ^ name);
            string_of_param_list (depth + 1) (ht
                |> Hashtbl.to_seq
                |> Seq.map (fun (x, y) -> (y, x))
                |> List.of_seq
            );
            line depth ")";
        ]

    | BlockStmt b ->
        string_of_block depth b
