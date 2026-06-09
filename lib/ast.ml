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
    | TFunction of data_type list * data_type
    (* Represents some custom named type - structs or enums *)
    | TNamed of string
    | TImplicit (* this one shouldn't exist after semantic analysis *)
    | TUnit

and expr_kind =
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool
    | StrLit of string
    (* | EnumLit of string * string *)
    | FormattedStringLit of string list * expr list
    | ArrayContent of expr array
    | StructExpr of string * (string, expr) Hashtbl.t

    | Variable of string
    | Call of expr * expr list
    | Index of expr * expr
    | FieldAccess of expr * string

    | PostfixInc of expr
    | PostfixDec of expr

    | Binary of expr * binary_op * expr
    | Unary of unary_op * expr
    | Logical of expr * logical_op * expr

    | Assign of expr * expr
    | FunExpr of parameter list * data_type * block

    | Group of expr


and statement_kind =
    | IfStmt of expr * block * block option (* conditional_expr then_block else_block *)
    | WhileStmt of expr * block
    | ReturnStmt of expr option
    | VarDeclStmt of data_type * string * expr option
    | FunDeclStmt of string * parameter list * data_type * block
    | StructDeclStmt of string * (string, data_type) Hashtbl.t
    | ExprStmt of expr (* example foo(1,2) or print(x) are expressions but they are used as statements ofc *)
    | BlockStmt of block
    | EnumDeclStmt of string * string list

and expr = {
    kind: expr_kind;
    pos: Lexing_types.position;
}

and statement = {
    mutable kind: statement_kind;
    pos: Lexing_types.position;
}

and parameter = data_type * string

and block = statement list


