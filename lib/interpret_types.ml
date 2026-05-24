type function_value = {
        params: Ast.parameter list;
        body: Ast.block;
        (* env: env *)
}

type value =
        | VNumber of float
        | VBoolean of bool
        | VString of string
        | VFunction of function_value
