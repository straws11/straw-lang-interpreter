type function_value =
    (* environment is captured for closures. should later optimize to store only relevant ones *)
    | UserFunction of Ast.parameter list * Ast.data_type * Ast.block * environment
    | BuiltinFunction of (value list -> value)

and value =
    | VInteger of int
    | VFloat of float
    | VBoolean of bool
    | VString of string
    | VCharacter of char
    | VArray of value array
    | VFunction of function_value
    | VStruct of (string, value) Hashtbl.t
    | VEnumMember of string * string
    | VUnit

and environment = {
    outer: environment option;
    tbl: (string, value option) Hashtbl.t;
}

(* return None if variable doesn't exist, and Some if it does. the Some
   contains a Some x if there's an associated value, and None if
   there isn't yet
   *)
let lookup env var =
    let rec loop scope =
        let item = Hashtbl.find_opt scope.tbl var in
        match item with
            | Some Some x -> Some (Some x) (* found & has value *)
            | Some None -> Some None (* found but no assigned value *)
            | _ ->
                begin match scope.outer with
                    | Some e -> loop e
                    | None -> None
                end
    in
    loop env

let insert env name v = Hashtbl.replace env.tbl name (Some v)

let insert_empty env name = Hashtbl.replace env.tbl name None

let update env name new_v =
    let rec loop scope =
        match Hashtbl.find_opt scope.tbl name with
            | Some _ -> Hashtbl.replace scope.tbl name (Some new_v)
            | None ->
                begin match scope.outer with
                | Some e -> loop e
                | None -> ()
                end
    in loop env


