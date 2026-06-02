
exception Lexing_error of string * Lexing_types.position

exception Parse_error of string * Lexing_types.position

exception Type_mismatch_error of string * string * Lexing_types.position

exception Type_invalid_operator_error of string * string * string * Lexing_types.position

exception Type_invalid_un_operator_error of string * string * Lexing_types.position

exception Type_undeclared_error of string * Lexing_types.position

exception Type_custom_error of string * Lexing_types.position

exception Runtime_error of string

    let () = Printexc.register_printer (function
        | Lexing_error (msg, pos) ->
                Some (Printf.sprintf "LexingError: %s at %d:%d" msg pos.line pos.column)

        | Type_mismatch_error (found, expected, pos) ->
                Some (Printf.sprintf "TypeError: found %s but expected %s at %d:%d" found expected pos.line pos.column)

        | Type_invalid_un_operator_error (dt, op, pos) ->
                Some (Printf.sprintf "TypeError: operator '%s' not applicable for type %s at %d:%d" op dt pos.line pos.column)

        | Type_invalid_operator_error (op, dt, dt2, pos) ->
                Some (Printf.sprintf "TypeError: operator '%s' not applicable for types %s and %s at %d:%d" op dt dt2 pos.line pos.column)

        | Type_undeclared_error (name, pos) ->
                Some (Printf.sprintf "TypeError: variable %s not declared at %d:%d" name pos.line pos.column)

        | Type_custom_error (text, pos) ->
                Some (Printf.sprintf "TypeError: %s at %d:%d" text pos.line pos.column)

        | Parse_error (s, pos) ->
                Some (Printf.sprintf "ParseError: %s at %d:%d" s pos.line pos.column)

        | Runtime_error (s) -> Some (Printf.sprintf "RuntimeError: %s" s)

        | _ -> None
    )

