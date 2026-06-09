open Straw_lang_interpreter

let read_file name = In_channel.with_open_text name In_channel.input_lines

let write_file name content = Out_channel.with_open_text name (fun oc ->
        Out_channel.output_string oc content)

let () =
    let file_name = if Array.length Sys.argv > 1 then
        Sys.argv.(1)
    else
        failwith "Provide input file path to run"
    in

    let lines = read_file file_name in

    let lexer = Lexer.create (String.concat "\n" lines) in

    let rec loop toks = let token = Lexer.next_token lexer in
        (* print_endline (Lexing_types.string_of_token token.kind); *)
        let new_acc = token :: toks in

        if token.kind != Lexing_types.EOF then
            loop new_acc
        else
            new_acc
    in
    let toks = List.rev (loop []) in

    (* for printing *)
    let tok_kinds = List.map (fun (x: Lexing_types.token) -> x.kind) toks in
    print_endline (Printf.sprintf "[%s]" (String.concat "\n" (List.map Lexing_types.string_of_token tok_kinds)));
    (* end printing *)

    let parser = Parser.create (Array.of_list toks) in
    let ast = Parser.parse parser in

    let rec loop rem = match rem with
        | h :: t -> Ast.string_of_statement 0 h ^ "\n" ^ loop t
        | [] -> ""
    in
    let out = loop ast in
    print_endline (out);

    let st = Semantic.run_type_checking ast in

    Interpreter.interpret st ast


