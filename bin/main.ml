open Straw_lang_interpreter

let process_args () =
  Array.iter
    (function
      | "--dbg-lexing" ->
          Dbg_prints.lexing_enabled := true

      | "--dbg-parser" ->
          Dbg_prints.parser_enabled := true

      | "--dbg-semantic" ->
          Dbg_prints.semantic_enabled := true

      | "--dbg-interpreter" ->
          Dbg_prints.interpreter_enabled := true

      | "--dbg-all" ->
          Dbg_prints.enable_all ()

      | _ -> ())
    Sys.argv

let read_file name = In_channel.with_open_text name In_channel.input_lines

let write_file name content = Out_channel.with_open_text name (fun oc ->
        Out_channel.output_string oc content)

let () =
    let file_name = if Array.length Sys.argv > 1 then
        Sys.argv.(1)
    else
        failwith "Provide input file path to run"
    in

    process_args ();

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

    Dbg_prints.dbg_print_token_list toks;

    let parser = Parser.create (Array.of_list toks) in
    let ast = Parser.parse parser in

    Dbg_prints.dbg_print_ast ast;

    let st = Semantic.run_type_checking ast in

    Interpreter.interpret st ast


