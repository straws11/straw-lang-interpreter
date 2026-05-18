open Straw_lang_interpreter.Lexer
open Straw_lang_interpreter.Lexing_types

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

    let lexer = create (String.concat "\n" lines) in

    let rec loop () = match next_token lexer with
        | EOF -> print_endline (string_of_token EOF)
        | tok -> print_endline (string_of_token tok);
            loop ()
    in
    loop ()

    (* print_endline ("Tokens: " ^ string_of_token_list tokens); *)
