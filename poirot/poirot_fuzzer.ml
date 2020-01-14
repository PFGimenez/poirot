let () =
    if Array.length Sys.argv = 3 then
        let w = Grammar_io.read_bnf_grammar Sys.argv.(1) |> Tree_fuzzer.fuzzer (int_of_string Sys.argv.(2)) None None in
        match w with
        | None -> print_endline "No word in language !"
        | Some p -> print_endline (Grammar.string_of_word p)
    else
        print_endline ("Usage: "^Sys.argv.(0)^" <input BNF filename> <explosion depth size>")
