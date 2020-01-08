let () =
    if Array.length Sys.argv = 3 then
        Grammar_io.read_bnf_grammar Sys.argv.(1) |> Tree_fuzzer.fuzzer (int_of_string Sys.argv.(2)) None |> Grammar.string_of_word |> print_endline
    else
        print_endline ("Usage: "^Sys.argv.(0)^" <input BNF filename> <minimal parse tree size>")
