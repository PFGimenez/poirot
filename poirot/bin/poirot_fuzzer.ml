let () =
    if Array.length Sys.argv = 3 then(
        print_endline (Poirot.string_of_grammar (Poirot.read_bnf_grammar Sys.argv.(1)));
        let w = Poirot.read_bnf_grammar Sys.argv.(1) |> Poirot.fuzzer (int_of_string Sys.argv.(2)) None None in
        match w with
        | None -> print_endline "No word in language !"
        | Some p -> print_endline (Poirot.string_of_word p))
    else
        print_endline ("Usage: "^Sys.argv.(0)^" <input BNF filename> <explosion depth size>")
