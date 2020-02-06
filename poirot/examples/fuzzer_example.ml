let () =
    if Array.length Sys.argv = 3 then
        let w = Poirot.read_bnf_grammar true Sys.argv.(1) |> Poirot.to_lowercase |> Poirot.simplify |> Poirot.fuzzer (int_of_string Sys.argv.(2)) in
        match w with
        | None -> print_endline "No word in language !"
        | Some s -> print_endline s
    else
        print_endline ("Usage: "^Sys.argv.(0)^" <input BNF filename> <explosion depth size>")
