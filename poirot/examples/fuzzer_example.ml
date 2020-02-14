let () =
    if Array.length Sys.argv >= 3 then
        let subst = if Array.length Sys.argv = 4 then Some (Poirot.read_subst Sys.argv.(3)) else None in
        let w = Poirot.read_bnf_grammar Sys.argv.(1) |> Poirot.fuzzer ~subst:subst (int_of_string Sys.argv.(2)) in
        match w with
        | None -> print_endline "No word in language !"
        | Some s -> print_endline s
    else
        print_endline ("Usage: "^Sys.argv.(0)^" <input BNF filename> <explosion depth size> [<substition file>]")
