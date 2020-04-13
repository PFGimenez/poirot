let () =
    if Array.length Sys.argv >= 3 then begin
        Poirot.set_log_level (Result.get_ok (Logs.level_of_string "debug"));
        Poirot.set_reporter (Logs_fmt.reporter ());
        let subst = if Array.length Sys.argv = 4 then Some (Poirot.read_subst Sys.argv.(3)) else None in
        let w = Poirot.read_bnf_grammar Sys.argv.(1) |> Poirot.fuzzer ~subst:subst ~complexity:(int_of_string Sys.argv.(2)) in
        match w with
        | None -> print_endline "No word in language !"
        | Some s -> print_endline s
    end else
        print_endline ("Usage: "^Sys.argv.(0)^" <input BNF filename> <explosion depth size> [<substition file>]")
