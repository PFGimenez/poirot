let ()=
    if Array.length Sys.argv = 2 then
        let s = Sys.argv.(1) in
        let last_dot = String.rindex s '.' in
        let fname = String.sub s 0 last_dot in
        Poirot.export_antlr4 fname (Poirot.read_bnf_grammar s)
    else
        print_endline ("Usage: "^Sys.argv.(0)^" grammar.bnf")
