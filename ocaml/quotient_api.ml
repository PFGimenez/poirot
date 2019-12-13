let () = 
	if Array.length Sys.argv = 4 then
        let grammar = Grammar_io.read_bnf_grammar Sys.argv.(1)
        and prefix = Grammar_io.read_tokens (Sys.argv.(2))
        and suffix = Grammar_io.read_tokens (Sys.argv.(3)) in
        print_endline ("Prefix: "^(Base.part2string prefix)^"Suffix: "^(Base.part2string suffix));
        let g = Quotient.generate_blind_grammar_both_sides prefix suffix grammar in
        print_endline (Grammar_io.bnf_string_of_grammar g)
    else print_endline ("Usage : "^Sys.argv.(0)^" <fichierGrammaireBNF> <prefixe> <suffixe>")


