let () = 
	if Array.length Sys.argv = 4 then
        let grammar = Grammar_io.read_bnf_grammar Sys.argv.(1)
        and prefix = Grammar_io.read_tokens (Sys.argv.(2))
        and suffix = Grammar_io.read_tokens (Sys.argv.(3)) in
        print_string ("Prefix: "^(Base.part2string prefix)^"\nSuffix: "^(Base.part2string suffix)^"\n");
        let g = Quotient.generate_blind_grammar_both_sides prefix suffix grammar in
        print_string (Grammar_io.bnf_string_of_grammar g)
    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaireBNF> <prefixe> <suffixe>\n")


