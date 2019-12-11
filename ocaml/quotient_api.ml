let () = 
	if Array.length Sys.argv = 4 then
        let grammaire = Base.read_bnf_grammar Sys.argv.(1)
        and prefix = Base.read_tokens (Sys.argv.(2))
        and suffix = Base.read_tokens (Sys.argv.(3)) in
        print_string ("Prefix: "^(Base.partie2string prefix)^"\nSuffix: "^(Base.partie2string suffix)^"\n");
        let g = Quotient.generate_blind_grammar_both_sides prefix suffix grammaire in
        print_string (Base.bnf_string_of_grammar g)
    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaireBNF> <prefixe> <suffixe>\n")


