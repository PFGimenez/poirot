open Base
open Quotient

let () = 
	if Array.length Sys.argv = 4 then
        let grammaire = Base.read_bnf_grammar Sys.argv.(1)
        and prefix = read_tokens (Sys.argv.(2))
        and suffix = read_tokens (Sys.argv.(3)) in
        print_string ("Prefix: "^(partie2string prefix)^"\nSuffix: "^(partie2string suffix)^"\n");
        let g = generate_blind_grammar_both_sides prefix suffix grammaire in
        print_string (bnf_string_of_grammar g)
    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaireBNF> <prefixe> <suffixe>\n")


