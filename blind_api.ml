open Base
open Blind
open Parser
open Clean
open Quotient

let ()=
	if Array.length Sys.argv = 5 then
        let grammaire = Base.read_bnf_grammar Sys.argv.(1)
        and prefix = string2partie (Sys.argv.(2))
        and suffix = string2partie (Sys.argv.(3))
        and intepart = string2partie (Sys.argv.(4)) in
        let g = clean (rec_grammar_of_grammar grammaire) in
        print_string ("Grammaire lue:\n"^(string_of_rec_grammar g)^"\n");
        print_string ("Grammaire lue (bnf):\n"^(bnf_string_of_rec_grammar g)^"\n");
(*        let lists = get_epsilon_symbols g in
        print_string ("Eps symbols: "^(string_of_rec_part lists)^"\n");
        let eps_sym = List.hd lists in
        print_string ("Symbol: "^(string_of_tree eps_sym)^"\n");
        let r = List.hd (get_rules_with_symbol eps_sym g.rules) in
        print_string ("Rule: "^(string_of_rec_rule r)^"\n");
        let g4 = g.axiom @@@ (remove_epsilon_symbols_once g) in
        print_string ((string_of_rec_grammar g4)^"\n");
        let g5 = g.axiom @@@ (remove_epsilon_symbols_once g4) in
        print_string ((string_of_rec_grammar g5)^"\n");
        let g2 = remove_useless_symbols g in
        print_string ((string_of_rec_grammar g2)^"\n");
        let g3 = remove_unreachable_symbols g2 in
        print_string ((string_of_rec_grammar g3)^"\n");*)
        let values = Hashtbl.create 1 in
        Hashtbl.add values (Terminal("value")) "val1";

        if List.length intepart != 1 then
            if List.length intepart = 0 then begin
                print_string "Au moins un objectif nécessaire\n"
            end else begin
                print_string "Pas plus d'un objectif !\n"
            end
        else
            let interest = List.hd intepart in

            if not (is_symbol_accessible grammaire interest) then
                print_string "Objectif inconnu !\n"
            else
                let blackbox = blackbox_template prefix suffix grammaire in
                let injection_tokens = get_injection_leaves blackbox grammaire in
                if List.length injection_tokens = 0 then
                    print_string "Pas de token d'injection !\n"
                else
                    print_string "Injection token:\n";
                    List.iter (fun (p,e,s) -> print_string ("  \""^(element2string e)^"\"\n")) injection_tokens;
                    let g = search_api blackbox interest grammaire injection_tokens in match g with
                    | None -> print_string "Pas de grammaire trouvée\n"
                    | Some(g2) -> print_string ("Injection:\n  "^(string_inst_of_part values (derive_word_with_symbol g2 interest))^"\n"); print_string ("Grammaire :\n"^(bnf_string_of_rec_grammar (clean (rec_grammar_of_grammar g2))))

    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <prefixe> <suffixe> <objectif>\n")
