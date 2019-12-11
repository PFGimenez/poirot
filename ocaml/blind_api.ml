let ()=
	if Array.length Sys.argv = 5 then
        let grammaire = Base.read_bnf_grammar Sys.argv.(1)
        and prefix = Base.read_tokens (Sys.argv.(2))
        and suffix = Base.read_tokens (Sys.argv.(3))
        and intepart = Base.read_tokens (Sys.argv.(4)) in
        print_string ("Prefix: "^(Base.partie2string prefix)^"\nSuffix: "^(Base.partie2string suffix)^"\n");
        let values = Hashtbl.create 1 in
        Hashtbl.add values (Base.Terminal("value")) "val1";

        if List.length intepart != 1 then
            if List.length intepart = 0 then begin
                print_string "Au moins un objectif nécessaire\n"
            end else begin
                print_string "Pas plus d'un objectif !\n"
            end
        else
            let interest = List.hd intepart in

            if not (Blind.is_symbol_accessible grammaire interest) then
                print_string "Objectif inconnu !\n"
            else
                let blackbox = Blind.blackbox_template prefix suffix grammaire in
                let injection_tokens = Blind.get_injection_leaves blackbox grammaire in
                if List.length injection_tokens = 0 then
                    print_string "Pas de token d'injection !\n"
                else
                    print_string "Injection token:\n";
                    List.iter (fun (p,e,s) -> print_string ("  \""^(Base.element2string e)^"\"\n")) injection_tokens;
                    let g = Blind.search_api blackbox interest grammaire injection_tokens in match g with
                    | None -> print_string "Pas de grammaire trouvée\n"
                    | Some(g2) -> print_string ("Injection:\n  "^(Base.string_inst_of_part values (Fuzzer.derive_word_with_symbol g2 interest))^"\n"); print_string ("Grammaire :\n"^(Base.bnf_string_of_rec_grammar (Clean.clean (Base.rec_grammar_of_grammar g2))))

    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <prefixe> <suffixe> <objectif>\n")
