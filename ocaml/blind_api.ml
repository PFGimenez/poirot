let ()=
    if Array.length Sys.argv = 5 then
        let grammar = Grammar_io.read_bnf_grammar Sys.argv.(1)
        and prefix = Grammar_io.read_tokens (Sys.argv.(2))
        and suffix = Grammar_io.read_tokens (Sys.argv.(3))
        and intepart = Grammar_io.read_tokens (Sys.argv.(4)) in
        print_endline ("Prefix: "^(Base.part2string prefix)^"Suffix: "^(Base.part2string suffix));
        let values = Hashtbl.create 1 in
        Hashtbl.add values (Base.Terminal("value")) "val1";

        if List.length intepart != 1 then
            if List.length intepart = 0 then begin
                print_endline "Au moins un objectif nécessaire"
            end else begin
                print_endline "Pas plus d'un objectif !"
            end
        else
            let interest = List.hd intepart in

            if not (Blind.is_symbol_accessible grammar interest) then
                print_endline "Objectif inconnu !"
            else
                let blackbox = Blind.blackbox_template prefix suffix grammar in
                let injection_tokens = Blind.get_injection_leaves blackbox grammar in
                if List.length injection_tokens = 0 then
                    print_endline "Pas de token d'injection !"
                else
                    print_endline "Injection token:";
                    List.iter (fun e -> print_endline ("  \""^(Base.element2string (Base.element_of_ext_element e))^"\"")) injection_tokens;
                    let g = Blind.search_api blackbox interest grammar injection_tokens in match g with
                    | None -> print_endline "Pas de grammar trouvée"
                    | Some(g2) -> print_endline ("Injection:  "^(Base.string_inst_of_part values (Fuzzer.derive_word_with_symbol g2 interest))); print_endline ("grammar :"^(Grammar_io.bnf_string_of_ext_grammar (Clean.clean (Base.ext_grammar_of_grammar g2))))

    else print_endline ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <prefixe> <suffixe> <objectif>")
