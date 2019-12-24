let ()=
    if Array.length Sys.argv = 5 then
        let grammar = Grammar_io.read_bnf_grammar Sys.argv.(1)
        and prefix = Grammar_io.read_tokens (Sys.argv.(2))
        and suffix = Grammar_io.read_tokens (Sys.argv.(3))
        and goals = Grammar_io.read_tokens (Sys.argv.(4)) in
        print_endline ("Prefix: "^(Base.part2string prefix));
        print_endline ("Suffix: "^(Base.part2string suffix));
        Base.print_grammar grammar;
        let values = Hashtbl.create 1 in
        Hashtbl.add values (Base.Terminal("value")) "val1";

        if List.length goals != 1 then
            if List.length goals = 0 then begin
                print_endline "Au moins un objectif nécessaire"
            end else begin
                print_endline "Pas plus d'un objectif !"
            end
        else
            let goal = List.hd goals in
            print_endline ("Goal: "^(Base.string_of_element goal));
            if not (Blind2.is_reachable grammar goal [grammar.axiom]) then
                print_endline "Objectif inconnu !"
            else
                let oracle = Blind2.oracle_template prefix suffix grammar in
                let injection_tokens = Blind2.get_injection_leaves oracle grammar in
                if List.length injection_tokens = 0 then
                    print_endline "Pas de token d'injection !"
                else
                    print_endline "Injection token:";
                    List.iter (fun e -> print_endline ("  \""^(Base.element2string e)^"\"")) injection_tokens;
                    let g = Blind2.search Blind2.fuzzer oracle grammar goal injection_tokens in match g with
                    | None -> print_endline "Pas de grammar trouvée"
                    | Some(g2) -> print_endline ("Injection:  "^(Base.string_inst_of_part values (Fuzzer.derive_word_with_symbol (Base.grammar_of_ext_grammar g2) goal))); print_endline ("grammar :"^(Grammar_io.bnf_string_of_ext_grammar (Clean.clean g2)))

    else print_endline ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <prefixe> <suffixe> <objectif>")
