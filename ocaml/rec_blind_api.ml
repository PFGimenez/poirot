open Base

let ()=
    let grammar = Grammar_io.read_bnf_grammar "../bnf_grammars/msg_exec.bnf" in
    let ll = Rec_quotient.left_symbol_list grammar grammar.axiom in
    print_endline("Left symbols:");
    List.iter (fun e -> print_endline (string_of_element e)) ll;
    let lr = Rec_quotient.right_symbol_list grammar grammar.axiom in
    print_endline("Right symbols:");
    List.iter (fun e -> print_endline (string_of_element e)) lr;
    print_grammar grammar;
    (* let out = Rec_quotient.quotient grammar (Nonterminal("Msg")) (ext_rule_of_rule (List.nth (grammar.rules) 5)) in
    let rlist = fst out in
    print_endline ("New rules:\n"^(string_of_ext_rules rlist));
    let new_sym = snd out in
    print_endline "New symbols:";
    List.iter (fun e -> print_endline (string_of_ext_element e)) new_sym *)
    (* let new_sym = Rec_quotient.quotient grammar [ext_rule_of_rule (List.nth (grammar.rules) 6)] [Nonterminal("Exe");Terminal(";");Nonterminal("Msg");Terminal(";");Nonterminal("Msg")] [] in
    print_endline "New symbols:";
    List.iter (fun e -> print_endline (string_of_ext_element e)) new_sym *)
    let q = Rec_quotient.quotient_mem grammar in
    print_endline "First call";
    let g2 = q {pf=List.rev [Nonterminal("=");Terminal(";");Terminal("value")];e=Nonterminal("S");sf=[]} in
print_endline ((string_of_ext_grammar g2));
