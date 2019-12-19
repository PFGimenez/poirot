open Base

let ()=
    let grammar = Grammar_io.read_bnf_grammar "../bnf_grammars/msg_exec.bnf" in
    let q = Rec_quotient.quotient_mem grammar in
    print_endline "First call";
    let g2 = q {pf=List.rev [Nonterminal("=");Terminal(";");Terminal("value")];e=Nonterminal("S");sf=[]} in
print_endline ((string_of_ext_grammar g2));
