open Base

let ()=
    let grammar = Grammar_io.read_bnf_grammar "../bnf_grammars/ours.bnf" in
    let q = Rec_quotient.quotient_mem grammar in
(*    print_endline "First call";
    let g2 = q {pf=List.rev [Nonterminal("=");Terminal(";");Terminal("value")];e=Nonterminal("S");sf=[]} in *)
(*    let g2 = q {pf=List.rev [Nonterminal("Msg");Terminal(";");Terminal("exec");Terminal("cmd");Terminal(";");Nonterminal("Exe");Terminal(";")];e=Nonterminal("S");sf=[]} in
    let g2 = q {pf=List.rev [Nonterminal("Msg");Terminal(";");Terminal("exec");Terminal("cmd");Terminal(";");Nonterminal("Exe");Terminal(";")];e=Nonterminal("S");sf=[]} in *)
(*let g2 = q {pf=List.rev [Terminal("qui");Terminal("a")];e=Nonterminal("B");sf=[Nonterminal("E")]} in*)
let g2 = q {pf=List.rev [];e=Terminal("qui");sf=[]} in
(* let g2 = q {pf=List.rev [Nonterminal("Msg");Terminal(";");Terminal("exec")];e=Nonterminal("S");sf=[]} in *)
print_endline ((string_of_ext_grammar g2));

