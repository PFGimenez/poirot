open Base

let ()=
    let grammar = Grammar_io.read_bnf_grammar "../bnf_grammars/msg_exec.bnf" in
    let q = Rec_quotient.quotient_mem grammar in
(*    print_endline "First call";
    let g2 = q {pf=List.rev [Nonterminal("=");Terminal(";");Terminal("value")];e=Nonterminal("S");sf=[]} in *)
(*    let g2 = q {pf=List.rev [Nonterminal("Msg");Terminal(";");Terminal("exec");Terminal("cmd");Terminal(";");Nonterminal("Exe");Terminal(";")];e=Nonterminal("S");sf=[]} in
    let g2 = q {pf=List.rev [Nonterminal("Msg");Terminal(";");Terminal("exec");Terminal("cmd");Terminal(";");Nonterminal("Exe");Terminal(";")];e=Nonterminal("S");sf=[]} in *)
let g2 = q {pf=[Terminal("exec");Terminal("cmd")];e=Nonterminal("S");sf=[Terminal("value");Terminal(";");Nonterminal("Exe")]} in
(* let g2 = q {pf=List.rev [Nonterminal("Msg");Terminal(";");Terminal("exec")];e=Nonterminal("S");sf=[]} in *)
print_endline ((string_of_ext_grammar g2));

(* TODO: Exe_[Exe|] devrait renvoyer epsilon *)
(* TODO: Params_[ε|Exe] -> key = value & Params_[ε|Exe] devrait être useless *)

