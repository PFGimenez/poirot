open Base

let ()=
    let grammar = Nettoyage.nettoyage (Base.read_bnf_grammar Sys.argv.(1)) in
    let order = Fuzzer.get_order grammar in
    print_string ((partie2string order)^"\n");
    let g2 = Fuzzer.delete_recursion grammar in
    print_grammar g2
(*    derive_within_lengthPrint 10 grammar;*)
(*    let path = find_path_symbol grammar [Terminal(Sys.argv.(2)),[]] in
    List.iter (fun r -> print_string ((regle2string r)^"\n")) path; flush stdout *)
(*    let w = derive_with_path grammar [[grammar.axiome],path] in
    print_string ((partie2string w)^"\n")*)


