open Base

let ()=
    let grammar = Nettoyage.nettoyage (Grammar_io.read_bnf_grammar Sys.argv.(1)) in
    let order = Fuzzer.get_order grammar in
    print_endline ((part2string order));
    let g2 = Fuzzer.delete_recursion grammar in
    print_grammar g2
(*    derive_within_lengthPrint 10 grammar;*)
(*    let path = find_path_symbol grammar [Terminal(Sys.argv.(2)),[]] in
    List.iter (fun r -> print_endline ((rule2string r))) path; flush stdout *)
(*    let w = derive_with_path grammar [[grammar.axiom],path] in
    print_endline ((part2string w))*)


