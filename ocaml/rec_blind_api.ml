open Base

let ()=
    let grammar = Grammar_io.read_bnf_grammar "../bnf_grammars/msg_exec.bnf" in
    let ll = Rec_quotient.left_symbol_list grammar grammar.axiom in
    print_endline("Left symbols:");
    List.iter (fun e -> print_endline (string_of_element e)) ll;
    let lr = Rec_quotient.right_symbol_list grammar grammar.axiom in
    print_endline("Right symbols:");
    List.iter (fun e -> print_endline (string_of_element e)) lr
