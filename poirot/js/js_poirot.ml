let ()=
    let grammar = Poirot.read_bnf_grammar "msg_exec.bnf" (* TODO: menu déroulant *)
    and goal = Poirot.read_token "Exe" (* TODO: champ *)
    and start = Poirot.read_tokens "'value'" (* TODO: champ *)
    and oracle_fname = "oracles/prefix-suffix.py msg_exec axiom 'msg key = ' ' & key = value'" in (* TODO: js externe ? *)
    print_endline "Start searching…";
    let g = Poirot.search oracle_fname grammar goal start in
    match g with
    | Some gram -> print_endline ("Injection: "^(Option.get (Poirot.fuzzer ~complexity:0 ~goal:(Some goal) gram)))
    | None -> print_endline "No grammar found";
