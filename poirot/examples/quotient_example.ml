let ()=
    let grammar = ref None
    and prefix = ref None
    and suffix = ref None
    and axiom = ref None in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some (Poirot.read_bnf_grammar false s)),     "Target grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some (Poirot.read_tokens false s)),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some (Poirot.read_tokens false s)),     "Suffix of the request");
        ("-axiom",      Arg.String (fun s -> axiom := Some (Poirot.read_token false s)),     "Axiom of the request")
    ] in
    let usage = "Error: grammar, prefix and suffix are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !prefix <> None && !suffix <> None then
        let grammar = match !axiom with
            | None -> Option.get !grammar
            | Some e -> Poirot.set_axiom (Option.get !grammar) e
        and prefix = Option.get !prefix
        and suffix = Option.get !suffix in

        let inj_g = Poirot.quotient grammar prefix suffix in
        print_endline (Poirot.string_of_grammar inj_g);
    else print_endline usage

