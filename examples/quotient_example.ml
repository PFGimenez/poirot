let ()=
    let grammar = ref None
    and prefix = ref None
    and suffix = ref None
    and goal = ref None
    and injg_fname = ref None
    and axiom = ref None in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some (Poirot.read_bnf_grammar s)),     "Target grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some (Poirot.read_tokens s)),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some (Poirot.read_tokens s)),     "Suffix of the request");
        ("-axiom",      Arg.String (fun s -> axiom := Some (Poirot.read_token s)),     "Axiom of the request");
        ("-goal",       Arg.String (fun s -> goal := Some (Poirot.read_token s)),     "Terminal or nonterminal to reach");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Export the injection grammar in ANTLR4 format")
    ] in
    let usage = "Error: grammar, prefix and suffix are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !prefix <> None && !suffix <> None then begin
        let grammar = match !axiom with
            | None -> Option.get !grammar
            | Some e -> Poirot.set_axiom (Option.get !grammar) e
        and prefix = Option.get !prefix
        and suffix = Option.get !suffix in

        let inj_g = Poirot.quotient grammar prefix suffix in
        Option.iter (fun fname -> Poirot.export_antlr4 fname inj_g) !injg_fname;
        print_endline ("Injection: "^(Option.get (Poirot.fuzzer ~complexity:0 ~goal:!goal inj_g)))
    end else print_endline usage

