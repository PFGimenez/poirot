let ()=
    let prefix = ref None
    and suffix = ref None
    and goal = ref None
    and injg_fname = ref None
    and grammar_fname = ref None
    and qgraph_fname = ref None in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar_fname := Some s),     "Target grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some s),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some s),     "Suffix of the request");
        ("-goal",       Arg.String (fun s -> goal := Some (Poirot.read_token s)),     "Terminal or nonterminal to reach");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Export the injection grammar in ANTLR4 format");
        ("-qgraph",     Arg.String (fun s -> qgraph_fname := Some s),    "Save the quotient graph")
    ] in
    Arg.parse speclist ignore ("Quotient utility. Poirot v"^Poirot.version);

    if !grammar_fname <> None && !prefix <> None && !suffix <> None then begin
        let prefix = Option.get !prefix
        and suffix = Option.get !suffix
        and grammar_fname = Option.get !grammar_fname in

        let inj_g,word = Poirot.quotient ~qgraph_fname:!qgraph_fname grammar_fname prefix suffix !goal in
        print_endline (Poirot.string_of_grammar inj_g);
        Option.iter (fun fname -> Poirot.export_antlr4 fname inj_g) !injg_fname;
        match word with
        | None -> print_endline "No injection"
        | Some inj ->  print_endline ("Injection: "^inj)
    end else print_endline "Error: grammar, prefix and suffix are necessary"

