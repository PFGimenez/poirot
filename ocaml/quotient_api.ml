open Grammar

let ()=
    let injg_fname = ref None
    and grammar = ref None
    and prefix = ref None
    and suffix = ref None
    and qgraph_fname = ref None in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some (Grammar_io.read_bnf_grammar s)),     "Target grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some (Grammar_io.read_tokens s)),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some (Grammar_io.read_tokens s)),     "Suffix of the request");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Save the final grammar");
        ("-qgraph",     Arg.String (fun s -> qgraph_fname := Some s),    "Save the quotient graph")
    ] in
    let usage = "Error: grammar, prefix and suffix are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !prefix <> None && !suffix <> None then
        let grammar = Option.get !grammar
        and prefix = Option.get !prefix
        and suffix = Option.get !suffix in

        let qgraph_channel = Option.map open_out !qgraph_fname in
        Option.iter (fun ch -> output_string ch "digraph {\n") qgraph_channel;

        let q = Rec_quotient.quotient_mem grammar qgraph_channel in
        let inj_g = q {e=grammar.axiom; pf=List.rev prefix; sf=suffix} in
        print_endline (string_of_ext_grammar inj_g);
        Option.iter (fun f -> Grammar_io.export_bnf f inj_g) !injg_fname;
        Option.iter (fun ch -> output_string ch "}"; close_out ch) qgraph_channel
    else print_endline usage
