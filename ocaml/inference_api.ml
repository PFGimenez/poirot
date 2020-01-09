let ()=
    let graph_fname = ref None
    and qgraph_fname = ref None
    and injg_fname = ref None
    and max_depth = ref 10
    and grammar = ref None
    and prefix = ref None
    and suffix = ref None
    and goal = ref None
    and start = ref None
    and avoid = ref "" in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some (Clean.clean_grammar (Grammar_io.read_bnf_grammar s))),     "Target grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some (Grammar_io.read_tokens s)),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some (Grammar_io.read_tokens s)),     "Suffix of the request");
        ("-goal",       Arg.String (fun s -> goal := Some (List.hd (Grammar_io.read_tokens s))),     "Terminal or nonterminal to reach");
        ("-start",      Arg.String (fun s -> start := Some (Grammar_io.read_tokens s)),     "A valid injection, either terminal or nonterminal");
        ("-avoid",      Arg.Set_string avoid,     "List of characters to avoid");
        ("-maxdepth",   Arg.Set_int max_depth,    "Set the max depth search (default: "^(string_of_int !max_depth)^")");
        ("-graph",      Arg.String (fun s -> graph_fname := Some s),    "Save the search graph");
        ("-qgraph",     Arg.String (fun s -> qgraph_fname := Some s),    "Save the quotient graph");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Save the injection grammar")
    ] in
    let usage = "Error: grammar, prefix, suffix and goal are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !prefix <> None && !suffix <> None && !goal <> None then begin
        let grammar = Option.get !grammar
        and prefix = Option.get !prefix
        and suffix = Option.get !suffix
        and goal = Option.get !goal in

        let values = Hashtbl.create 100 in
        let oracle = Oracle.oracle_mem2 (Fuzzer.oracle prefix suffix grammar) and
(*        let oracle = Oracle.parenth_oracle prefix suffix and*)
        fuzzer = Tree_fuzzer.fuzzer 0 None in

        let fuzzer_oracle (g: Grammar.grammar) : Oracle.oracle_status = g |> fuzzer |> oracle in

        let qgraph_channel = Option.map open_out !qgraph_fname in
        Option.iter (fun ch -> output_string ch "digraph {\n") qgraph_channel;

        let g = Inference.search fuzzer_oracle grammar goal !start !max_depth (Inference.explode !avoid) !graph_fname qgraph_channel in
        if g = None then print_endline "No grammar found"
        else begin
            let inj_g = Option.get g in
            print_endline ("Injection:  "^(Fuzzer.string_inst_of_part values (Option.get (fuzzer (Grammar.grammar_of_ext_grammar inj_g)))));
            Option.iter (fun f -> Grammar_io.export_bnf f inj_g) !injg_fname
        end;
        Option.iter (fun ch -> output_string ch "}"; close_out ch) qgraph_channel
    end else print_endline usage
