let explode s = List.init (String.length s) (String.get s)

let ()=
    let graph_fname = ref None
    and qgraph_fname = ref None
    and injg_fname = ref None
    and max_depth = ref 10
    and grammar = ref None
    and oracle_fname = ref None
    and subst = ref None
    and goal = ref None
    and start = ref None
    and verbose = ref false
    and avoid = ref "" in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some (Poirot.read_bnf_grammar s)),     "Target grammar");
        ("-goal",       Arg.String (fun s -> goal := Some (Poirot.read_token s)),     "Terminal or nonterminal to reach");
        ("-oracle",     Arg.String (fun s -> oracle_fname := Some s),     "Oracle script filename");
        ("-start",      Arg.String (fun s -> start := Some (Poirot.read_tokens s)),     "A valid injection, either terminal or nonterminal");
        ("-avoid",      Arg.Set_string avoid,     "List of characters to avoid");
        ("-subst",      Arg.String (fun s -> subst := Some s),     "Filename of the substitutions configuration file");
        ("-maxdepth",   Arg.Set_int max_depth,    "Set the max depth search (default: "^(string_of_int !max_depth)^")");
        ("-sgraph",     Arg.String (fun s -> graph_fname := Some s),    "Save the search graph");
        ("-qgraph",     Arg.String (fun s -> qgraph_fname := Some s),    "Save the quotient graph");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Export the injection grammar in ANTLR4 format");
        ("-verbose",    Arg.Set verbose,     "Make Poirot verbose")
    ] in
    let usage = "Error: grammar, goal, start and oracle are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !oracle_fname <> None && !goal <> None  && !start <> None then
        let grammar = Option.get !grammar
        and goal = Option.get !goal
        and start = Option.get !start
        and oracle_fname = Option.get !oracle_fname in

        let g = Poirot.search ~verbose:!verbose ~subst:(Option.map Poirot.read_subst !subst) ~max_depth:!max_depth ~forbidden_chars:(explode !avoid) ~sgraph_fname:!graph_fname ~qgraph_fname:!qgraph_fname oracle_fname grammar goal start in
        match (g,!injg_fname) with
        | Some gram, Some fname -> Poirot.export_antlr4 fname gram; print_endline ("Injection: "^(Option.get (Poirot.fuzzer ~complexity:0 ~goal:(Some goal) gram)))
        | Some gram, _ -> print_endline ("Injection: "^(Option.get (Poirot.fuzzer ~complexity:0 ~goal:(Some goal) gram)))
        | None, _ -> print_endline "No grammar found";
    else print_endline usage
