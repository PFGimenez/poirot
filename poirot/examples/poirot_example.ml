let explode s = List.init (String.length s) (String.get s)

let ()=
    let graph_fname = ref None
    and qgraph_fname = ref None
    and injg_fname = ref None
    and max_depth = ref 10
    and grammar = ref None
    and oracle_fname = ref None
    and goal = ref None
    and start = ref None
    and avoid = ref "" in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some (Poirot.read_bnf_grammar s)),     "Target grammar");
        ("-goal",       Arg.String (fun s -> goal := Some (Poirot.read_token s)),     "Terminal or nonterminal to reach");
        ("-oracle",     Arg.String (fun s -> oracle_fname := Some s),     "Oracle script filename");
        ("-start",      Arg.String (fun s -> start := Some (Poirot.read_tokens s)),     "A valid injection, either terminal or nonterminal");
        ("-avoid",      Arg.Set_string avoid,     "List of characters to avoid");
        ("-maxdepth",   Arg.Set_int max_depth,    "Set the max depth search (default: "^(string_of_int !max_depth)^")");
        ("-sgraph",     Arg.String (fun s -> graph_fname := Some s),    "Save the search graph");
        ("-qgraph",     Arg.String (fun s -> qgraph_fname := Some s),    "Save the quotient graph");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Save the injection grammar")
    ] in
    let usage = "Error: grammar, goal and oracle are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !oracle_fname <> None && !goal <> None then
        let grammar = Option.get !grammar
        and goal = Option.get !goal
        and oracle_fname = Option.get !oracle_fname in

        let qgraph_channel = Option.map open_out !qgraph_fname in

        let g = Poirot.search None oracle_fname grammar goal !start !max_depth (explode !avoid) !graph_fname qgraph_channel in
        if g = None then print_endline "No grammar found";
    else print_endline usage
