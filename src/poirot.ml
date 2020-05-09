let ()=
    let graph_fname = ref None
    and qgraph_fname = ref None
    and save_h = ref true
    and injg_fname = ref None
    and max_depth = ref 10
    and max_steps = ref 10000
    and oracle_timeout = ref (-1.)
    and oracle_wait = ref (-1.)
    and oracle_save = ref true
    and oracle_fname = ref None
    and grammar_fname = ref None
    and prefix = ref None
    and suffix = ref None
    and dict = ref None
    and goal = ref None
    and start = ref None
    and oneline_comment = ref None
    and lowercase = ref false
    and uppercase = ref false
    and simplify = ref false
    and verbose_lvl = ref (Some Logs.Info)
    and avoid = ref "" in

    let set_verbose_lvl (s: string) =
        let r = Logs.level_of_string s in
        if Result.is_error r then
            failwith "Unknown verbose level. Possibles levels are: debug, info, warning, error."
        else
            verbose_lvl := Result.get_ok r in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar_fname := Some s),     "Target grammar");
        ("-goal",       Arg.String (fun s -> goal := Some (Poirot.read_token s)),     "Terminal or nonterminal to reach");
        ("-oracle",     Arg.String (fun s -> oracle_fname := Some s),     "Oracle script filename");
        ("-oracle_pf_sf", Arg.Tuple ([Arg.String (fun s -> prefix := Some s);Arg.String (fun s -> suffix := Some s)]), "Oracle based on the simulation of a query given a prefix and a suffix");
        ("-start",      Arg.String (fun s -> start := Some (Poirot.read_tokens s)),     "A valid injection, either terminal or nonterminal");
        ("-avoid",      Arg.Set_string avoid,     "List of characters to avoid");
        ("-dict",      Arg.String (fun s -> dict := Some s),     "Filename of the semantics dictionary");
        ("-maxdepth",   Arg.Set_int max_depth,    "Set the max depth search (default: "^(string_of_int !max_depth)^")");
        ("-maxsteps",   Arg.Set_int max_steps,    "Set the max steps search (default: "^(string_of_int !max_steps)^")");
        ("-oracle_timeout",   Arg.Set_float oracle_timeout,    "Set the timeout to oracle calls (in seconds, -1 for no timeout)");
        ("-oracle_interval",   Arg.Set_float oracle_wait,    "Set the minimal duration between two oracle calls (in seconds, -1 for no wait)");
        ("-sgraph",     Arg.String (fun s -> graph_fname := Some s),    "Save the search graph");
        ("-qgraph",     Arg.String (fun s -> qgraph_fname := Some s),    "Save the quotient graph");
        ("-nosave_h",     Arg.Clear save_h,    "Disable the heuristics save");
        ("-nosave_oracle",   Arg.Clear oracle_save,    "Disable the oracle calls save");
        ("-oneline_comment",     Arg.String (fun s -> oneline_comment := Some s),    "The string that starts one-line comment");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Export the injection grammar in ANTLR4 format");
        ("-lowercase",  Arg.Set lowercase,     "Convert all terminals to lowercase");
        ("-uppercase",  Arg.Set uppercase,     "Convert all terminals to uppercase");
        ("-simplify",   Arg.Set simplify,     "If used with -lowercase or -uppercase, simplify the grammar");
        ("-verbose_lvl",    Arg.String(set_verbose_lvl),     "Choose Poirot verbosity: debug, info, warning or error");
        ("-v",    Arg.Unit (fun () -> print_endline ("Poirot v"^Poirot.version)),     "Show Poirot version")
    ] in
    Arg.parse speclist ignore ("Poirot v"^Poirot.version);
    if !grammar_fname <> None && ((!oracle_fname <> None) <> (!prefix <> None)) && !goal <> None  && !start <> None then
        let timeout = match !oracle_timeout with
            | -1. -> None
            | n when n > 0. -> Some n
            | _ -> failwith "Negative timeout!" in
        let wait = match !oracle_wait with
            | -1. -> None
            | n when n > 0. -> Some n
            | _ -> failwith "Negative interval!" in

        let grammar_fname = Option.get !grammar_fname in
        let grammar = Poirot.read_bnf_grammar grammar_fname
        and goal = Option.get !goal
        and start = Option.get !start in
        let oracle = match !oracle_fname with
        | None -> Poirot__Oracle.oracle_from_pf_sf ~oneline_comment:!oneline_comment wait grammar_fname (Option.get !prefix) (Option.get !suffix)
        | Some fname -> Poirot__Oracle.oracle_from_script wait timeout fname in

        let grammar = if !lowercase then Poirot.to_lowercase ~simplify:!simplify grammar else (if !uppercase then Poirot.to_uppercase ~simplify:!simplify grammar else grammar) in

        let explode s = List.init (String.length s) (String.get s) in

        Poirot.set_log_level !verbose_lvl;
        Poirot.set_reporter (Logs_fmt.reporter ());

        let s = Option.map Poirot.read_dict !dict in
        match (Poirot.search ~oneline_comment:!oneline_comment ~dict:s ~max_depth:!max_depth ~max_steps:!max_steps ~forbidden_chars:(explode !avoid) ~sgraph_fname:!graph_fname ~qgraph_fname:!qgraph_fname ~save_h:!save_h ~save_oracle:!oracle_save oracle grammar goal start, !injg_fname) with
        | Some (gram, word), Some fname -> Poirot.export_antlr4 fname gram; print_endline ("Injection: "^word)
        | Some (_, word), _ -> print_endline ("Injection: "^word)
        | None, _ -> print_endline "No grammar found";
    else print_endline "Error: grammar, goal, start and one oracle (either -oracle or -oracle_pf_sf) are necessary"
