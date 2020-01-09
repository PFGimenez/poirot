let ()=
    let graph_fname = ref None
    and injg_fname = ref None
    and max_depth = ref 10
    and grammar = ref None
    and oracle_fname = ref None
    and goal = ref None
    and start = ref None
    and avoid = ref "" in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some (Grammar_io.read_bnf_grammar s)),     "Target grammar");
        ("-goal",       Arg.String (fun s -> goal := Some (List.hd (Grammar_io.read_tokens s))),     "Terminal or nonterminal to reach");
        ("-oracle",     Arg.String (fun s -> oracle_fname := Some s),     "Oracle script filename");
        ("-start",      Arg.String (fun s -> start := Some (Grammar_io.read_tokens s)),     "A valid injection, either terminal or nonterminal");
        ("-avoid",      Arg.Set_string avoid,     "List of characters to avoid");
        ("-maxdepth",   Arg.Set_int max_depth,    "Set the max depth search (default: "^(string_of_int !max_depth)^")");
        ("-graph",      Arg.String (fun s -> graph_fname := Some s),    "Save the search graph");
        ("-injg",       Arg.String (fun s -> injg_fname := Some s),     "Save the injection grammar")
    ] in
    let usage = "Error: grammar, goal and oracle are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !oracle_fname <> None && !goal <> None then
        let grammar = Option.get !grammar
        and goal = Option.get !goal
        and oracle_fname = Option.get !oracle_fname in

        let values = Hashtbl.create 100 in
(*        Hashtbl.add values (Grammar.Terminal "value") "/tmp";
        Hashtbl.add values (Grammar.Terminal "key") "dir";
        Hashtbl.add values (Grammar.Nonterminal "Exe") "ls";*)
        let fuzzer = Tree_fuzzer.fuzzer (Tree_fuzzer.explode !avoid) 0 (Some values) in

        let fuzzer_oracle (g: Grammar.grammar) : bool = g |> fuzzer |> Option.map Grammar.string_of_word |> Oracle.oracle_mem_from_script oracle_fname in

        let g = Blind.search fuzzer_oracle grammar goal !start !max_depth !graph_fname in match g with
        | None -> print_endline "No grammar found"
        | Some inj_g -> print_endline ("Injection:  "^(Grammar.string_of_word (Option.get (fuzzer (Grammar.grammar_of_ext_grammar inj_g))))); Option.iter (fun f -> Grammar_io.export_bnf f inj_g) !injg_fname
    else print_endline usage
