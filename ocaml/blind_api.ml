let ()=
    let graph_fname = ref None in
    let injg_fname = ref None in
    let max_depth = ref 5 in
    let grammar = ref None in
    let prefix = ref None in
    let suffix = ref None in
    let goal = ref None in

    let speclist = [
        ("-maxdepth",   Arg.Set_int max_depth,    "Set the max depth search (default: "^(string_of_int !max_depth)^")");
        ("-graph",      Arg.String (fun s -> graph_fname := Some(s)),    "Save the search graph");
        ("-injg",       Arg.String (fun s -> injg_fname := Some(s)),     "Save the injection grammar");
        ("-grammar",    Arg.String (fun s -> grammar := Some(Grammar_io.read_bnf_grammar s)),     "Save the injection grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some(Grammar_io.read_tokens s)),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some(Grammar_io.read_tokens s)),     "Suffix of the request");
        ("-goal",         Arg.String (fun s -> goal := Some(List.hd (Grammar_io.read_tokens s))),     "Terminal or nonterminal to reach")
    ] in
    let usage = "Error: grammar, prefix, suffix and goal are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !prefix <> None && !suffix <> None && !goal <> None then
        let grammar = Option.get !grammar
        and prefix = Option.get !prefix
        and suffix = Option.get !suffix
        and goal = Option.get !goal in

        let values = Hashtbl.create 100 in
        Hashtbl.add values (Grammar.Terminal("value")) "val1";
        let oracle = Fuzzer.oracle prefix suffix grammar and
(*       let oracle = Oracle.oracle_sql_rootme and*)
(*       let oracle = Tree_fuzzer.parenth_oracle prefix suffix and*)
        fuzzer = Tree_fuzzer.fuzzer in
(*        ignore (Tree_fuzzer.fuzzer grammar);
        exit 0;*)

        let fuzzer_oracle (g: Grammar.grammar) : bool = g |> fuzzer |> oracle in

        let g = Blind.search fuzzer_oracle grammar goal !max_depth !graph_fname in match g with
        | None -> print_endline "No grammar found"
        | Some(inj_g) -> print_endline ("Injection:  "^(Fuzzer.string_inst_of_part values (fuzzer (Grammar.grammar_of_ext_grammar inj_g)))); Option.iter (fun f -> Grammar_io.export_bnf f inj_g) !injg_fname
    else print_endline usage
