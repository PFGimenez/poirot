open Grammar

let ()=
    let explode s = List.init (String.length s) (String.get s) in

(*    let parenth_oracle (prefix: part) (suffix: part) (inj: part) : Oracle.oracle_status =
        let rec parenth_oracle_aux (word: part) (stack: part): Oracle.oracle_status = match (word,stack) with
            | [],_ -> if stack = [] then No_error else Syntax_error
            | ((Terminal "(") as t)::q,_ -> parenth_oracle_aux q (t::stack)
            | ((Terminal "[") as t)::q,_ -> parenth_oracle_aux q (t::stack)
            | (Terminal ")")::q1,(Terminal "(")::q2 -> parenth_oracle_aux q1 q2
            | (Terminal ")")::_,_ -> Syntax_error
            | (Terminal "]")::q1,(Terminal "[")::q2 -> parenth_oracle_aux q1 q2
            | (Terminal "]")::_,_ -> Syntax_error
            | (Terminal "b")::q1,[] -> parenth_oracle_aux q1 []
            | (Terminal "b")::_,_::_ -> Syntax_error
            | _::q,_ -> parenth_oracle_aux q stack in
        parenth_oracle_aux (prefix@inj@suffix) [] in*)

    let oracle_mem2 (o: part -> Oracle.oracle_status) : part option -> Oracle.oracle_status =
        let mem : (part, Oracle.oracle_status) Hashtbl.t = Hashtbl.create 1000 in
        fun (inj: part option): Oracle.oracle_status -> match inj with
            | None -> Grammar_error
            | Some inj ->
                if Hashtbl.mem mem inj then begin
                    print_endline ("Memoization: "^(string_of_word inj));
                    Hashtbl.find mem inj
                end else begin
                    let answer = o inj in
                    Hashtbl.add mem inj answer;
                    print_endline ((string_of_int (Hashtbl.length mem))^"th call to oracle: "^(string_of_word inj)^" ("^(Oracle.string_of_oracle_status answer)^")");
                    answer
                end in

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
        ("-grammar",    Arg.String (fun s -> grammar := Some (Clean.merge_consecutive_terminals (Clean.simplify_nonterminals (Clean.clean_grammar (Grammar_io.read_bnf_grammar s))))),     "Target grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some (Grammar_io.read_tokens s)),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some (Grammar_io.read_tokens s)),     "Suffix of the request");
        ("-goal",       Arg.String (fun s -> goal := Some (Grammar_io.read_token s)),     "Terminal or nonterminal to reach");
        ("-start",      Arg.String (fun s -> start := Some (Grammar_io.read_tokens s)),     "A valid injection, either terminal or nonterminal");
        ("-avoid",      Arg.Set_string avoid,     "List of characters to avoid");
        ("-maxdepth",   Arg.Set_int max_depth,    "Set the max depth search (default: "^(string_of_int !max_depth)^")");
        ("-sgraph",     Arg.String (fun s -> graph_fname := Some s),    "Save the search graph");
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

(*        let oracle = parenth_oracle prefix suffix and*)
        let oracle = oracle_mem2 (Fuzzer.oracle prefix suffix grammar) and
        fuzzer = Tree_fuzzer.fuzzer 0 None (Some goal) in

        let fuzzer_oracle (g: Grammar.grammar) : Oracle.oracle_status = g |> fuzzer |> oracle in

        let qgraph_channel = Option.map open_out !qgraph_fname in
        Option.iter (fun ch -> output_string ch "digraph {\n") qgraph_channel;

        let g = Inference.search fuzzer_oracle grammar goal !start !max_depth (explode !avoid) !graph_fname qgraph_channel in
        Option.iter (fun ch -> output_string ch "}"; close_out ch) qgraph_channel;

        if g = None then print_endline "No grammar found"
        else begin
            let inj_g = Option.get g in
            print_endline ("Injection:  "^(Grammar.string_of_word (Option.get (fuzzer (Grammar.grammar_of_ext_grammar inj_g)))));
            Option.iter (fun f -> Grammar_io.export_bnf f inj_g) !injg_fname
        end;
    end else print_endline usage
