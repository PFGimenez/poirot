open Grammar

let inf = 100000

type node_origin = DERIVATION | INDUCTION

type node = {g_val: int; h_val: int; e: ext_element; par: ext_element; origin: node_origin}

let symbols_from_parents (g: grammar) (axiom : element) : element list =
    g.rules |> List.filter (fun r -> List.mem axiom r.right_part) |> List.rev_map (fun r -> r.left_symbol) |> List.sort_uniq compare

let search (fuzzer_oracle: grammar -> Oracle.oracle_status) (unclean_g: grammar) (goal: element) (start: element list option) (max_depth: int) (max_steps: int) (forbidden: char list) (graph_fname: string option) (qgraph_channel: out_channel option) (verbose: bool) : ext_grammar option =
    if verbose then print_endline "Clean grammar…";
    let g = Clean.clean_grammar unclean_g in (* clean is necessary *)
    let quotient = Quotient.quotient_mem g qgraph_channel
    and all_sym = g.rules |> List.rev_map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare in
    let heuristic : (ext_element, int) Hashtbl.t = Hashtbl.create ((List.length g.rules)*(List.length all_sym)) in
    let reachable : (ext_element, bool) Hashtbl.t = Hashtbl.create ((List.length g.rules)*(List.length all_sym)) in
    let uniq_rule : (element, bool) Hashtbl.t = Hashtbl.create (List.length all_sym) in
    let seen_hashtbl = Hashtbl.create (List.length all_sym) in

    List.iter (fun e -> Hashtbl.add uniq_rule e ((List.compare_length_with (List.filter (fun r -> r.left_symbol = e) g.rules) 1) == 0)) all_sym;

    let graph_channel = Option.map open_out graph_fname in

    let set_init_node : ext_element -> unit =
        Grammar_io.set_node_attr graph_channel "shape=doublecircle" in

    let set_node_color_in_graph: ext_element -> string -> unit =
        Grammar_io.set_node_color_in_graph graph_channel in

    let is_allowed ({e;_}: ext_element): bool = match e with
        | Terminal s -> not (List.exists (String.contains s) forbidden)
        | _ -> true in

     (* compute the non-trivial grammar. To do that, just add a new axiom with the same rules as the normal axiom EXCEPT the trivial rule (the rule that leads to the parent grammar) *)
    let make_non_trivial_grammar (g: ext_grammar) (par: ext_element) : ext_grammar =
        let e = g.ext_axiom in
        let dummy_axiom : ext_element = {e=Nonterminal ((string_of_element e.e)^"_dummy_axiom"); pf=e.pf; sf=e.sf} in
        let new_rules = g.ext_rules |> List.filter (fun r -> r.ext_left_symbol = e && r.ext_right_part <> [par]) |> List.map (fun r -> dummy_axiom ---> r.ext_right_part) in
        let allowed_rules = List.filter (fun r -> List.for_all is_allowed r.ext_right_part) (new_rules @ g.ext_rules) in
        dummy_axiom @@@ allowed_rules in

    (* get all the possible prefix/suffix surrounding an element in the rhs on a rule to create the new ext_elements *)
    let split (e: ext_element) (original_rule: rule) : (ext_element * ext_element) list =
        let rec split_aux (prefix : element list) (acc: (ext_element * ext_element) list) (rhs: part) : (ext_element * ext_element) list = match rhs with
        | [] -> acc
        | t::q when t=e.e -> (split_aux [@tailcall]) (t::prefix) (({pf=prefix;e=original_rule.left_symbol;sf=q}, {pf=e.pf@prefix;e=original_rule.left_symbol;sf=e.sf@q})::acc) q
        | t::q -> (split_aux [@tailcall]) (t::prefix) acc q in
    split_aux [] [] original_rule.right_part in

    let is_reachable_mem (par: element option) (e: ext_element) : bool =
        match Hashtbl.find_opt reachable e with
        | Some b -> b
        | None -> let g_local = match par with
            | Some p -> make_non_trivial_grammar (quotient e) (ext_element_of_element p)
            | None -> quotient e in
            let b = is_reachable (grammar_of_ext_grammar g_local) goal (full_element_of_ext_element e) in
            Hashtbl.add reachable e b; b in

    (* breadth-first search *)
    let rec compute_heuristic (queue: (element * ext_element) list list) : ext_element list = match queue with
    | [] -> []
    | []::_ -> failwith "impossible"
    | ((_,t)::_)::q2 when Hashtbl.mem seen_hashtbl t -> (compute_heuristic [@tailcall]) q2
    | (((p,t)::_) as t2)::_ when (not (Hashtbl.find uniq_rule t.e)) && is_reachable_mem (Some p) t -> List.map snd t2 (* plusieurs règles pour t et objectif accessible : on arrête *)
    | (((_,t)::_) as t2)::q2 -> Hashtbl.add seen_hashtbl t ();
                    let neighbours = g.rules
                                    |> List.map (split (ext_element_of_element t.e))
                                    |> List.flatten
                                    |> List.map (fun (e,_) -> (t.e,e)::t2) in
                    (compute_heuristic [@tailcall]) (q2@neighbours) in

    (* compute the heuristic if needed *)
    let get_heuristic (eh: ext_element) (par: element) : int =
(*        print_endline ("get_heuristic of "^(string_of_ext_element eh));*)
        if not (Hashtbl.mem heuristic eh) then begin
            Hashtbl.clear seen_hashtbl;
            let path = compute_heuristic [[(par,eh)]] in
            if path <> [] then List.iteri (fun index elem -> (*print_endline ((string_of_ext_element elem)^" "^(string_of_int index));*) Hashtbl.add heuristic elem index) path
            else Hashtbl.add heuristic eh inf
        end;
        Hashtbl.find heuristic eh in

   (* compare for the open set sorting *)
    let compare_with_score (a: node) (b: node) : int = match a,b with
        | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
        | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag=bg && ah=bh -> (List.length (b.e.pf) + List.length (b.e.sf)) - (List.length (a.e.pf) + List.length (a.e.sf)) (* prefer the longest prefix/suffix *)
        | _ -> 1 in

    (* construct the new ext_elements (the neighborhood) *)
    let build_ext_elements (e: ext_element) : (ext_element* ext_element) list =
        g.rules |> List.filter (fun r -> List.exists ((=) e.e) r.right_part) |> List.rev_map (split e) |> List.flatten in
(*        List.iter (Grammar_io.add_edge_in_graph graph_channel "" e) new_elems;*)

    (* add new elements to the open set *)
    let add_in_openset (allow_derivation: bool) (g_val: int) (null_h: bool) (origin: node_origin) (par: ext_element) (openset: node list) : node list =
    (* openset is already sorted *)
        let openset = (* first INDUCTION and then DERIVATION *)
            if origin=INDUCTION then
                par
                |> build_ext_elements
                |> List.rev_map (fun (eh, e: ext_element * ext_element) : node -> {g_val;h_val=get_heuristic eh par.e;e=e;par=par;origin=INDUCTION})
                |> List.filter (fun {h_val;_} -> h_val <> inf)
                |> List.sort compare_with_score
                |> List.merge compare_with_score openset
            else openset in
        let openset =
            if allow_derivation && null_h then begin
(*                List.iter (fun p -> print_endline (string_of_part p)) (build_derivation g par.sf);*)
                let new_elems =
                    par.sf
                    |> build_derivation g
                    |> List.rev_map (fun (_, newsf) : node -> {g_val=g_val;h_val=0;e={e=par.e;sf=newsf;pf=par.pf};par=par;origin=DERIVATION})
                    |> List.sort compare_with_score in
(*                List.iter (fun n -> Grammar_io.add_edge_in_graph graph_channel "penwidth=3" par n.e) new_elems;*)
                List.merge compare_with_score openset new_elems
            end else openset in
        openset in

    (* core algorithm : an A* algorithm *)
    let rec search_aux (closedset: (ext_element, unit) Hashtbl.t) (step: int) (openset: node list) : ext_grammar option = match openset with
    | [] -> None (* openset is empty : there is no way *)
    | {g_val;h_val;e;par;origin}::q ->
        if verbose then print_endline ("Search "^(string_of_int step)^" (queue: "^(string_of_int (List.length q))^"): "^(string_of_ext_element e));
        assert (g_val <= max_depth);
        if step > max_steps then
            (if verbose then print_endline "Steps limit reached"; None)
        (* verify whether e has already been visited *)
        else if Hashtbl.mem closedset e then
            (if verbose then print_endline "Visited"; (search_aux [@tailcall]) closedset (step + 1) q)
        else begin
            Grammar_io.set_node_attr graph_channel ("[label=\""^(string_of_ext_element e)^"\nstep="^(string_of_int step)^" g="^(string_of_int g_val)^" h="^(string_of_int h_val)^"\"]") e;
            Grammar_io.add_edge_in_graph graph_channel (if origin=INDUCTION then "" else "penwidth=3") par e;
            (* now it is visited *)
            Hashtbl.add closedset e ();
            (* if this element has only one rule, we know it cannot reach the goal (otherwise it would have be done by its predecessor) *)
            if Hashtbl.find uniq_rule e.e then begin
                if verbose then print_endline "Explore uniq";
                (search_aux [@tailcall]) closedset (step + 1) (add_in_openset false (g_val + 1) (h_val = 0) origin e q)
            end else begin
                (* compute the non-trivial grammar and avoid some characters *)
                let nt_inj_g = if origin=INDUCTION then Clean.clean (make_non_trivial_grammar (quotient e) par) else quotient e in
                (* Grammar_io.export_bnf "out.bnf" nt_inj_g; *)
                (* call the fuzzer/oracle with this grammar *)
                let status = nt_inj_g |> grammar_of_ext_grammar |> fuzzer_oracle in
                if status = Syntax_error then begin (* this grammar has been invalidated by the oracle: ignore *)
                    if verbose then print_endline "Invalid";
                    set_node_color_in_graph e "crimson";
                    (search_aux [@tailcall]) closedset (step + 1) q
                end else if is_reachable (grammar_of_ext_grammar nt_inj_g) goal (full_element_of_ext_element nt_inj_g.ext_axiom) then begin (* the goal has been found ! *)
                    print_endline ("Found on step "^(string_of_int step));
                    set_node_color_in_graph e "forestgreen";
    (*                if verbose then print_endline (string_of_ext_grammar nt_inj_g);*)
                    Some (Clean.clean nt_inj_g)
                end else if g_val = max_depth then begin (* before we explore, verify if the max depth has been reached *)
                    if verbose then print_endline "Depth max";
                    set_node_color_in_graph e "orange";
                    (search_aux [@tailcall]) closedset (step + 1) q
                end else begin (* we explore in this direction *)
                    (* get the rules e -> ... to verify if e is testable or not *)
                    if status = Grammar_error then set_node_color_in_graph e "grey";
                    if verbose then print_endline "Explore";
                    (search_aux [@tailcall]) closedset (step + 1) (add_in_openset true (g_val + 1) (h_val = 0) origin e q)
                end
            end
        end in
    let get_epsilon_possible_symbols (g : grammar) : element list =
        g.rules |> List.filter (fun r -> List.length r.right_part = 0) |> List.rev_map (fun r -> r.left_symbol) |> List.sort_uniq compare in
    let inj = match start with
        | Some l -> l
        | None -> get_all_symbols g |> List.filter (fun e -> e@@g.rules |> fuzzer_oracle |> (=) Oracle.No_error) in
    let inj = match List.mem (Terminal "") inj with
        | true -> (get_epsilon_possible_symbols unclean_g) @ (List.filter ((<>) (Terminal "")) inj)
        | false -> inj in
    (* get the possible injections tokens *)
    if not (is_reachable_mem None (ext_element_of_element g.axiom)) then failwith "Unknown or unreachable goal" (* the goal is not reachable from the axiom ! *)
    else if inj = [] then failwith "No trivial injection" (* no injection token found *)
    else begin
        let result = find_start g goal inj in
        if result <> None then begin
            if verbose then print_endline "Directly injection found!";
            Option.map (fun e -> ext_grammar_of_grammar (e@@g.rules)) result
        end
        else begin
            (* the injection token can't reach the goal *)
            List.iter (fun e -> Hashtbl.add reachable (ext_element_of_element e) false) inj;

            (* prepare the dot files *)
            Option.iter (fun ch -> output_string ch "digraph {\n") qgraph_channel;
            Option.iter (fun ch -> output_string ch "digraph {\n") graph_channel;

            let ext_inj = List.rev_map ext_element_of_element inj in
            List.iter (set_init_node) ext_inj;
            if verbose then print_endline "Computing some heuristic values…";
            let openset = List.fold_right (add_in_openset true 1 false INDUCTION) ext_inj [] in (* injection tokens won't be derived *)
            try
                Sys.catch_break true;
                let result = search_aux (Hashtbl.create 1000) 1 openset (* search *) in
                (* close the dot files *)
                Option.iter (fun ch -> output_string ch "}"; close_out ch) graph_channel;
                Option.iter (fun ch -> output_string ch "}"; close_out ch) qgraph_channel;
                Sys.catch_break false;
                result
            with Sys.Break ->
                Option.iter (fun ch -> output_string ch "}"; close_out ch) graph_channel;
                Option.iter (fun ch -> output_string ch "}"; close_out ch) qgraph_channel;
                raise Sys.Break
        end
    end
