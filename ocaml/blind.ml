open Grammar

type node = {g: int; h: int; e: ext_element; par: ext_element option}

let search (fuzzer_oracle: grammar -> bool) (g: grammar) (goal: element) (start: element list option) (max_depth: int) (graph_fname: string option) : ext_grammar option =
    let quotient = Rec_quotient.quotient_mem g
    and all_sym = g.rules |> List.rev_map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare in
    let distance_to_goal : (element * element, int) Hashtbl.t = Hashtbl.create ((List.length all_sym)*(List.length all_sym)) in
    let graph_channel = Option.map open_out graph_fname in
    Option.iter (fun ch -> output_string ch "digraph {\n") graph_channel;

    let symbols_from_parents (axiom : element) : element list =
        g.rules |> List.filter (fun r -> List.mem axiom r.right_part) |> List.rev_map (fun r -> r.left_symbol) |> List.sort_uniq compare in

    (* add an edge in the graphviz output *)
    let add_edge_in_graph (from: ext_element) (dest: ext_element): unit =
        Option.iter (fun ch -> output_string ch ("\""^(string_of_ext_element from)^"\"->\""^(string_of_ext_element dest)^"\"\n")) graph_channel in

    (* color a node in the graphviz output *)
    let set_node_color_in_graph (e: ext_element) (c: string): unit =
        Option.iter (fun ch -> output_string ch ("\""^(string_of_ext_element e)^"\"[color="^c^",style=filled]\n")) graph_channel in

    let rec compute_distance_to_goal (e : element) : (element * int) list -> int = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_reachable g e s -> nb
    | (s,nb)::q -> (compute_distance_to_goal [@tailcall]) e (q@(List.rev_map (fun e -> (e,nb+1)) (symbols_from_parents s))) in

    let compute_one_distance (a: element) (b: element) : unit =
        Hashtbl.add distance_to_goal (a,b) (compute_distance_to_goal b [a,0]) in

    let get_distance_to_goal (e: element) : int =
        Hashtbl.find distance_to_goal (e,goal) in

    (* compute the non-trivial grammar. To do that, just add a new axiom with the same rules as the normal axiom EXCEPT the trivial rule (the rule that leads to the parent grammar) *)
    let make_non_trivial_grammar (g: ext_grammar) (e: ext_element) (par: ext_element) : ext_grammar =
        let dummy_axiom : ext_element = {e=Nonterminal ((string_of_element e.e)^"_dummy_axiom"); pf=e.pf; sf=e.sf} in
        let new_rules = g.ext_rules |> List.filter (fun r -> r.ext_left_symbol = e && r.ext_right_part <> [par]) |> List.map (fun r -> dummy_axiom ---> r.ext_right_part) in
        dummy_axiom @@@ (new_rules @ g.ext_rules) in

    (* compute all the distances one and for all *)
    all_sym |> List.iter (fun e -> all_sym |> List.iter (compute_one_distance e));

    (* compare for the open set sorting *)
    let compare_with_score (a: node) (b: node) : int = match a,b with
        | {g=ag;h=ah;e=_;par=_},{g=bg;h=bh;e=_;par=_} when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
        | {g=ag;h=ah;e=_;par=_},{g=bg;h=bh;e=_;par=_} when ag=bg && ah=bh -> 0
        | _ -> 1 in

    (* add new elements to the open set *)
    let add_in_list (g: int) (openset: node list) (par: ext_element option) (new_elems: ext_element list) : node list =
    (* openset is already sorted *)
        new_elems |> List.rev_map (fun (e: ext_element) : node -> {g=g;h=get_distance_to_goal (element_of_ext_element e);e=e;par=par}) |> List.sort compare_with_score |> List.merge compare_with_score openset in

    (* get all the possible prefix/suffix surrounding an element in the rhs on a rule to create the new ext_elements *)
    let split (e: ext_element) (original_rule: rule) : ext_element list =
        let rec split_aux (prefix : element list) (acc: ext_element list) (rhs: part) : (ext_element) list = match rhs with
        | [] -> acc
        | t::q when t=e.e -> (split_aux [@tailcall]) (t::prefix) ({pf=e.pf@prefix;e=original_rule.left_symbol;sf=e.sf@q}::acc) q
        | t::q -> (split_aux [@tailcall]) (t::prefix) acc q in
    split_aux [] [] original_rule.right_part in

    (* construct the new ext_elements (the neighborhood) *)
    let build_ext_elements (e: ext_element) : ext_element list =
        let new_elems = g.rules |> List.filter (fun r -> List.exists ((=) e.e) r.right_part) |> List.rev_map (split e) |> List.flatten in
        List.iter (add_edge_in_graph e) new_elems;
        new_elems in

    (* core algorithm : an A* algorithm *)
    let rec search_aux (closedset: (ext_element, bool) Hashtbl.t) (step: int) (openset: node list) : ext_grammar option = match openset with
    | [] -> None (* openset is empty : there is no way *)
    | {g=distance;h=_;e=e;par=par}::q ->
        print_endline ("Search "^(string_of_int step)^" (queue: "^(string_of_int (List.length q))^"): "^(string_of_ext_element e));
        assert (distance <= max_depth);
        (* verify whether e has already been visited *)
        if Hashtbl.mem closedset e then
            (print_endline "Visited"; (search_aux [@tailcall]) closedset (step + 1) q)
        else begin
            (* now it is visited *)
            Hashtbl.add closedset e true;
            (* compute the grammar of injection *)
            let inj_g = quotient e in
            print_endline ("Heuristic: "^(string_of_int (get_distance_to_goal e.e)));
            (*print_endline (string_of_ext_grammar inj_g);*)
            (* get the rules e -> ... to verify if e is testable or not *)
            let rules = inj_g.ext_rules |> List.filter (fun r -> inj_g.ext_axiom=r.ext_left_symbol) in
            assert ((List.compare_length_with rules 1) >= 0);
            if Option.is_some par && (List.compare_length_with rules 1) > 0 then begin (* testable *)
                let nt_inj_g = make_non_trivial_grammar inj_g e (Option.get par) in
                if not (nt_inj_g |> grammar_of_ext_grammar |> fuzzer_oracle) then begin (* this grammar has been invalidated by the oracle: ignore *)
                    print_endline "Invalid";
                    set_node_color_in_graph e "red";
                    (search_aux [@tailcall]) closedset (step + 1) q
                end else if is_reachable (grammar_of_ext_grammar nt_inj_g) goal (full_element_of_ext_element nt_inj_g.ext_axiom) then begin (* the goal has been found ! *)
                    print_endline "Found!";
                    set_node_color_in_graph e "green";
                    print_endline (string_of_ext_grammar nt_inj_g);
                    Some nt_inj_g
                end else if distance = max_depth then begin (* before we explore, verify if the max depth has been reached *)
                    print_endline "Depth max";
                    set_node_color_in_graph e "orange";
                    (search_aux [@tailcall]) closedset (step + 1) q
                end else begin (* we explore in this direction *)
                    print_endline "Explore";
                    (search_aux [@tailcall]) closedset (step + 1) (add_in_list (distance + 1) q (Some e) (build_ext_elements e))
                end
            end else if distance = max_depth then begin (* max depth reached *)
                print_endline "Not testable and depth max";
                set_node_color_in_graph e "orange";
                (search_aux [@tailcall]) closedset (step + 1) q
            end else begin 
                (* only one rule: its grammar is the same as its predecessor and so can't be invalidated *)
                (* otherwise, no parent means we already know this is a valid injection *)
                assert (rules |> List.hd |> rhs_of_ext_rule |> (<>) []); (* since there is always the trivial injection, it can't be an epsilon rule *)
                print_endline "Not testable";
                set_node_color_in_graph e "grey";
                (search_aux [@tailcall]) closedset (step + 1) (add_in_list (distance + 1) q (Some e) (build_ext_elements e))
            end
        end in
    let inj = match start with
        | Some l -> l
        | None -> get_all_symbols g |> List.filter (fun e -> e@@g.rules |> fuzzer_oracle) in (* get the possible injections tokens *)
    if not (is_reachable g goal g.axiom) then failwith "Unknown goal" (* the goal is not reachable from the axiom ! *)
    else if inj = [] then failwith "No trivial injection" (* no injection token found *)
    else begin
        let out = inj |> List.rev_map ext_element_of_element |> add_in_list 0 [] None |> search_aux (Hashtbl.create 1000) 0 (* search *) in
        Option.iter (fun ch -> output_string ch "}"; close_out ch) graph_channel;
        out
    end
