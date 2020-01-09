open Grammar

let explode s = List.init (String.length s) (String.get s)

type node = {g: int; h: int; e: ext_element; par: ext_element}

let search (fuzzer_oracle: grammar -> Oracle.oracle_status) (g: grammar) (goal: element) (start: element list option) (max_depth: int) (forbidden: char list) (graph_fname: string option) : ext_grammar option =
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

    let is_allowed (e: ext_element): bool = match e with
        | {pf=_;sf=_;e=Terminal s} -> not (List.exists (String.contains s) forbidden)
        | _ -> true in


    (* compute the non-trivial grammar. To do that, just add a new axiom with the same rules as the normal axiom EXCEPT the trivial rule (the rule that leads to the parent grammar) *)
    let make_non_trivial_grammar (g: ext_grammar) (e: ext_element) (par: ext_element) : ext_grammar =
        let dummy_axiom : ext_element = {e=Nonterminal ((string_of_element e.e)^"_dummy_axiom"); pf=e.pf; sf=e.sf} in
        let new_rules = g.ext_rules |> List.filter (fun r -> r.ext_left_symbol = e && r.ext_right_part <> [par]) |> List.map (fun r -> dummy_axiom ---> r.ext_right_part) in
        let allowed_rules = List.filter (fun r -> List.for_all is_allowed r.ext_right_part) (new_rules @ g.ext_rules) in
        Clean.clean (dummy_axiom @@@ allowed_rules) in

    (* compute all the distances one and for all *)
    all_sym |> List.iter (fun e -> all_sym |> List.iter (compute_one_distance e));

    (* compare for the open set sorting *)
    let compare_with_score (a: node) (b: node) : int = match a,b with
        | {g=ag;h=ah;e=_;par=_},{g=bg;h=bh;e=_;par=_} when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
        | {g=ag;h=ah;e=_;par=_},{g=bg;h=bh;e=_;par=_} when ag=bg && ah=bh -> 0
        | _ -> 1 in

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

    (* add new elements to the open set *)
    let add_in_openset (g: int) (par: ext_element) (openset: node list) : node list =
    (* openset is already sorted *)
        par |> build_ext_elements |> List.rev_map (fun (e: ext_element) : node -> {g=g;h=get_distance_to_goal (element_of_ext_element e);e=e;par=par}) |> List.sort compare_with_score |> List.merge compare_with_score openset in

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
            (* compute the non-trivial grammar and avoid some characters *)
            let nt_inj_g = make_non_trivial_grammar (quotient e) e par in
            (* call the fuzzer/oracle with this grammar *)
            let status = nt_inj_g |> grammar_of_ext_grammar |> fuzzer_oracle in
            if status = Syntax_error then begin (* this grammar has been invalidated by the oracle: ignore *)
                print_endline "Invalid";
                set_node_color_in_graph e "crimson";
                (search_aux [@tailcall]) closedset (step + 1) q
            end else if is_reachable (grammar_of_ext_grammar nt_inj_g) goal (full_element_of_ext_element nt_inj_g.ext_axiom) then begin (* the goal has been found ! *)
                print_endline "Found!";
                set_node_color_in_graph e "forestgreen";
                print_endline (string_of_ext_grammar nt_inj_g);
                Some nt_inj_g
            end else if distance = max_depth then begin (* before we explore, verify if the max depth has been reached *)
                print_endline "Depth max";
                set_node_color_in_graph e "orange";
                (search_aux [@tailcall]) closedset (step + 1) q
            end else begin (* we explore in this direction *)
                (* get the rules e -> ... to verify if e is testable or not *)
                if status = Grammar_error then set_node_color_in_graph e "grey";
                print_endline "Explore";
                (search_aux [@tailcall]) closedset (step + 1) (add_in_openset (distance + 1) e q)
            end
        end in
    let inj = match start with
        | Some l -> l
        | None -> get_all_symbols g |> List.filter (fun e -> e@@g.rules |> fuzzer_oracle |> (=) Oracle.No_error) in (* get the possible injections tokens *)
    if not (is_reachable g goal g.axiom) then failwith "Unknown goal" (* the goal is not reachable from the axiom ! *)
    else if inj = [] then failwith "No trivial injection" (* no injection token found *)
    else begin
        let ext_inj = List.rev_map ext_element_of_element inj in
        let openset = List.fold_right (add_in_openset 0) ext_inj [] in
        let result = search_aux (Hashtbl.create 1000) 0 openset (* search *) in
        Option.iter (fun ch -> output_string ch "}"; close_out ch) graph_channel;
        result
    end
