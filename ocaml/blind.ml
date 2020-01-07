open Grammar

exception No_trivial_injection
exception Unknown_goal

let search (fuzzer_oracle: grammar -> bool) (g: grammar) (goal: element) (max_depth: int) (graph_fname: string option) : ext_grammar option =
    let quotient = Rec_quotient.quotient_mem g
    and all_sym = g.rules |> List.rev_map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare in
    let distance_to_goal : (element * element, int) Hashtbl.t = Hashtbl.create ((List.length all_sym)*(List.length all_sym)) in
    let graph_channel = Option.map open_out graph_fname in
    Option.iter (fun ch -> output_string ch "digraph {\n") graph_channel;

    let symbols_from_parents (axiom : element) : element list =
        g.rules |> List.filter (fun r -> List.mem axiom r.right_part) |> List.rev_map (fun r -> r.left_symbol) |> List.sort_uniq compare in

    let add_edge_in_graph (from: ext_element) (dest: ext_element): unit =
        Option.iter (fun ch -> output_string ch ("\""^(string_of_ext_element from)^"\"->\""^(string_of_ext_element dest)^"\"\n")) graph_channel in

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

    (* compute all the distances one and for all *)
    all_sym |> List.iter (fun e -> all_sym |> List.iter (compute_one_distance e));

    (* compare for the open set sorting *)
    let compare_with_score (a: int * int * ext_element) (b: int * int * ext_element) : int = match a,b with
        | (ag,ah,_),(bg,bh,_) when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
        | (ag,ah,_),(bg,bh,_) when ag=bg && ah=bh -> 0
        | _ -> 1 in

    (* add new elements to the open set *)
    let add_in_list g (openset: (int * int * ext_element) list) (new_elems: ext_element list) : (int * int * ext_element) list =
    (* openset is already sorted *)
    new_elems |> List.rev_map (fun (e: ext_element) : (int * int * ext_element) -> (g,get_distance_to_goal (element_of_ext_element e),e)) |> List.sort compare_with_score |> List.merge compare_with_score openset in

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
    let rec search_aux (closedset: (ext_element, bool) Hashtbl.t) (step: int) (openset: (int * int * ext_element) list) : ext_grammar option = match openset with
    | [] -> None (* openset is empty : there is no way *)
    | (distance,_,t)::q ->
        print_endline ("Search "^(string_of_int step)^" (queue: "^(string_of_int (List.length q))^"): "^(string_of_ext_element t));
        assert (distance <= max_depth);
        (* verify whether t has already been visited *)
        if Hashtbl.mem closedset t then
            (print_endline "Visited"; (search_aux [@tailcall]) closedset (step + 1) q)
        else begin
            (* now it is visited *)
            Hashtbl.add closedset t true;
            (* first case: t is nonterminad *)
            if is_ext_element_non_terminal t then begin
                (* compute the grammar of injection *)
                let inj_g = quotient t in
                assert (inj_g.ext_axiom = t);
                print_endline ("Heuristic: "^(string_of_int (get_distance_to_goal t.e)));
                (*print_endline (string_of_ext_grammar inj_g);*)
                (* get the rules t -> ... to verify if t is testable or not *)
                let rules = inj_g.ext_rules |> List.filter (fun r -> inj_g.ext_axiom=r.ext_left_symbol) in
                assert ((List.compare_length_with rules 1) >= 0);
                if (List.compare_length_with rules 1) > 0 then begin (* testable *)
                    if not (inj_g |> grammar_of_ext_grammar |> fuzzer_oracle) then (* this grammar has been invalidated by the oracle: ignore *)
                        (print_endline "Invalid";
                        set_node_color_in_graph t "red";
                        (search_aux [@tailcall]) closedset (step + 1) q)
                    else if is_reachable (grammar_of_ext_grammar inj_g) goal (full_element_of_ext_element inj_g.ext_axiom) then (* the goal has been found ! *)
                        (print_endline "Found!";
                        set_node_color_in_graph t "green";
                        print_endline (string_of_ext_grammar inj_g);
                        Some(inj_g))
                    else if distance = max_depth then (* before we explore, verify if the max depth has been reached *)
                        (print_endline "Depth max";
                        set_node_color_in_graph t "orange";
                        (search_aux [@tailcall]) closedset (step + 1) q)
                    else (* we explore in this direction *)
                        (print_endline "Explore";
                        (search_aux [@tailcall]) closedset (step + 1) (add_in_list (distance + 1) q (build_ext_elements t)))
                end else if distance = max_depth then (* max depth reached *)
                    (print_endline "Depth max";
                    set_node_color_in_graph t "orange";
                    (search_aux [@tailcall]) closedset (step + 1) q)
                else begin (* only one rule: its grammar is the same as its predecessor and so can't be invalidated *)
                    assert (rules |> List.hd |> rhs_of_ext_rule |> (<>) []); (* since there is always the trivial injection, it can't be an epsilon rule *)
                    (*let uniq_rhs = (List.hd rules).ext_right_part in
                    (* if the only rule is A_{alpha} -> A_{beta}, then the successors of A_{alpha} are the same as A_{beta} and are already in the openset/closedset *)
                    if List.compare_length_with uniq_rhs 1 = 0 && (List.hd uniq_rhs).e = t.e then
                        (print_endline "Infinite"; (search_aux [@tailcall]) closedset (step + 1) q)
                    else*)
                    (print_endline "Not testable";
                    set_node_color_in_graph t "grey";
                    (search_aux [@tailcall]) closedset (step + 1) (add_in_list (distance + 1) q (build_ext_elements t)))
                end
            end
            else (* t is terminal *)
                (search_aux [@tailcall]) closedset (step + 1) (add_in_list (distance + 1) q (build_ext_elements t))
            end in
    let inj = get_all_symbols g |> List.filter (fun e -> e@@g.rules |> fuzzer_oracle) in (* get the possible injections tokens *)
    if not (is_reachable g goal g.axiom) then raise Unknown_goal (* the goal is not reachable from the axiom ! *)
    else if inj = [] then raise No_trivial_injection (* no injection token found *)

    else begin
        let out = match List.find_opt (is_reachable g goal) inj with
        | Some(ip) -> Some(ext_grammar_of_grammar (ip@@g.rules)) (* We verify if we can achieve the goal without doing any actual research *)
        | None -> inj |> List.rev_map ext_element_of_element |> add_in_list 0 [] |> search_aux (Hashtbl.create 1000) 0 (* search *) in
        Option.iter (fun ch -> output_string ch "}"; close_out ch) graph_channel;
        out
    end
