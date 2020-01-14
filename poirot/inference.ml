open Grammar

let explode s = List.init (String.length s) (String.get s)

type node_origin = DERIVATION | INDUCTION

type node = {g: int; h: int; e: ext_element; par: ext_element; origin: node_origin}

let search (fuzzer_oracle: grammar -> Oracle.oracle_status) (g: grammar) (goal: element) (start: element list option) (max_depth: int) (forbidden: char list) (graph_fname: string option) (qgraph_channel: out_channel option) : ext_grammar option =
    let quotient = Rec_quotient.quotient_mem g qgraph_channel
    and all_sym = g.rules |> List.rev_map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare in
    let distance_to_goal : (element * element, int) Hashtbl.t = Hashtbl.create ((List.length all_sym)*(List.length all_sym)) in
    let graph_channel = Option.map open_out graph_fname in

    let symbols_from_parents (axiom : element) : element list =
        g.rules |> List.filter (fun r -> List.mem axiom r.right_part) |> List.rev_map (fun r -> r.left_symbol) |> List.sort_uniq compare in

    let set_init_node : ext_element -> unit =
        Grammar_io.set_node_attr graph_channel "shape=doublecircle" in

    let set_node_color_in_graph: ext_element -> string -> unit =
        Grammar_io.set_node_color_in_graph graph_channel in

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

    (* tail-recursive *)
    let rec build_derivation (sofar: part) (acc: part list) (p: part) : part list = match p with
        | [] -> acc
        | (Terminal _ as t)::q -> (build_derivation [@tailcall]) (t::sofar) acc q
        | (Nonterminal _ as t)::q-> let new_parts = g.rules |> List.filter (fun r -> r.left_symbol = t) |> List.rev_map (fun r -> (List.rev sofar)@r.right_part@q) in
            (build_derivation [@tailcall]) (t::sofar) (new_parts@acc) q in


    (* construct the new ext_elements (the neighborhood) *)
    let build_ext_elements (e: ext_element) : ext_element list =
        let new_elems = g.rules |> List.filter (fun r -> List.exists ((=) e.e) r.right_part) |> List.rev_map (split e) |> List.flatten in
        List.iter (Grammar_io.add_edge_in_graph graph_channel "" e) new_elems;
        new_elems in

    (* add new elements to the open set *)
    let add_in_openset (g: int) (origin: node_origin) (par: ext_element) (openset: node list) : node list =
    (* openset is already sorted *)
        let openset = (* first INDUCTION and then DERIVATION *)
            if origin=INDUCTION then par |> build_ext_elements |> List.rev_map (fun (e: ext_element) : node -> {g=g;h=get_distance_to_goal e.e;e=e;par=par;origin=INDUCTION}) |> List.sort compare_with_score |> List.merge compare_with_score openset
            else openset in
        let openset =
            if get_distance_to_goal par.e = 0 then begin
                List.iter (fun p -> print_endline (string_of_part p)) (build_derivation [] [] par.sf);
                let new_elems = par.sf |> build_derivation [] [] |> List.rev_map (fun (newsf: part) : node -> {g=g;h=get_distance_to_goal par.e;e={e=par.e;sf=newsf;pf=par.pf};par=par;origin=DERIVATION}) |> List.sort compare_with_score in
                List.iter (fun n -> Grammar_io.add_edge_in_graph graph_channel "penwidth=3" par n.e) new_elems;
                List.merge compare_with_score openset new_elems
            end else openset in
        openset in

    (* core algorithm : an A* algorithm *)
    let rec search_aux (closedset: (ext_element, bool) Hashtbl.t) (step: int) (openset: node list) : ext_grammar option = match openset with
    | [] -> None (* openset is empty : there is no way *)
    | {g=distance;h=_;e=e;par=par;origin=origin}::q ->
        print_endline ("Search "^(string_of_int step)^" (queue: "^(string_of_int (List.length q))^"): "^(string_of_ext_element e));
        assert (distance <= max_depth);
        (* verify whether e has already been visited *)
        if Hashtbl.mem closedset e then
            (print_endline "Visited"; (search_aux [@tailcall]) closedset (step + 1) q)
        else begin
            (* now it is visited *)
            Hashtbl.add closedset e true;
            (* compute the non-trivial grammar and avoid some characters *)
            (* TODO: non-trivial ne concerne que les inductions *)
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
                (search_aux [@tailcall]) closedset (step + 1) (add_in_openset (distance + 1) origin e q)
            end
        end in
    let inj = match start with
        | Some l -> l
        | None -> get_all_symbols g |> List.filter (fun e -> e@@g.rules |> fuzzer_oracle |> (=) Oracle.No_error) in (* get the possible injections tokens *)
    if not (is_reachable g goal g.axiom) then failwith "Unknown or unreachable goal" (* the goal is not reachable from the axiom ! *)
    else if inj = [] then failwith "No trivial injection" (* no injection token found *)
    else begin
        let result = List.find_opt (fun e -> is_reachable g goal e) inj in
        if result <> None then Option.map (fun e -> ext_grammar_of_grammar (e@@g.rules)) result
        else begin
            Option.iter (fun ch -> output_string ch "digraph {\n") graph_channel;
            let ext_inj = List.rev_map ext_element_of_element inj in
            List.iter (set_init_node) ext_inj;
            let openset = List.fold_right (add_in_openset 1 INDUCTION) ext_inj [] in
            let result = search_aux (Hashtbl.create 1000) 0 openset (* search *) in
            Option.iter (fun ch -> output_string ch "}"; close_out ch) graph_channel;
            result
        end
    end
