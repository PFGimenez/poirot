open Grammar

(* infinity *)
let inf = 100000

(* the origin of a node *)
type node_origin = DERIVATION | INDUCTION

(* the structure of a node *)
type node = {g_val: int; h_val: int; e: ext_element; par: ext_element; origin: node_origin}

let search (oracle: Oracle.t) (unclean_g: grammar) (goal: element) (start: element list) (oneline_comment: string option) (dict: (element,string) Hashtbl.t option) (max_depth: int) (max_steps: int) (graph_fname: string option) (qgraph_fname: string option) (h_fname: string option) (o_fname: string option) (forbidden: char list) (manual_stop: bool) : (ext_grammar * string) option =

    (* words refused by the user *)
    let refused_words : string list ref = ref [] in

    let closedset: (ext_element, unit) Hashtbl.t = Hashtbl.create 10000 in

    let g_non_comment = Clean.clean_grammar unclean_g in (* clean is necessary *)

    let h_time = ref 0. in
    let user_time = ref 0. in

    (* add the oneline comment if requested *)
    let g = match oneline_comment with
    | Some s -> let g_comment = add_comment g_non_comment s in
                g_comment.axiom @@ ((g_comment.axiom --> [g_non_comment.axiom])::(g_comment.axiom --> [g_non_comment.axiom])::g_non_comment.rules) (* this double rule is just a hack to tell the inference that the new axiom has sereval rules *)
    | None -> g_non_comment in

    let quotient = Quotient.init oneline_comment g_non_comment forbidden dict qgraph_fname
    and all_sym = get_all_symbols g in

    (* load the heuristic *)
    let heuristic : (ext_element, int) Hashtbl.t = match h_fname with
        | Some fname -> begin
                            try let out = Marshal.from_channel (open_in_bin fname) in Log.L.info (fun m -> m "Imported heuristic values from %s" fname); out
                            with _ -> Log.L.info (fun m -> m "New heuristic file: %s" fname); Hashtbl.create ((List.length g.rules)*(List.length all_sym))
                        end
        | None -> Hashtbl.create ((List.length g.rules)*(List.length all_sym)) in

    (* load the oracle calls *)
    Option.iter (Oracle.load_mem oracle) o_fname;

    let iw_fname = Option.map (fun s -> "invalid_"^s) o_fname in

    (* load the invalid words *)
    (* words invalidated by the oracle *)
    let invalid_words : part list ref = match iw_fname with
        | Some fname -> begin
                            try let out = Marshal.from_channel (open_in_bin fname) in Log.L.info (fun m -> m "Imported invalid words from %s" fname); ref out
                            with _ -> Log.L.info (fun m -> m "New invalid words file: %s" fname); ref []
                        end
        | None -> ref [] in

    (* used to know if the heuristic file and invalid words file need to be updated *)
    let initial_heuristic_length = Hashtbl.length heuristic in
    let initial_invalid_length = List.length (!invalid_words) in

    (* tail-recursive *)
    (* build all the possible one-step derivation of part p in the grammar g *)
    let build_derivation (g: grammar) (p: part) : (rule * part) list =
        let rec build_derivation_aux (sofar: part) (acc: (rule * part) list) (p: part) : (rule * part) list = match p with
            | [] -> acc
            | (Terminal _ as t)::q -> (build_derivation_aux [@tailcall]) (t::sofar) acc q
            | (Nonterminal _ as t)::q-> let new_parts = g.rules |> List.filter (fun r -> r.left_symbol = t) |> List.rev_map (fun r -> r,(List.rev sofar)@r.right_part@q) in
                (build_derivation_aux [@tailcall]) (t::sofar) (new_parts@acc) q in
        build_derivation_aux [] [] p in

    (* is the goal reachable from that element ? *)
    let reachable : (ext_element, bool) Hashtbl.t = Hashtbl.create ((List.length g.rules)*(List.length all_sym)) in

    (* element that are the lhs of a single rule *)
    let uniq_rule : (element, bool) Hashtbl.t = Hashtbl.create (List.length all_sym) in

    (* used in the heuristic computation *)
    let seen_hashtbl : (ext_element, unit) Hashtbl.t = Hashtbl.create (List.length all_sym) in

    (* populate the uniq_rule hashtable *)
    List.iter (fun e -> Hashtbl.add uniq_rule e ((List.compare_length_with (List.filter (fun r -> r.left_symbol = e) g.rules) 1) == 0)) all_sym;

    (* open the search graph file if requested *)
    let graph_channel = Option.map open_out graph_fname in

    let set_init_node : ext_element -> unit =
        Grammar_io.set_node_attr graph_channel "shape=doublecircle" in

    let set_node_color_in_graph: ext_element -> string -> unit =
        Grammar_io.set_node_color_in_graph graph_channel in

    (* compute the non-trivial grammar. To do that, just add a new axiom with the same rules as the normal axiom EXCEPT the trivial rule (the rule that leads to the parent grammar) *)
    let make_non_trivial_grammar (g: ext_grammar) (par: ext_element) : ext_grammar =
        let e = g.ext_axiom in
        let dummy_axiom : ext_element = {e=Nonterminal ((string_of_element e.e)^"_dummy_axiom"); pf=e.pf; sf=e.sf} in
        let new_rules = g.ext_rules |> List.filter (fun r -> r.ext_left_symbol = e && r.ext_right_part <> [par]) |> List.rev_map (fun r -> dummy_axiom ---> r.ext_right_part) in
        dummy_axiom @@@ (new_rules @ g.ext_rules) in

    (* get all the possible prefix/suffix surrounding an element in the rhs on a rule to create the new ext_elements *)
    let split (e: ext_element) (original_rule: rule) : (ext_element * ext_element) list =
        let rec split_aux (prefix : element list) (acc: (ext_element * ext_element) list) (rhs: part) : (ext_element * ext_element) list =
            (* print_endline ("Iter "^(string_of_part prefix)^" "^(string_of_int (List.length acc))^" "^(string_of_part rhs)); *)
            match rhs with
        | [] -> acc
        | t::q when t=e.e -> (split_aux [@tailcall]) (t::prefix) (({pf=prefix;e=original_rule.left_symbol;sf=q}, {pf=e.pf@prefix;e=original_rule.left_symbol;sf=e.sf@q})::acc) q
        | t::q -> (split_aux [@tailcall]) (t::prefix) acc q in
        (* print_endline ("split de "^(string_of_ext_element e)^" avec "^(string_of_rule original_rule)); *)
    split_aux [] [] original_rule.right_part in

    let is_reachable_mem (par: element option) (e: ext_element) : bool =
        match Hashtbl.find_opt reachable e with
        | Some b -> b
        | None -> let g_local = match par with
            | Some p -> let g_inj = Quotient.get_grammar quotient e in
                    make_non_trivial_grammar g_inj (ext_element_of_element p)
            | None -> Quotient.get_grammar quotient e in
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
        let start_time = Unix.gettimeofday () in
        if not (Hashtbl.mem heuristic eh) then begin
            Hashtbl.clear seen_hashtbl;
            let path = compute_heuristic [[(par,eh)]] in
            if path <> [] then List.iteri (fun index elem -> (*print_endline ((string_of_ext_element elem)^" "^(string_of_int index));*) Hashtbl.replace heuristic elem index) path
            else Hashtbl.replace heuristic eh inf
        end;
        h_time := !h_time +. (Unix.gettimeofday () -. start_time);
        Hashtbl.find heuristic eh in

   (* compare for the open set sorting *)
    let compare_with_score (a: node) (b: node) : int = match a,b with
        | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
        | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag=bg && ah=bh -> (List.length (b.e.pf) + List.length (b.e.sf)) - (List.length (a.e.pf) + List.length (a.e.sf)) (* prefer the longest prefix/suffix *)
        | _ -> 1 in

    (* construct the new ext_elements (the neighborhood) *)
    let build_ext_elements (e: ext_element) : (ext_element * ext_element) list =
        (* print_endline "Rules built ext elements"; *)
        (* print_endline (string_of_grammar g); *)
        g.rules |> List.filter (fun r -> List.exists ((=) e.e) r.right_part) |> List.rev_map (split e) |> List.flatten in
        (* print_endline ("Children: "^(string_of_ext_part (fst (List.split new_e)))); *)

    (* add new elements to the open set *)
    let add_in_openset (allow_derivation: bool) (g_val: int) (null_h: bool) (origin: node_origin) (par: ext_element) (openset: node list) : node list =
    (* openset is already sorted *)
        let openset = (* first INDUCTION and then DERIVATION *)
            if origin=INDUCTION then
                par
                |> build_ext_elements
                (* |> List.map (fun e -> print_endline ("A:"^(string_of_ext_element (fst e)));e) *)
                |> List.rev_map (fun (eh, e: ext_element * ext_element) : node -> {g_val;h_val=get_heuristic eh par.e;e;par;origin=INDUCTION})
                (* |> List.map (fun x -> print_endline ("B:"^(string_of_ext_element (x.e)^" "^(string_of_int x.h_val)));x) *)
                |> List.filter (fun {h_val;_} -> h_val <> inf)
                (* |> List.map (fun x -> print_endline ("C:"^(string_of_ext_element (x.e)));x) *)
                |> List.sort compare_with_score
                (* |> List.map (fun x -> print_endline ("D:"^(string_of_ext_element (x.e)));x) *)
                |> List.merge compare_with_score openset
            else openset in
        let openset =
            if allow_derivation && null_h then begin (* we only derive if the local axiom can access the goal *)
                let g_val = match origin with
                | INDUCTION -> g_val + 5 (* small malus when beginning the derivation *)
                | DERIVATION -> g_val in
(*                List.iter (fun p -> print_endline (string_of_part p)) (build_derivation g par.sf);*)
                let new_elems =
                    par.sf
                    |> build_derivation g
                    |> List.rev_map (fun (_, newsf) : node -> {g_val;h_val=0;e={e=par.e;sf=newsf;pf=par.pf};par;origin=DERIVATION})
                    |> List.sort compare_with_score in
(*                List.iter (fun n -> Grammar_io.add_edge_in_graph graph_channel "penwidth=3" par n.e) new_elems;*)
                List.merge compare_with_score openset new_elems
            end else openset in
        openset in

    let verify_previous_oracle_calls (e: ext_element) : bool =
        if (!invalid_words <> []) then Log.L.debug (fun m -> m "Verify with %d oracle calls" (List.length !invalid_words));
        List.exists (Quotient.is_in_language quotient e) !invalid_words in

    let stop_search (word: part) : bool =
        let rec stop_search_aux (word: part) : bool =
            print_endline ("Injection found: "^(string_of_word word));
            print_endline "End the search? (Y/n)";
            let s = String.lowercase_ascii (read_line ()) in
                if (s = "y" || s="yes" || s="") then
                    true
                else if (s = "n" || s="no") then begin
                    refused_words := (string_of_word word) :: !refused_words;
                    false
                end else begin
                    print_endline "I didn't understand your answer.";
                    stop_search_aux word
            end in
        let start_time = Unix.gettimeofday () in
        let out = stop_search_aux word in
        user_time := !user_time +. (Unix.gettimeofday () -. start_time);
        out in

    (* core algorithm : an A* algorithm *)
    (* tail recursive *)
    let rec search_aux (step: int) (openset: node list) : (ext_grammar * string) option = match openset with
    | [] -> None (* openset is empty : there is no way *)
    | {g_val;h_val;e;par;origin}::q ->
        assert (g_val <= max_depth);
        if step > max_steps then begin
            Log.L.info (fun m -> m "Steps limit reached");
            None
        (* verify whether e has already been visited *)
        end else if Hashtbl.mem closedset e then begin
            Log.L.debug (fun m -> m "Visited");
            (search_aux [@tailcall]) step q
        end else begin
            Log.L.info (fun m -> m "Search %d (queue: %d): %s" step (List.length q) (string_of_ext_element e));
            Grammar_io.set_node_attr graph_channel ("[label=\""^(Grammar_io.export_ext_element e)^"\nstep="^(string_of_int step)^" g="^(string_of_int g_val)^" h="^(string_of_int h_val)^"\"]") e;
            Grammar_io.add_edge_in_graph graph_channel (if origin=INDUCTION then "" else "penwidth=3") par e;
            (* now it is visited *)
            Hashtbl.add closedset e ();
            (* if this element has only one rule, we know it cannot reach the goal (otherwise it would have be done by its predecessor) *)
            if Hashtbl.find uniq_rule e.e && g_val < max_depth && step < max_steps then begin
                Log.L.debug (fun m -> m "Explore uniq");
                (search_aux [@tailcall]) (step + 1) (add_in_openset false (g_val + 1) (h_val = 0) origin e q)
            (* if this language is invalidated by a new oracle call *)
            end else if verify_previous_oracle_calls e then begin
                    Log.L.debug (fun m -> m "Invalid");
                    set_node_color_in_graph e "crimson";
                    (search_aux [@tailcall]) (step + 1) q
            end else begin
                let word,goal_reached = Quotient.get_injection quotient e (Some goal) in
                (* print_endline "Grammar:"; *)
                (* print_endline (string_of_ext_grammar inj_g); *)
                assert (word <> None);
                let word = Option.get word in
                assert (Quotient.is_in_language quotient e word);
                let word_str = string_of_word word in (* there is always a word as the trivial injection always works *)
                let status = Oracle.call oracle word_str in
                if status = Syntax_error then invalid_words := word::!invalid_words;
                if goal_reached && status = No_error && not (List.mem word_str !refused_words) && (not (manual_stop) || stop_search word) then begin (* the goal has been found ! *)
                    Log.L.info (fun m -> m "Found on step %d" step);
                    set_node_color_in_graph e "forestgreen";
    (*                if verbose then print_endline (string_of_ext_grammar inj_g);*)
                    Some (Clean.clean (Quotient.get_grammar quotient e), word_str)
                end else if step = max_steps then begin (* the end *)
                    Log.L.info (fun m -> m "Steps limit reached");
                    None
                end else if status = Syntax_error then begin (* this grammar has been invalidated by the oracle: ignore *)
                    Log.L.debug (fun m -> m "Invalid");
                    set_node_color_in_graph e "crimson";
                    (search_aux [@tailcall]) (step + 1) q
                end else if g_val = max_depth then begin (* before we explore, verify if the max depth has been reached *)
                    Log.L.debug (fun m -> m "Depth max");
                    set_node_color_in_graph e "orange";
                    (search_aux [@tailcall]) (step + 1) q
                end else begin (* we explore in this direction *)
                    (* get the rules e -> ... to verify if e is testable or not *)
                    Log.L.debug (fun m -> m "Explore");
                    (search_aux [@tailcall]) (step + 1) (add_in_openset true (g_val + 1) (h_val = 0) origin e q)
                end
            end
        end in

    (* initialize the call times *)
    let start_time = Unix.gettimeofday () in

    let finalize () =
        (* print the statistics *)
        Quotient.print_statistics quotient;
        let total_duration = Unix.gettimeofday () -. start_time -. !user_time in
        Log.L.info (fun m -> m "Search duration: %.2fs (inference: %.2fs, heuristic: %.2fs, quotient: %.2fs, oracle: %.2fs, idle: %.2fs)." total_duration (total_duration -. !h_time -. Quotient.get_call_time quotient -. Oracle.get_call_time oracle -. Oracle.get_idle_time oracle) !h_time (Quotient.get_call_time quotient) (Oracle.get_call_time oracle) (Oracle.get_idle_time oracle));
        Log.L.info (fun m -> m "%d calls to oracle." (Oracle.get_call_nb oracle));

        (* close the dot files *)
        Option.iter (fun ch -> Log.L.info (fun m -> m "Save search graph."); output_string ch "}"; close_out ch) graph_channel;
        Quotient.finalizer quotient;

        (* save the heuristics if necessary *)
        match h_fname with
            | Some fname when Hashtbl.length heuristic > initial_heuristic_length -> Log.L.info (fun m -> m "Save heuristic values into %s" fname); Marshal.to_channel (open_out_bin fname) heuristic []
            | _ -> ();

        (* save the invalid words if necessary *)
        match iw_fname with
            | Some fname when List.length !invalid_words > initial_invalid_length -> Log.L.info (fun m -> m "Save invalid words into %s" fname); Marshal.to_channel (open_out_bin fname) !invalid_words []
            | _ -> ();

        (* save the oracle answers if necessary *)
        Option.iter (Oracle.save_mem oracle) o_fname
        in

    let inj = start in

    (* get the possible injections tokens *)
    if not (is_reachable_mem None (ext_element_of_element g.axiom)) then failwith "Unknown or unreachable goal" (* the goal is not reachable from the axiom ! *)
    else if inj = [] then failwith "No trivial injection token" (* no injection token found *)
    else begin
        let result = find_start g goal inj in
        if result <> None then begin
            Log.L.info (fun m -> m "Injection directly found!");
            let e = (ext_element_of_element (Option.get result)) in
            let w,goal_reached = Quotient.get_injection quotient e (Some goal) in
            assert goal_reached;
            Some (Quotient.get_grammar quotient e, string_of_word (Option.get w))
        end
        else begin
            (* the injection token can't reach the goal *)
            List.iter (fun e -> Hashtbl.add reachable (ext_element_of_element e) false) inj;

            (* prepare the dot files *)
            Option.iter (fun ch -> output_string ch "digraph {\n") graph_channel;

            let ext_inj = List.rev_map ext_element_of_element inj in
            List.iter (set_init_node) ext_inj;
            Log.L.debug (fun m -> m "Computing some heuristic values…");
            try
                let openset = List.fold_right (add_in_openset true 1 false INDUCTION) ext_inj [] in (* injection tokens won't be derived *)
                Sys.catch_break true;
                let result = search_aux 1 openset (* search *) in
                finalize ();
                Sys.catch_break false;
                result
            with Sys.Break ->
                (* in case of Crtl+C : save the work *)
                finalize ();
                raise Sys.Break
        end
    end
