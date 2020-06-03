open Grammar

(* infinity *)
let infinity = 100000

exception Unreachable

type heuristic = No_heuristic | Complicated

(* the origin of a node *)
type node_origin = Derivation | Induction

(* the structure of a node *)
type node = {g_val: int; mutable h_val: int; e: ext_element; par: ext_element; origin: node_origin; start: element}

type t = {  mutable refused_elems: element list;
            mutable invalid_words: part list;
            mutable openset: node list;
            can_reach_goal: (element, unit) Hashtbl.t;
            heuristic: (element*element, int) Hashtbl.t; (* parent then child *)
            closedset: (ext_element, unit) Hashtbl.t;
            uniq_rule : (element, bool) Hashtbl.t;
            quotient_g: grammar;
            inference_g: grammar;
            (* h_fname: string option; *)
            iw_fname: string option;
            o_fname: string option;
            manual_stop: bool;
            mutable start_time: float;
            mutable h_time: float;
            mutable user_time: float;
            quotient: Quotient.t;
            htype: heuristic;
            oracle: Oracle.t;
            start: element list;
            goal: element;
            max_depth: int;
            max_steps: int;
            graph_channel: out_channel option}

(* tail-recursive *)
(* build all the possible one-step derivation of part p in the grammar g *)
(* uses the inference grammar *)
let build_derivation (g: grammar) (p: part) : (rule * part) list =
    let rec build_derivation_aux (sofar: part) (acc: (rule * part) list) (p: part) : (rule * part) list = match p with
        | [] -> acc
        | (Terminal _ as t)::q -> (build_derivation_aux [@tailcall]) (t::sofar) acc q
        | (Nonterminal _ as t)::q -> let new_parts = g.rules |> List.filter (fun r -> r.left_symbol = t) |> List.rev_map (fun r -> r,(List.rev sofar)@r.right_part@q) in
            (build_derivation_aux [@tailcall]) (t::sofar) (new_parts@acc) q in
    build_derivation_aux [] [] p

let set_init_node (inf: t): ext_element -> unit =
    Grammar_io.set_node_attr inf.graph_channel "shape=doublecircle"

let set_node_color_in_graph (inf: t): ext_element -> string -> unit =
    Grammar_io.set_node_color_in_graph inf.graph_channel

(* get all the possible prefix/suffix surrounding an element in the rhs on a rule to create the new ext_elements *)
let split (e: ext_element) (original_rule: rule) : ext_element list =
    let rec split_aux (prefix : element list) (acc: ext_element list) (rhs: part) : ext_element list =
        (* print_endline ("Iter "^(string_of_part prefix)^" "^(string_of_int (List.length acc))^" "^(string_of_part rhs)); *)
        match rhs with
        | [] -> acc
        | t::q when t=e.e -> (split_aux [@tailcall]) (t::prefix) ({pf=e.pf@prefix;e=original_rule.left_symbol;sf=e.sf@q}::acc) q
        | t::q -> (split_aux [@tailcall]) (t::prefix) acc q in
    (* print_endline ("split de "^(string_of_ext_element e)^" avec "^(string_of_rule original_rule)); *)
    split_aux [] [] original_rule.right_part

(* compare for the open set sorting *)
let compare_with_score (a: node) (b: node) : int = match a,b with
    | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
    | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag=bg && ah=bh -> (List.length (b.e.pf) + List.length (b.e.sf)) - (List.length (a.e.pf) + List.length (a.e.sf)) (* prefer the longest prefix/suffix *)
    | _ -> 1

(* Get the heuristic. It is already computed. *)
let get_heuristic (inf: t) (ch: element) (par: element) : int =
    match inf.htype with
        | No_heuristic -> 0
        | Complicated -> Option.value ~default:infinity (Hashtbl.find_opt inf.heuristic (ch,par))
        (* ch and par are reversed. Heuristic thinks of "parent" in the sense of the grammar. Inference thinks of "parents" in the sense of the A* search. But the search it bottom up ! *)

(* use the quotient grammar *)
(* populate can_reach_goal. Verify if a element can reach the goal with a path that contains no refused word *)
let update_reach_goal (inf: t) : unit =
    (* tail recursive *)
    let rec update_reach_goal_aux (reachable: element list) : element list =
        let acceptable_rule (r: rule) =
            not (List.mem r.left_symbol reachable) (* not already found *)
            && not (List.mem r.left_symbol inf.refused_elems) (* not refused *)
            && List.exists (fun e -> List.mem e reachable) r.right_part in (* can directly access a symbol that can reach the goal *)

        let new_reachable_elems = List.filter acceptable_rule inf.quotient_g.rules
        |> List.map (fun r -> r.left_symbol) in
        if new_reachable_elems = [] then reachable
        else (update_reach_goal_aux [@tailcall]) (new_reachable_elems @ reachable) in
    Hashtbl.clear inf.can_reach_goal;
    let reach = List.sort_uniq compare (update_reach_goal_aux [inf.goal]) in
    List.iter (fun e -> Hashtbl.replace inf.can_reach_goal e ()) reach

(* update the openset after a change of heuristic *)
let update_openset (inf: t) : unit =
    inf.openset <-
        inf.openset
        |> List.map (fun {g_val;e;par;origin;start;_} -> {g_val;h_val=get_heuristic inf e.e par.e;e;par;origin;start}) (* recompute the heuristic *)
        |> List.filter (fun {h_val;_} -> h_val <> infinity) (* remove node with h=infinity *)
        |> List.sort compare_with_score (* sort again *)

let update_heuristic (inf: t) : unit =
    (* get the number of rules an element can reach the goal with *)
    let children_with_goal_number (e: element) : int =
        if Hashtbl.mem inf.can_reach_goal e then
            e   |> get_all_rhs inf.quotient_g.rules
                |> List.filter (List.exists (fun e2 -> Hashtbl.mem inf.can_reach_goal e2))
                |> List.length
        else 0 in (* if the word has been refused, then it artificially has no child that can reach the goal *)

    (* construct the children of new_par whose heuristic is h *)
    let make_new_couples (new_par: element) (h: int) : (element*element*int) list =
        get_all_rhs inf.quotient_g.rules new_par |> List.concat |> List.sort_uniq compare |> List.map (fun e -> (new_par,e,h)) in

    (* compute the heuristic from the top of the grammar to its leaves *)
    (* all the heuristic is computed at once *)
    let rec update_heuristic_aux (queue: (element*element*int) list) : unit =
        match queue with
            | [] -> ()
            | (par,ch,_)::q when Hashtbl.mem inf.heuristic (par,ch) -> (update_heuristic_aux [@tailcall]) q (* heuristic already computed *)
            | (par,ch,h_par)::q -> begin let children_number = children_with_goal_number par in
                let h = (* search the non-trivial injections. If the children can already reach the goal, then the parent should reach the goal with at least two paths (the trivial and at least one non-trivial) *)
                    if (Hashtbl.mem inf.can_reach_goal ch && children_number >= 2) || (not (Hashtbl.mem inf.can_reach_goal ch) && children_number >= 1) then 0
                    else h_par + 1 in
                Hashtbl.replace inf.heuristic (par,ch) h;
                let new_couples = make_new_couples ch h in
                (update_heuristic_aux [@tailcall]) (q@new_couples) end in

    if inf.htype = Complicated then begin
        let start_time = Unix.gettimeofday () in
        Log.L.info (fun m -> m "Heuristic update");

        (* based on can_reach_goal, so we need to update it as well *)
        update_reach_goal inf;
        Hashtbl.clear inf.heuristic;
        if (Hashtbl.mem inf.can_reach_goal inf.quotient_g.axiom) then begin
            (* update the heuristic *)
            update_heuristic_aux (make_new_couples inf.quotient_g.axiom 0);
            update_openset inf;
            inf.h_time <- inf.h_time +. (Unix.gettimeofday () -. start_time);
            Log.L.debug (fun m -> m "Heuristic updated")
        end else
            (* the goal is unreachable because the axiom can't reach it *)
            raise Unreachable
    end
    (* if inf.hype = No_heuristic, no computation *)

(* construct the new ext_elements (the neighborhood) *)
let build_ext_elements (inf: t) (e: ext_element) : ext_element list =
    inf.inference_g.rules |> List.filter (fun r -> List.exists ((=) e.e) r.right_part) |> List.rev_map (split e) |> List.flatten

(* add new elements to the open set *)
let add_in_openset (inf: t) (allow_derivation: bool) (start: element) (g_val: int) (null_h: bool) (origin: node_origin) (par: ext_element) : unit =
(* openset is already sorted *)
    if origin=Induction then
        inf.openset <-
            par
            |> build_ext_elements inf
            |> List.rev_map (fun (e: ext_element) : node -> {g_val;h_val=get_heuristic inf e.e par.e;e;par;origin=Induction;start=start})
            |> List.filter (fun {h_val;_} -> h_val <> infinity)
            |> List.sort compare_with_score
            |> List.merge compare_with_score inf.openset;
    if allow_derivation && null_h then begin (* we only derive if the local axiom can access the goal *)
        let g_val = match origin with
        | Induction -> g_val + 5 (* small malus when beginning the derivation *)
        | Derivation -> g_val in
        inf.openset <-
            par.sf
            |> build_derivation inf.inference_g
            |> List.rev_map (fun (_, newsf) : node -> {g_val;h_val=0;e={e=par.e;sf=newsf;pf=par.pf};par;origin=Derivation;start=start})
            |> List.sort compare_with_score
            |> List.merge compare_with_score inf.openset
    end

(* verify whether a previous invalid oracle call is in the grammar of e *)
let verify_previous_oracle_calls (inf: t) (e: ext_element) : bool =
    if (inf.invalid_words <> []) then Log.L.debug (fun m -> m "Verify with %d oracle calls" (List.length inf.invalid_words));
    List.exists (Quotient.is_in_language inf.quotient e) inf.invalid_words

(* refuse a word and update the heuristic *)
let refuse_word (inf: t) (e: element) : unit =
    inf.refused_elems <- e::inf.refused_elems;
    update_heuristic inf

(* ask the user whether the injection is correct *)
let stop_search (inf: t) (words: part list) (e: ext_element) : bool =
    let rec stop_search_aux () : bool =
        List.iter (fun w -> print_endline ("Injection proposed: "^(string_of_word w))) words;
        print_endline "Does it contain the goal you seek? (Y/n)";
        let s = String.lowercase_ascii (read_line ()) in
            if (s = "y" || s="yes" || s="") then
                true
            else if (s = "n" || s="no") then begin
                set_node_color_in_graph inf e "blue";
                refuse_word inf e.e;
                Quotient.refuse_injections inf.quotient e.e;
                false
            end else begin
                print_endline "I didn't understand your answer.";
                stop_search_aux ()
        end in
    let start_time = Unix.gettimeofday () in
    let out = stop_search_aux () in
    inf.user_time <- inf.user_time +. (Unix.gettimeofday () -. start_time);
    out

(* core algorithm : an A* algorithm *)
(* tail recursive *)
let rec search_aux (inf: t) (step: int) : (ext_grammar * string list) option =
    match inf.openset with
    | [] -> None (* openset is empty : there is no way *)
    | {g_val;h_val;e;par;origin;start}::q ->
    assert (h_val <> infinity);
    inf.openset <- q;
    (* assert (g_val <= max_depth); *)
    if step > inf.max_steps then begin
        Log.L.info (fun m -> m "Steps limit reached");
        None
    (* verify whether e has already been visited *)
    end else if Hashtbl.mem inf.closedset e then begin
        Log.L.debug (fun m -> m "Visited");
        (search_aux [@tailcall]) inf step
    end else begin
        Log.L.info (fun m -> m "Search %d (queue: %d) : %s (from %s)" step (List.length q) (Quotient.get_possible_query_from_ext_element inf.quotient e start) (string_of_element e.e));
        Log.L.debug (fun m -> m "Queue: %d. Hypothesis: %s, g = %d, h = %d" (List.length q) (string_of_ext_element e) g_val h_val);
        Grammar_io.set_node_attr inf.graph_channel ("[label=\""^(Grammar_io.export_ext_element e)^"\nstep="^(string_of_int step)^" g="^(string_of_int g_val)^" h="^(string_of_int h_val)^"\"]") e;
        Grammar_io.add_edge_in_graph inf.graph_channel (if origin=Induction then "" else "penwidth=3") par e;
        (* now it is visited *)
        Hashtbl.replace inf.closedset e ();
        (* if this element has only one rule, we know it cannot reach the goal (otherwise it would have be done by its predecessor) *)
        if Hashtbl.find inf.uniq_rule e.e && g_val < inf.max_depth && step < inf.max_steps then begin
            Log.L.debug (fun m -> m "Explore uniq");
            add_in_openset inf false start (g_val + 1) (h_val = 0) origin e;
            (search_aux [@tailcall]) inf (step + 1)
        (* if this language is invalidated by a new oracle call *)
        end else if verify_previous_oracle_calls inf e then begin
            Log.L.debug (fun m -> m "Invalid");
            set_node_color_in_graph inf e "crimson";
            (search_aux [@tailcall]) inf (step + 1)
        end else begin
            let goal_reached,words = Quotient.get_injection inf.quotient e in
            (* print_endline "Grammar:"; *)
            (* print_endline (string_of_ext_grammar inj_g); *)
            assert (words <> []); (* there is always a word as the trivial injection always works *)
            let word = List.hd words in (* during the inference, we verify only one word to reduce the oracle calls *)
            (* assert (List.for_all (fun b -> b) (List.map (Quotient.is_in_language inf.quotient e) words)); *)

            (* if (not (Quotient.is_in_language inf.quotient par word)) then *)
                (* print_endline ("Mot trivial !" ^(string_of_word word)); *)

            let status = Oracle.call inf.oracle (string_of_word word) in
            if status = Oracle.Syntax_error then inf.invalid_words <- word::inf.invalid_words;
            if goal_reached && (not (inf.manual_stop) || stop_search inf words e) && status = Oracle.No_error then begin (* the goal has been found ! *)
                Log.L.info (fun m -> m "Found on step %d" step);
                set_node_color_in_graph inf e "forestgreen";
                let status : (part * Oracle.status) list = List.map (fun w -> (w, Oracle.call inf.oracle (string_of_word w))) words in
                let injection = status |> List.filter (fun (_,st) -> st = Oracle.No_error) |> List.map fst |> List.map string_of_word in
(*                if verbose then print_endline (string_of_ext_grammar inj_g);*)
                Some (Clean.clean (Quotient.get_grammar inf.quotient e), injection)
            end else if step = inf.max_steps then begin (* the end *)
                Log.L.info (fun m -> m "Steps limit reached");
                None
            end else if status = Oracle.Syntax_error then begin (* this grammar has been invalidated by the oracle: ignore *)
                Log.L.debug (fun m -> m "Invalid");
                set_node_color_in_graph inf e "crimson";
                (search_aux [@tailcall]) inf (step + 1)
            end else if g_val >= inf.max_depth then begin (* before we explore, verify if the max depth has been reached *)
                Log.L.debug (fun m -> m "Depth max");
                set_node_color_in_graph inf e "orange";
                (search_aux [@tailcall]) inf (step + 1)
            end else begin (* we explore in this direction *)
                (* get the rules e -> ... to verify if e is testable or not *)
                Log.L.debug (fun m -> m "Explore");
                add_in_openset inf true start (g_val + 1) (h_val = 0) origin e;
                (search_aux [@tailcall]) inf (step + 1)
            end
        end
    end

let finalize (inf: t) =
    (* print the statistics *)
    Quotient.print_statistics inf.quotient;
    Oracle.print_mem inf.oracle;
    let total_duration = Unix.gettimeofday () -. inf.start_time -. inf.user_time in
    Log.L.info (fun m -> m "Search duration: %.2fs (inference: %.2fs, heuristic: %.2fs, quotient: %.2fs, fuzzer: %.2fs, oracle: %.2fs, idle: %.2fs)." total_duration (total_duration -. inf.h_time -. Quotient.get_call_time inf.quotient -. Quotient.get_fuzzer_time inf.quotient -. Oracle.get_call_time inf.oracle -. Oracle.get_idle_time inf.oracle) inf.h_time (Quotient.get_call_time inf.quotient) (Quotient.get_fuzzer_time inf.quotient) (Oracle.get_call_time inf.oracle) (Oracle.get_idle_time inf.oracle));

    (* close the dot files *)
    Option.iter (fun ch -> Log.L.info (fun m -> m "Save search graph."); output_string ch "}"; close_out ch) inf.graph_channel;
    Quotient.finalizer inf.quotient;

    (* save the invalid words if necessary *)
    begin match inf.iw_fname with
        | Some fname (*when List.length inf.invalid_words > initial_invalid_length*) -> Log.L.info (fun m -> m "Save invalid words into %s" fname); Marshal.to_channel (open_out_bin fname) inf.invalid_words []
        | _ -> ()
    end;

    (* save the oracle answers if necessary *)
    Option.iter (Oracle.save_mem inf.oracle) inf.o_fname

let init (oracle: Oracle.t) (inference_g: grammar option) (quotient_g: grammar) (goal: element) (start: element list) (oneline_comment: string option) (dict: (element,string) Hashtbl.t option) (max_depth: int) (max_steps: int) (graph_fname: string option) (qgraph_fname: string option) (o_fname: string option) (forbidden: char list) (manual_stop: bool) (htype: heuristic) : t =

    let quotient_g = Clean.clean_grammar quotient_g in (* clean is necessary *)
    let g_non_comment = match inference_g with
    | Some g -> Clean.clean_grammar g
    | None -> quotient_g in

    if inference_g <> None then begin
        Log.L.debug (fun m -> m "Inference grammar: %d rules" (List.length g_non_comment.rules));
        Log.L.debug (fun m -> m "Quotient grammar: %d rules" (List.length quotient_g.rules))
    end else
        Log.L.debug (fun m -> m "Inference/quotient grammar: %d rules" (List.length g_non_comment.rules));

    if inference_g <> None && not (is_subgrammar g_non_comment quotient_g) then
        failwith "The inference grammar should be a subgrammar of the quotient grammar !";

    (* add the oneline comment if requested *)
    let g = match oneline_comment with
    | Some _ -> add_comment_inference g_non_comment
    | None -> g_non_comment in

    let quotient = Quotient.init oneline_comment quotient_g forbidden dict qgraph_fname (Some goal)
    and all_sym = get_all_symbols g in

    let can_reach_goal: (element, unit) Hashtbl.t = Hashtbl.create (List.length all_sym) in

    (* load the oracle calls *)
    Option.iter (Oracle.load_mem oracle) o_fname;

    let iw_fname = Option.map (fun s -> "invalid_"^s) o_fname in

    (* load the invalid words *)
    (* words invalidated by the oracle *)
    let invalid_words : part list = match iw_fname with
        | Some fname -> begin
                            try let out = Marshal.from_channel (open_in_bin fname) in Log.L.info (fun m -> m "Imported invalid words from %s" fname); out
                            with _ -> Log.L.info (fun m -> m "New invalid words file: %s" fname); []
                        end
        | None -> [] in

    (* element that are the lhs of a single rule *)
    let uniq_rule : (element, bool) Hashtbl.t = Hashtbl.create (List.length all_sym) in

    (* populate the uniq_rule hashtable *)
    (* we must verify the quotient_g rules ! *)
    List.iter (fun e -> Hashtbl.replace uniq_rule e ((List.compare_length_with (List.filter (fun r -> r.left_symbol = e) quotient_g.rules) 1) == 0)) all_sym;

    let all_sym = get_all_symbols g in

    let no_start = List.filter (fun s -> not (List.mem s all_sym)) start in
    let start = List.filter (fun s -> List.mem s all_sym) start in
    if no_start <> [] then Log.L.warn (fun m -> m "Unknown starting points: %s" (Grammar.string_of_part no_start));
    if start = [] then failwith "No starting point!"; (* no injection token found *)
    Log.L.debug (fun m -> m "Starting points: %s" (Grammar.string_of_part start));
    if not (List.mem goal all_sym) then failwith "Unknown goal";

    let inf = {   refused_elems = [];
        invalid_words = invalid_words;
        can_reach_goal = can_reach_goal;
        heuristic = Hashtbl.create 10000;
        closedset = Hashtbl.create 10000;
        openset = [];
        uniq_rule = uniq_rule;
        quotient_g = quotient_g;
        inference_g = g;
        o_fname = o_fname;
        iw_fname = iw_fname;
        manual_stop = manual_stop;
        start_time = 0.;
        h_time = 0.;
        user_time = 0.;
        quotient = quotient;
        htype = htype;
        oracle = oracle;
        start = start;
        goal = goal;
        max_depth = max_depth;
        max_steps = max_steps;
        graph_channel = Option.map open_out graph_fname} in

    (* initialize the heuristic *)
    begin
        try
            update_heuristic inf;
        with Unreachable -> failwith "Unreachable goal"
    end;

    if inf.htype = No_heuristic then (* we initialize "can_reach_goal" to verify if the axiom can access it *)
        update_reach_goal inf;

    if not (List.mem goal (get_all_symbols quotient_g)) then failwith "Unknown goal"
    else if not (Hashtbl.mem can_reach_goal quotient_g.axiom) then failwith "Unreachable goal";
    (* if not (is_reachable_mem None (ext_element_of_element quotient_g.axiom)) then failwith "Unknown or unreachable goal" (1* the goal is not reachable from the axiom ! *1) *)
    inf


let search (inf: t) : (ext_grammar * string list) option =
    (* initialize the call times *)
    inf.start_time <- Unix.gettimeofday ();
    begin
        (* prepare the dot files *)
        Option.iter (fun ch -> output_string ch "digraph {\n") inf.graph_channel;

        let ext_inj = List.rev_map ext_element_of_element inf.start in
        List.iter (set_init_node inf) ext_inj;
        try
            inf.openset <- [];
            List.iter (fun (start : ext_element) -> add_in_openset inf true start.e 1 false Induction start) ext_inj; (* injection tokens won't be derived *)
            Sys.catch_break true;
            let result = search_aux inf 1 (* search *) in
            finalize inf;
            Sys.catch_break false;
            result
        with Unreachable ->
            finalize inf;
            None
        | Sys.Break ->
            (* in case of Crtl+C : save the work *)
            finalize inf;
            raise Sys.Break
    end
