open Grammar

(* infinity *)
let infinity = 100000

type heuristic = NO_HEURISTIC | COMPLICATED

(* the origin of a node *)
type node_origin = DERIVATION | INDUCTION

(* the structure of a node *)
type node = {g_val: int; h_val: int; e: ext_element; par: ext_element; origin: node_origin}

type t = {  mutable refused_elems: element list;
            mutable invalid_words: part list;
            can_reach_goal: (element, unit) Hashtbl.t;
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
            (* initial_heuristic_length: int; *)
            (* initial_invalid_length: int} *)

let init (oracle: Oracle.t) (inference_g: grammar option) (quotient_g: grammar) (goal: element) (start: element list) (oneline_comment: string option) (dict: (element,string) Hashtbl.t option) (max_depth: int) (max_steps: int) (graph_fname: string option) (qgraph_fname: string option) (o_fname: string option) (forbidden: char list) (manual_stop: bool) (htype: heuristic) : t =

    let can_reach_goal: (element, unit) Hashtbl.t = Hashtbl.create 1000 in

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

    let quotient = Quotient.init oneline_comment quotient_g forbidden dict qgraph_fname
    and all_sym = get_all_symbols g in

    (* load the heuristic *)
(*    let heuristic : (ext_element, int) Hashtbl.t = match h_fname with
        | Some fname -> begin
                            try let out = Marshal.from_channel (open_in_bin fname) in Log.L.info (fun m -> m "Imported heuristic values from %s" fname); out
                            with _ -> Log.L.info (fun m -> m "New heuristic file: %s" fname); Hashtbl.create ((List.length g.rules)*(List.length all_sym))
                        end
        | None -> Hashtbl.create ((List.length g.rules)*(List.length all_sym)) in*)

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

    (* used to know if the heuristic file and invalid words file need to be updated *)
    (* let initial_heuristic_length = Hashtbl.length heuristic in *)
    (* let initial_invalid_length = List.length (!invalid_words) in *)

    (* element that are the lhs of a single rule *)
    let uniq_rule : (element, bool) Hashtbl.t = Hashtbl.create (List.length all_sym) in
    (* populate the uniq_rule hashtable *)
    (* we must verify the quotient_g rules ! *)
    List.iter (fun e -> Hashtbl.add uniq_rule e ((List.compare_length_with (List.filter (fun r -> r.left_symbol = e) quotient_g.rules) 1) == 0)) all_sym;

    {   refused_elems = [];
        invalid_words = invalid_words;
        can_reach_goal = can_reach_goal;
        closedset = Hashtbl.create 10000;
        uniq_rule = uniq_rule;
        quotient_g = quotient_g;
        inference_g = g;
        (* h_fname = h_fname; *)
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
        graph_channel = Option.map open_out graph_fname}
        (* initial_heuristic_length: Hashtbl.length heuristic; *)
        (* initial_invalid_length: int} *)



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

(* is the goal reachable from that element ? *)
(* let reachable : (ext_element, bool) Hashtbl.t = Hashtbl.create ((List.length g.rules)*(List.length all_sym)) in *)

(* used in the heuristic computation *)
(* let seen_hashtbl : (ext_element, unit) Hashtbl.t = Hashtbl.create (List.length all_sym) in *)

let set_init_node (inf: t): ext_element -> unit =
    Grammar_io.set_node_attr inf.graph_channel "shape=doublecircle"

let set_node_color_in_graph (inf: t): ext_element -> string -> unit =
    Grammar_io.set_node_color_in_graph inf.graph_channel
(*
(* compute the non-trivial grammar. To do that, just add a new axiom with the same rules as the normal axiom EXCEPT the trivial rule (the rule that leads to the parent grammar) *)
let make_non_trivial_grammar (g: ext_grammar) (par: ext_element) : ext_grammar =
    let e = g.ext_axiom in
    let dummy_axiom : ext_element = {e=Nonterminal ((string_of_element e.e)^"_dummy_axiom"); pf=e.pf; sf=e.sf} in
    let new_rules = g.ext_rules |> List.filter (fun r -> r.ext_left_symbol = e && r.ext_right_part <> [par]) |> List.rev_map (fun r -> dummy_axiom ---> r.ext_right_part) in
    dummy_axiom @@@ (new_rules @ g.ext_rules) in
*)
(* get all the possible prefix/suffix surrounding an element in the rhs on a rule to create the new ext_elements *)
let split (e: ext_element) (original_rule: rule) : (ext_element * ext_element) list =
    let rec split_aux (prefix : element list) (acc: (ext_element * ext_element) list) (rhs: part) : (ext_element * ext_element) list =
        (* print_endline ("Iter "^(string_of_part prefix)^" "^(string_of_int (List.length acc))^" "^(string_of_part rhs)); *)
        match rhs with
        | [] -> acc
        | t::q when t=e.e -> (split_aux [@tailcall]) (t::prefix) (({pf=prefix;e=original_rule.left_symbol;sf=q}, {pf=e.pf@prefix;e=original_rule.left_symbol;sf=e.sf@q})::acc) q
        | t::q -> (split_aux [@tailcall]) (t::prefix) acc q in
    (* print_endline ("split de "^(string_of_ext_element e)^" avec "^(string_of_rule original_rule)); *)
    split_aux [] [] original_rule.right_part

(*
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
    let get_heuristic_aux (eh: ext_element) (par: element) : int =
        if htype = NO_HEURISTIC then
            0
        else begin
            if not (Hashtbl.mem heuristic eh) then begin
                Hashtbl.clear seen_hashtbl;
                let path = compute_heuristic [[(par,eh)]] in
                if path <> [] then List.iteri (fun index elem -> (*print_endline ((string_of_ext_element elem)^" "^(string_of_int index));*) Hashtbl.replace heuristic elem index) path
                else Hashtbl.replace heuristic eh inf
            end;
            Hashtbl.find heuristic eh
        end in
    let start_time = Unix.gettimeofday () in
    let out = get_heuristic_aux eh par in
    h_time := !h_time +. (Unix.gettimeofday () -. start_time);
    out in*)

let get_heuristic (_: ext_element) (_: element) : int = 0

(* compare for the open set sorting *)
let compare_with_score (a: node) (b: node) : int = match a,b with
    | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
    | {g_val=ag;h_val=ah;_},{g_val=bg;h_val=bh;_} when ag=bg && ah=bh -> (List.length (b.e.pf) + List.length (b.e.sf)) - (List.length (a.e.pf) + List.length (a.e.sf)) (* prefer the longest prefix/suffix *)
    | _ -> 1

(* construct the new ext_elements (the neighborhood) *)
let build_ext_elements (inf: t) (e: ext_element) : (ext_element * ext_element) list =
    (* print_endline "Rules built ext elements"; *)
    (* print_endline (string_of_grammar g); *)
    inf.inference_g.rules |> List.filter (fun r -> List.exists ((=) e.e) r.right_part) |> List.rev_map (split e) |> List.flatten
    (* print_endline ("Children: "^(string_of_ext_part (fst (List.split new_e)))); *)

(* add new elements to the open set *)
let add_in_openset (inf: t) (allow_derivation: bool) (g_val: int) (null_h: bool) (origin: node_origin) (par: ext_element) (openset: node list) : node list =
(* openset is already sorted *)
    let openset = (* first INDUCTION and then DERIVATION *)
        if origin=INDUCTION then
            par
            |> build_ext_elements inf
            (* |> List.map (fun e -> print_endline ("A:"^(string_of_ext_element (fst e)));e) *)
            |> List.rev_map (fun (eh, e: ext_element * ext_element) : node -> {g_val;h_val=get_heuristic eh par.e;e;par;origin=INDUCTION})
            (* |> List.map (fun x -> print_endline ("B:"^(string_of_ext_element (x.e)^" "^(string_of_int x.h_val)));x) *)
            |> List.filter (fun {h_val;_} -> h_val <> infinity)
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
                |> build_derivation inf.inference_g
                |> List.rev_map (fun (_, newsf) : node -> {g_val;h_val=0;e={e=par.e;sf=newsf;pf=par.pf};par;origin=DERIVATION})
                |> List.sort compare_with_score in
(*                List.iter (fun n -> Grammar_io.add_edge_in_graph graph_channel "penwidth=3" par n.e) new_elems;*)
            List.merge compare_with_score openset new_elems
        end else openset in
    openset

let verify_previous_oracle_calls (inf: t) (e: ext_element) : bool =
    if (inf.invalid_words <> []) then Log.L.debug (fun m -> m "Verify with %d oracle calls" (List.length inf.invalid_words));
    List.exists (Quotient.is_in_language inf.quotient e) inf.invalid_words

let update_reach_goal () : unit =
    ()

let refuse_word (inf: t) (e: element) : unit =
    inf.refused_elems <- e::inf.refused_elems;
    update_reach_goal ()

let stop_search (inf: t) (words: part list) (e: ext_element) : bool =
    let rec stop_search_aux () : bool =
        List.iter (fun w -> print_endline ("Injection found: "^(string_of_word w))) words;
        print_endline "End the search? (Y/n)";
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
let rec search_aux (inf: t) (step: int) (openset: node list) : (ext_grammar * string list) option = match openset with
| [] -> None (* openset is empty : there is no way *)
| {g_val;h_val;e;par;origin}::q ->
    (* assert (g_val <= max_depth); *)
    if step > inf.max_steps then begin
        Log.L.info (fun m -> m "Steps limit reached");
        None
    (* verify whether e has already been visited *)
    end else if Hashtbl.mem inf.closedset e then begin
        Log.L.debug (fun m -> m "Visited");
        (search_aux [@tailcall]) inf step q
    end else begin
        Log.L.info (fun m -> m "Search %d (queue: %d): %s" step (List.length q) (string_of_ext_element e));
        Grammar_io.set_node_attr inf.graph_channel ("[label=\""^(Grammar_io.export_ext_element e)^"\nstep="^(string_of_int step)^" g="^(string_of_int g_val)^" h="^(string_of_int h_val)^"\"]") e;
        Grammar_io.add_edge_in_graph inf.graph_channel (if origin=INDUCTION then "" else "penwidth=3") par e;
        (* now it is visited *)
        Hashtbl.add inf.closedset e ();
        (* if this element has only one rule, we know it cannot reach the goal (otherwise it would have be done by its predecessor) *)
        if Hashtbl.find inf.uniq_rule e.e && g_val < inf.max_depth && step < inf.max_steps then begin
            Log.L.debug (fun m -> m "Explore uniq");
            (search_aux [@tailcall]) inf (step + 1) (add_in_openset inf false (g_val + 1) (h_val = 0) origin e q)
        (* if this language is invalidated by a new oracle call *)
        end else if verify_previous_oracle_calls inf e then begin
            Log.L.debug (fun m -> m "Invalid");
            set_node_color_in_graph inf e "crimson";
            (search_aux [@tailcall]) inf (step + 1) q
        end else begin
            let words,goal_reached = Quotient.get_injection inf.quotient e (Some inf.goal) in
            (* print_endline "Grammar:"; *)
            (* print_endline (string_of_ext_grammar inj_g); *)
            assert (words <> []); (* there is always a word as the trivial injection always works *)
            let word = List.hd words in (* during the inference, we verify only one word to reduce the oracle calls *)
            assert (List.for_all (fun b -> b) (List.map (Quotient.is_in_language inf.quotient e) words));

            if (not (Quotient.is_in_language inf.quotient par word)) then
                print_endline ("Mot trivial !" ^(string_of_word word));
            let status = Oracle.call inf.oracle (string_of_word word) in
            if status = Oracle.Syntax_error then inf.invalid_words <- word::inf.invalid_words;
            if goal_reached && status = Oracle.No_error && (not (inf.manual_stop) || stop_search inf words e) then begin (* the goal has been found ! *)
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
                (search_aux [@tailcall]) inf (step + 1) q
            end else if g_val >= inf.max_depth then begin (* before we explore, verify if the max depth has been reached *)
                Log.L.debug (fun m -> m "Depth max");
                set_node_color_in_graph inf e "orange";
                (search_aux [@tailcall]) inf (step + 1) q
            end else begin (* we explore in this direction *)
                (* get the rules e -> ... to verify if e is testable or not *)
                Log.L.debug (fun m -> m "Explore");
                (search_aux [@tailcall]) inf (step + 1) (add_in_openset inf true (g_val + 1) (h_val = 0) origin e q)
            end
        end
    end

let finalize (inf: t) =
    (* print the statistics *)
    Quotient.print_statistics inf.quotient;
    let total_duration = Unix.gettimeofday () -. inf.start_time -. inf.user_time in
    Log.L.info (fun m -> m "Search duration: %.2fs (inference: %.2fs, heuristic: %.2fs, quotient: %.2fs, oracle: %.2fs, idle: %.2fs)." total_duration (total_duration -. inf.h_time -. Quotient.get_call_time inf.quotient -. Oracle.get_call_time inf.oracle -. Oracle.get_idle_time inf.oracle) inf.h_time (Quotient.get_call_time inf.quotient) (Oracle.get_call_time inf.oracle) (Oracle.get_idle_time inf.oracle));
    Log.L.info (fun m -> m "%d calls to oracle." (Oracle.get_call_nb inf.oracle));

    (* close the dot files *)
    Option.iter (fun ch -> Log.L.info (fun m -> m "Save search graph."); output_string ch "}"; close_out ch) inf.graph_channel;
    Quotient.finalizer inf.quotient;

    (* save the heuristics if necessary *)
    (* match inf.h_fname with *)
        (* | Some fname when Hashtbl.length heuristic > initial_heuristic_length -> Log.L.info (fun m -> m "Save heuristic values into %s" fname); Marshal.to_channel (open_out_bin fname) heuristic [] *)
        (* | _ -> (); *)
        (* TODO *)

    (* save the invalid words if necessary *)
    match inf.iw_fname with
        | Some fname (*when List.length inf.invalid_words > initial_invalid_length*) -> Log.L.info (fun m -> m "Save invalid words into %s" fname); Marshal.to_channel (open_out_bin fname) inf.invalid_words []
        | _ -> ();

    (* save the oracle answers if necessary *)
    Option.iter (Oracle.save_mem inf.oracle) inf.o_fname

let search (inf: t) : (ext_grammar * string list) option =
    (* initialize the call times *)
    inf.start_time <- Unix.gettimeofday ();

    if not (List.mem inf.goal (get_all_symbols inf.quotient_g)) then failwith "Unknown goal"
    else if not (Hashtbl.mem inf.can_reach_goal inf.quotient_g.axiom) then failwith "Unreachable goal"
    (* if not (is_reachable_mem None (ext_element_of_element quotient_g.axiom)) then failwith "Unknown or unreachable goal" (1* the goal is not reachable from the axiom ! *1) *)
    else if inf.start = [] then failwith "No starting point!" (* no injection token found *)
    else begin
    (*        let result = find_start g goal inj in
        if result <> None then begin
            Log.L.info (fun m -> m "Injection directly found!");
            let e = (ext_element_of_element (Option.get result)) in
            let w,goal_reached = Quotient.get_injection quotient e (Some goal) in
            assert goal_reached;
            Some (Quotient.get_grammar quotient e, List.map string_of_word w)
        end
        else*) begin
            (* the injection token can't reach the goal *)
            (* List.iter (fun e -> Hashtbl.add reachable (ext_element_of_element e) false) inj; *)

            (* prepare the dot files *)
            Option.iter (fun ch -> output_string ch "digraph {\n") inf.graph_channel;

            let ext_inj = List.rev_map ext_element_of_element inf.start in
            List.iter (set_init_node inf) ext_inj;
            Log.L.debug (fun m -> m "Computing some heuristic values…");
            try
                let openset = List.fold_right (add_in_openset inf true 1 false INDUCTION) ext_inj [] in (* injection tokens won't be derived *)
                Sys.catch_break true;
                let result = search_aux inf 1 openset (* search *) in
                finalize inf;
                Sys.catch_break false;
                result
            with Sys.Break ->
                (* in case of Crtl+C : save the work *)
                finalize inf;
                raise Sys.Break
        end
    end
