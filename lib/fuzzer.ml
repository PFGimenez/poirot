open Grammar

type parse_tree = Leaf of element | Node of element * parse_tree list

(* the fuzzer is deterministic when max_depth = 0
 * this is on purpose, so similar grammars yield same words and the oracle memoization can be exploited *)

let best_rule : (element, part) Hashtbl.t = Hashtbl.create 500

(* all nonterminal must be the left-hand side of a rule *)
let fuzzer (max_depth: int) (values: (element, string) Hashtbl.t option) (goal: element option) (forbidden: char list) (g : grammar) : part option =
    Random.self_init ();
    Hashtbl.clear best_rule;

    let is_allowed (e: element): bool = match e with
        | Terminal s -> not (List.exists (String.contains s) forbidden)
        | _ -> true in

    let allowed_rules = List.filter (fun r -> List.for_all is_allowed r.right_part) g.rules in
    let g = g.axiom @@ allowed_rules in

    let compare_rule (r1: rule) (r2: rule) : int =
        let diff = List.compare_lengths (List.filter is_non_terminal r1.right_part) (List.filter is_non_terminal r2.right_part) in
        if diff <> 0 then diff
        else begin
            let diff2 = List.compare_lengths r1.right_part r2.right_part in
            if diff2 <> 0 then diff2
            else (Hashtbl.hash r2.right_part) - (Hashtbl.hash r1.right_part) (* to be deterministic we want to compare any couple *)
        end in

    (* based on level decomposition *)
    let rec update_best_rule (necessary: element) (original_rules: rule list) : unit =
        if is_terminal necessary || Hashtbl.mem best_rule necessary || original_rules = [] then ()
        else begin
            let usable_rules = List.filter (fun r -> List.for_all (fun s -> is_terminal s || Hashtbl.mem best_rule s) r.right_part) original_rules in
            List.iter (fun r -> Hashtbl.replace best_rule r.left_symbol r.right_part) (List.sort compare_rule usable_rules);
            (update_best_rule [@tailcall]) necessary (List.filter (fun r -> not (Hashtbl.mem best_rule r.left_symbol)) original_rules)
        end in

    (* tail-recursive *)
    let rec fuzzer_minimize (goal_rules: rule list) (word_prefix: element list) (sentential_suffix: element list) : element list = match sentential_suffix, goal_rules with
        | [],_ -> assert (goal_rules = []); List.rev word_prefix
        | t::q,_ when is_terminal t -> (fuzzer_minimize [@tailcall]) goal_rules (t::word_prefix) q
        | t::q,r::q2 when r.left_symbol=t -> (fuzzer_minimize [@tailcall]) q2 word_prefix (r.right_part@q)
        | t::q,_ when Option.map (fun v -> Hashtbl.mem v t) values = Some true ->
                (fuzzer_minimize [@tailcall]) goal_rules ((Terminal (Hashtbl.find (Option.get values) t))::word_prefix) q
        | t::q,_ -> (fuzzer_minimize [@tailcall]) goal_rules word_prefix ((Hashtbl.find best_rule t)@q) in

    (* not tail-recursive ! but the max depth is controlled *)
    let rec fuzzer_explode (depth: int) (e: element) : element list =
        if Option.map (fun v -> Hashtbl.mem v e) values = Some true then begin
            let all_bindings = Hashtbl.find_all (Option.get values) e in
            [Terminal (List.nth all_bindings (Random.int (List.length all_bindings)))]
        end else if is_terminal e then
            [e]
        else if depth >= max_depth then begin
            update_best_rule e g.rules;
            fuzzer_minimize [] [] [e]
        end else begin
            let possible_rules = List.filter (fun r -> r.left_symbol = e) g.rules in
            let r = List.nth possible_rules (Random.int (List.length possible_rules)) in (* random rule *)
            let parts = List.map (fuzzer_explode (depth + 1)) r.right_part in
            List.concat parts;
        end in

    let rec has_new (seen: element list) (p: element list) : bool = match p with
        | [] -> false
        | t::_ when not (List.mem t seen) -> true
        | _::q -> (has_new [@tailcall]) seen q in

    let rec find_path_to_goal_aux (seen: element list) (queue : (part * rule list) list) : rule list =
        assert (goal <> None);
        match queue with
        | [] -> assert false (* we know there is a path since the goal is reachable ! *)
        | (form,path)::_ when List.mem (Option.get goal) form -> List.rev path
        | (form,_)::q when not (has_new seen form) -> (find_path_to_goal_aux [@tailcall]) seen q
        | (form,path)::q -> let new_items = List.map (fun (r,p) -> (p,r::path)) (build_derivation g form) in
            (find_path_to_goal_aux [@tailcall]) (List.sort_uniq compare (form@seen)) (q@new_items) in

    let find_path_to_goal () : rule list =
        let l = find_path_to_goal_aux [] [([g.axiom],[])] in
        l |> List.rev_map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare |> List.iter (fun e -> update_best_rule e g.rules);
        l
        (* we update the best rules for all the rhs of the rules towards the goal *)
    in

    update_best_rule g.axiom g.rules;
    if not (Hashtbl.mem best_rule g.axiom) then None
    else
        if Option.is_some goal && is_reachable g (Option.get goal) g.axiom then begin
            Log.L.debug (fun m -> m "Fuzzing with goal");
            Some (fuzzer_minimize (find_path_to_goal ()) [] [g.axiom])
        end else begin
            Log.L.debug (fun m -> m "Fuzzing");
            Some (fuzzer_explode 20 g.axiom)
        end
