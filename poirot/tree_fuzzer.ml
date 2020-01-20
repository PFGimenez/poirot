open Grammar

type parse_tree = Leaf of element | Node of parse_tree list | Error

(* the fuzzer is deterministic when max_depth = 0
 * this is on purpose, so similar grammars yield same words and the oracle memoization can be exploited *)

(* all nonterminal must be the left-hand side of a rule *)
let fuzzer (max_depth: int) (values: (element,string) Hashtbl.t option) (goal: element option) (g : grammar) : part option =
    Random.self_init ();
    let nonrec_rules : (element, rule) Hashtbl.t = Hashtbl.create (List.length g.rules) in

    let compare_rule (r1: rule) (r2: rule) : int =
        let diff = List.compare_lengths (List.filter is_non_terminal r1.right_part) (List.filter is_non_terminal r2.right_part) in
        if diff <> 0 then diff
        else begin
            let diff2 = List.compare_lengths r1.right_part r2.right_part in
            if diff2 <> 0 then diff2
            else (Hashtbl.hash r2.right_part) - (Hashtbl.hash r1.right_part) (* to be deterministic we want to compare any couple *)
        end in

    (* not tail-recursive ! *)
    let rec fuzzer_minimize (goal_rules: rule list) (used_rules: rule list) (e: element) : parse_tree =
        if Option.map (fun v -> Hashtbl.mem v e) values = Some true then
            Leaf (Terminal (Hashtbl.find (Option.get values) e))
        else if is_terminal e then
            Leaf e
        else if goal_rules <> [] then begin
            let r = List.hd goal_rules in
            if List.tl goal_rules = [] then
                Node (List.map (fuzzer_minimize [] (r::used_rules)) r.right_part)
            else begin
                let q = List.tl goal_rules in
                let e = (List.hd q).left_symbol in
                let first = List.find ((=) e) r.right_part in
                (* the PHYSICAL equality is not an error : we want the first, not all of them *)
                Node (List.map (fun e -> fuzzer_minimize (if e == first then q else []) (r::used_rules) e) r.right_part)
            end
        end else begin
            let possible_rules = List.filter (fun r -> r.left_symbol = e && not (List.mem r used_rules)) g.rules in
            if Hashtbl.mem nonrec_rules e then
                let r = Hashtbl.find nonrec_rules e in
                Node (List.map (fuzzer_minimize goal_rules (r::used_rules)) r.right_part)
            else if possible_rules = [] then
                Error
            else begin
                let r = List.hd (List.sort_uniq compare_rule possible_rules) in (* rules with least nonterminals *)
                let new_used_rules = r::used_rules in (* rules in the exploding area are ignored for duplication purpose *)
                let trees = List.map (fuzzer_minimize goal_rules (new_used_rules)) r.right_part in
                if List.exists ((=) Error) trees then
                    (fuzzer_minimize [@tailcall]) goal_rules (new_used_rules) e (* restart with a different rule *)
                else begin
                    (* we need to verify the hashtable after the recursive call *)
                    if not (Hashtbl.mem nonrec_rules e) then Hashtbl.add nonrec_rules e r;
                    Node trees
                end
            end
        end in

    (* not tail-recursive ! but the max depth is controlled *)
    let rec fuzzer_explode (depth: int) (e: element) : parse_tree =
        if Option.map (fun v -> Hashtbl.mem v e) values = Some true then
            Leaf (Terminal (Hashtbl.find (Option.get values) e))
        else if is_terminal e then
            Leaf e
        else if depth >= max_depth then
            fuzzer_minimize [] [] e
        else begin
            let possible_rules = List.filter (fun r -> r.left_symbol = e) g.rules in
            let r = List.nth possible_rules (Random.int (List.length possible_rules)) in (* random rule *)
            let trees = List.map (fuzzer_explode (depth + 1)) r.right_part in
            if List.exists ((=) Error) trees then
                (fuzzer_explode [@tailcall]) depth e (* restart *)
            else Node trees
        end in

    let rec has_new (seen: element list) (p: element list) : bool = match p with
        | [] -> false
        | t::q when not (List.mem t seen) -> true
        | t::q -> (has_new [@tailcall]) seen q in

    let rec find_path_to_goal_aux (seen: element list) (queue : (part * rule list) list) : rule list = match queue with
        | [] -> assert false (* we know there is a path since the goal is reachable ! *)
        | (form,path)::_ when List.mem (Option.get goal) form -> List.rev path
        | (form,path)::q when not (has_new seen form) -> (find_path_to_goal_aux [@tailcall]) seen q
        | (form,path)::q -> let new_items = List.map (fun (r,p) -> (p,r::path)) (build_derivation true g form) in
            (find_path_to_goal_aux [@tailcall]) (List.sort_uniq compare (form@seen)) (q@new_items) in

    let find_path_to_goal () = find_path_to_goal_aux [] [([g.axiom],[])] in

    let rec part_of_tree (t: parse_tree) : part option = match t with
        | Error -> None
        | Leaf e -> Some [e]
        | Node l -> let trees = List.map part_of_tree l in
            if List.exists ((=) None) trees then None
            else Some (trees |> List.map Option.get |> List.flatten) in

    let tree = if Option.is_some goal && is_reachable g (Option.get goal) g.axiom then fuzzer_minimize (find_path_to_goal ()) [] g.axiom
    else (fuzzer_explode 0 g.axiom) in
    part_of_tree tree
