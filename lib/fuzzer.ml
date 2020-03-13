open Grammar

type parse_tree = Leaf of element | Node of element * parse_tree list | Error

(* the fuzzer is deterministic when max_depth = 0
 * this is on purpose, so similar grammars yield same words and the oracle memoization can be exploited *)

let nonrec_rules : (element, rule) Hashtbl.t = Hashtbl.create 500

(* all nonterminal must be the left-hand side of a rule *)
let fuzzer (max_depth: int) (values: (element,string) Hashtbl.t option) (goal: element option) (g : grammar) : part option =
    Random.self_init ();
    Hashtbl.clear nonrec_rules;

    let compare_rule (reverse: bool) (r1: rule) (r2: rule) : int =
        let r1, r2 = if reverse then r2,r1 else r1,r2 in
        let diff = List.compare_lengths (List.filter is_non_terminal r1.right_part) (List.filter is_non_terminal r2.right_part) in
        if diff <> 0 then diff
        else begin
            let diff2 = List.compare_lengths r1.right_part r2.right_part in
            if diff2 <> 0 then diff2
            else (Hashtbl.hash r2.right_part) - (Hashtbl.hash r1.right_part) (* to be deterministic we want to compare any couple *)
        end in

    (* like map, but stop whenever the fuzzing fails in one branch *)
    (* not tail-recursive *)
    let rec lazy_map (f: element -> parse_tree) (l: element list) : parse_tree list = match l with
        | [] -> []
        | t::q -> begin match f t with
                | Error -> [Error]
                | x -> x::(lazy_map f q) end in

    let rec fuzzer_minimize (depth: int) (goal_rules: rule list) (used_symbols: element list) (used_rules: rule list) (root: element) : parse_tree =
        if Option.map (fun v -> Hashtbl.mem v root) values = Some true then
            Leaf (Terminal (Hashtbl.find (Option.get values) root))
        else if is_terminal root then
            Leaf root
        else if goal_rules <> [] then begin
            let r = List.hd goal_rules in
            if List.tl goal_rules = [] then
                (* don't update used_symbols as the path to goal may require producing a tree with two identical symbols in the same branch *)
                Node (root,List.map (fuzzer_minimize (depth+1) [] used_symbols (r::used_rules)) r.right_part)
            else begin
                let q = List.tl goal_rules in
                let next_sym = (List.hd q).left_symbol in
                let first = List.find ((=) next_sym) r.right_part in
                (* the PHYSICAL equality is not an error : we want the first, not all of them *)
                Node (root,List.map (fun e -> fuzzer_minimize (depth+1)(if e == first then q else []) used_symbols (r::used_rules) e) r.right_part)
            end
        end else if List.mem root used_symbols then Error
        else begin
            (* goal_rules is empty *)
            match Hashtbl.find_opt nonrec_rules root with
            | Some r -> Node (root,List.map (fuzzer_minimize (depth+1)[] (root::used_symbols) (r::used_rules)) r.right_part) (* a good rule is already known *)
            | None ->
                (* get the rules root -> ... that are not used and with unused symbols in the RHS *)
                let possible_rules = List.sort (compare_rule (depth=0)) (List.filter (fun r -> r.left_symbol = root && not (List.mem r used_rules) && not (List.exists (fun e -> List.mem e used_symbols) r.right_part)) g.rules) in
                use_rule depth possible_rules (root::used_symbols) used_rules root
        end

    and use_rule (depth: int) (possible_rules: rule list) (used_symbols: element list) (used_rules: rule list) (root: element) : parse_tree = match possible_rules with
        | [] -> Error
        | r::q ->
            let trees = lazy_map (fuzzer_minimize (depth+1)[] used_symbols (r::used_rules)) r.right_part in
            if List.exists ((=) Error) trees then
                (use_rule [@tailcall]) depth q used_symbols used_rules root
            else begin
                (* we need to verify the hashtable after the recursive call. This rule is safe since the recursive call succeeded! *)
                assert (not (Hashtbl.mem nonrec_rules root));
                Hashtbl.replace nonrec_rules root r;
                Node (root,trees)
            end in

    (* not tail-recursive ! but the max depth is controlled *)
    let rec fuzzer_explode (depth: int) (e: element) : parse_tree =
        if Option.map (fun v -> Hashtbl.mem v e) values = Some true then begin
            let all_bindings = Hashtbl.find_all (Option.get values) e in
            Leaf (Terminal (List.nth all_bindings (Random.int (List.length all_bindings))))
        end else if is_terminal e then
            Leaf e
        else if depth >= max_depth then
            fuzzer_minimize 1 [] [] [] e
            (*fuzzer_minimize [] [] e*)
        else begin
            let possible_rules = List.filter (fun r -> r.left_symbol = e) g.rules in
            let r = List.nth possible_rules (Random.int (List.length possible_rules)) in (* random rule *)
            let trees = List.map (fuzzer_explode (depth + 1)) r.right_part in
            if List.exists ((=) Error) trees then
                (fuzzer_explode [@tailcall]) depth e (* restart *)
            else Node (e,trees)
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

    let find_path_to_goal () = find_path_to_goal_aux [] [([g.axiom],[])] in

    let rec part_of_tree (verbose_pf: string) (t: parse_tree) : part option = match t with
        | Error -> None
        | Leaf e -> Log.L.debug (fun m -> m "%s" (verbose_pf^(decorated_string_of_element e))); Some [e]
        | Node (e,l) -> Log.L.debug (fun m -> m "%s" (verbose_pf^(decorated_string_of_element e)));
                let trees = List.map (part_of_tree (verbose_pf^"   ")) l in
            if List.exists ((=) None) trees then None
            else Some (trees |> List.map Option.get |> List.flatten) in

    part_of_tree "" (
        if Option.is_some goal && is_reachable g (Option.get goal) g.axiom then begin
            Log.L.debug (fun m -> m "Fuzzing with goal");
            fuzzer_minimize 1 (find_path_to_goal ()) [] [] g.axiom
(*        fuzzer_minimize (find_path_to_goal ()) [] g.axiom*)
        end else begin
            Log.L.debug (fun m -> m "Fuzzing");
            fuzzer_explode 0 g.axiom
        end)
