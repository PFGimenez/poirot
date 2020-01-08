open Grammar

type parse_tree = Leaf of element | Node of parse_tree list

(* the fuzzer is deterministic when th = 0
 * this is on purpose, so similar grammars yield same words and the oracle memoization can be exploited *)

(* all nonterminal must be the left-hand side of a rule *)
let fuzzer (th: int) (values: (element,string) Hashtbl.t option) (g : grammar) : part =
    Random.self_init ();
    let nb_nodes = ref 0
    and nonrec_rules : (element, rule) Hashtbl.t = Hashtbl.create (List.length g.rules) in

    let compare_rule (r1: rule) (r2: rule) : int =
        let diff = List.compare_lengths (List.filter is_non_terminal r1.right_part) (List.filter is_non_terminal r2.right_part) in
        if diff <> 0 then diff
        else begin
            let diff2 = List.compare_lengths r1.right_part r2.right_part in
            if diff2 <> 0 then diff2
            else (Hashtbl.hash r2) - (Hashtbl.hash r1) (* to be deterministic we want to compare any couple *)
        end in

    (* not tail-recursive ! *)
    let rec fuzzer_aux (used_rules: rule list) (e: element) : parse_tree =
        nb_nodes := (!nb_nodes)+1;
        let explode = !nb_nodes < th in
        if Option.map (fun v -> Hashtbl.mem v e) values = Some true then
            Leaf (Terminal (Hashtbl.find (Option.get values) e))
        else if is_terminal e then
            Leaf e
        else begin
            let possible_rules = List.filter (fun r -> r.left_symbol = e && not (List.mem r used_rules)) g.rules in
            if Hashtbl.mem nonrec_rules e && (not explode) then
                let r = Hashtbl.find nonrec_rules e in
                Node (List.map (fuzzer_aux (r::used_rules)) r.right_part)
            else begin
                if possible_rules = [] then failwith "The grammar is not cleaned";
                let r =
                    if explode then (List.nth possible_rules (Random.int (List.length possible_rules))) (* random rule *)
                    else List.hd (List.sort_uniq compare_rule possible_rules) in (* rules with least nonterminals *)
                let new_used_rules = if explode then used_rules else r::used_rules in (* rules in the exploding area are ignored for duplication purpose *)
                let trees = List.map (fuzzer_aux (new_used_rules)) r.right_part in
                (* we need to verify the hashtable after the recursive call *)
                if not (Hashtbl.mem nonrec_rules e) then Hashtbl.add nonrec_rules e r;
                Node trees
            end
        end in

    let rec part_of_tree (t: parse_tree) : part = match t with
        | Leaf e -> [e]
        | Node l -> l |> List.map part_of_tree |> List.flatten in
    let s = part_of_tree (fuzzer_aux [] g.axiom) in
(*    print_endline ("Fuzzer: "^(string_of_word s));*)
    s
