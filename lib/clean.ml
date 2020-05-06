open Grammar

(* change the case (to upper case or to lower case) of a grammar *)
let change_case (f: string -> string) (g: grammar) : grammar =
    let e_change_case (e: element) : element = match e with
        | Terminal s -> Terminal (f s)
        | Nonterminal s -> Nonterminal s in
    let r_change_case (r: rule) : rule =
        (e_change_case r.left_symbol) --> (List.map e_change_case r.right_part) in
    (e_change_case g.axiom) @@ (List.sort_uniq compare (List.map r_change_case g.rules))

(* change a grammar to lower case *)
let to_lowercase : grammar -> grammar = change_case String.lowercase_ascii

(* change a grammar to upper case *)
let to_uppercase : grammar -> grammar = change_case String.uppercase_ascii

(* remove duplicated rules of a grammar *)
let remove_duplicated_rules (g: grammar) : grammar = g.axiom @@ (List.sort_uniq compare g.rules)

(* merge the consecutive terminals of the RHS of the rules. For example, A -> "a" "b" is transformed into A -> "ab" *)
let merge_consecutive_terminals (g: grammar) : grammar =
    let rec merge_consecutive_terminals_aux (p: part) : part = match p with
        | [] -> p
        | _::[] -> p
        | (Terminal s)::(Terminal s2)::l -> (merge_consecutive_terminals_aux [@tailcall]) (Terminal (s^s2)::l)
        | t::l -> t::(merge_consecutive_terminals_aux l) in
    g.axiom @@ (List.map (fun r -> (r.left_symbol --> merge_consecutive_terminals_aux r.right_part)) g.rules)

(* simplify a grammar : if a nonterminal (that is not the axiom) A is the LHS of a single rule, say A -> alpha, then replace each occurence of A in RHS with alpha, and remove the rule of A. *)
let rec simplify_nonterminals (g: grammar) : grammar =
    let rec simplify_one_nonterminal (rhs: part) (rlist: (element*part) list) : part = match rhs with
        | [] -> []
        | t::l when List.mem_assoc t rlist -> List.assoc t rlist @ (simplify_one_nonterminal l rlist)
        | t::l -> t::(simplify_one_nonterminal l rlist) in
    let rec uniq (sofar: element list) (l: element list) (uniq_list: element list) = match l with
        | [] -> uniq_list
        | t::q when not (List.mem t q) && not (List.mem t sofar) -> (uniq [@tailcall]) (t::sofar) q (t::uniq_list)
        | t::q -> (uniq [@tailcall]) (t::sofar) q uniq_list in
    let rec update_rule (elems: element list) (rules: (element*part) list) (r: rule) : rule option =
        if List.mem r.left_symbol elems then None
        else begin
            let new_rhs = simplify_one_nonterminal r.right_part rules in
            let new_r = {left_symbol=r.left_symbol; right_part=new_rhs} in
            if new_rhs = r.right_part then Some new_r
            else update_rule elems rules new_r
        end in
    let simplify_once (g: grammar) : grammar =
        let elems = List.filter ((<>) g.axiom) (uniq [] (List.map (fun r -> r.left_symbol) g.rules) []) in
        let rules = List.map (fun r -> (r.left_symbol,r.right_part)) (List.filter (fun r -> List.mem r.left_symbol elems) g.rules) in
        g.axiom @@ (List.filter_map (update_rule elems rules) g.rules) in
    let new_g = remove_duplicated_rules (simplify_once g) in
    if List.compare_lengths new_g.rules g.rules <> 0 then simplify_nonterminals new_g
    else new_g

(* iterate a function until its grammar size doesn't change *)
let rec iterate_until_convergence (f : ext_grammar -> ext_rule list) (g : ext_grammar) : ext_grammar = let new_rules = f g in
    if List.compare_lengths g.ext_rules new_rules = 0 then g
    else (iterate_until_convergence [@tailcall]) f (g.ext_axiom@@@new_rules)

(* The useful symbols are the non-terminal that are the left side of a production *)
let get_useful_symbols (g : ext_grammar) : ext_element list =
    g.ext_rules |> List.rev_map lhs_of_ext_rule |> List.sort_uniq compare

(* verify if a rule can derive (directly or not) a word *)
let is_rule_useful (useful_s : ext_element list) (t : ext_rule) : bool =
    List.mem t.ext_left_symbol useful_s && List.for_all (fun s -> is_ext_element_terminal s || List.mem s useful_s) t.ext_right_part

let remove_useless_symbols_once (g : ext_grammar) : ext_rule list = List.filter (is_rule_useful (get_useful_symbols g)) g.ext_rules

let remove_useless_symbols : ext_grammar -> ext_grammar = iterate_until_convergence remove_useless_symbols_once

let get_reachable_symbols_once (g : ext_grammar) (slist : ext_element list) : ext_element list =
    g.ext_rules |> List.filter (fun r -> List.mem r.ext_left_symbol slist) |> List.rev_map rhs_of_ext_rule |> List.cons slist |> List.concat |> List.sort_uniq compare

let rec get_reachable_symbols (g : ext_grammar) (slist : ext_element list) : ext_element list = let old_length = List.length slist and slist2 = get_reachable_symbols_once g slist in
    if old_length = List.length slist2 then slist else (get_reachable_symbols [@tailcall]) g slist2

let remove_unreachable_symbols (g : ext_grammar) : ext_grammar = g.ext_axiom @@@ (List.filter (is_rule_useful (get_reachable_symbols g [g.ext_axiom])) g.ext_rules)

let get_epsilon_symbols (g : ext_grammar) : ext_element list =
    g.ext_rules |> List.filter (fun r -> List.length r.ext_right_part = 0 && r.ext_left_symbol <> g.ext_axiom) |> List.rev_map lhs_of_ext_rule |> List.sort_uniq compare

(* returns the power set of a number. For exemple, 2 yields [[true,true],[true,false],[false,true],[false,false]] *)
(* not tail-recursive *)
let rec power_set : int -> bool list list = function
    | 0 -> [[]]
    | n -> let ps = power_set (n-1) in
        List.rev_map (List.cons false) ps @ List.rev_map (List.cons true) ps

let rec get_occurrences_number (s : ext_element) (nb: int) : ext_element list -> int = function
    | [] -> nb
    | t::q when s=t -> (get_occurrences_number [@tailcall]) s (nb + 1) q
    | _::q -> (get_occurrences_number [@tailcall]) s nb q

(* not tail-recursive *)
let rec filter_symbol (s : ext_element) (rhs : ext_element list) (blist : bool list) : ext_element list = match rhs with
    | [] -> []
    | t::q when s=t -> (match blist with
                    | f::r -> if f then (filter_symbol [@tailcall]) s q r else t::(filter_symbol s q r)
                    | _ -> failwith "Impossible")
    | t::q -> t::(filter_symbol s q blist)

(* not tail-recursive *)
let rec get_rules_with_symbol (s : ext_element) : ext_rule list -> ext_rule list = function
    | [] -> []
    | r::q when List.mem s (r.ext_right_part) -> r::(get_rules_with_symbol s q)
    | _::q -> get_rules_with_symbol s q

let duplicate_epsilon_symbol_from_rule (s : ext_element) (r : ext_rule) : ext_rule list =
    let ps = power_set (get_occurrences_number s 0 r.ext_right_part) in
    List.rev_map (fun blist -> r.ext_left_symbol ---> (filter_symbol s r.ext_right_part blist)) ps

let remove_epsilon_rules_except_ext_axiom epsilon_symbols (g : ext_grammar) : ext_rule list = List.filter (fun r -> r.ext_right_part <> [] || r.ext_left_symbol = g.ext_axiom || not (List.mem r.ext_left_symbol epsilon_symbols)) g.ext_rules

let remove_epsilon_symbols_once (g : ext_grammar) : ext_rule list =
    let remove_epsilon_one_symbol (rules: ext_rule list) (symbol: ext_element) : ext_rule list =
        get_rules_with_symbol symbol rules
        |> List.rev_map (duplicate_epsilon_symbol_from_rule symbol)
        |> List.flatten
        |> List.append rules
        |> List.sort_uniq compare in

    let epsilon_symbols = get_epsilon_symbols g in
    if epsilon_symbols = [] then
        g.ext_rules
    else begin
        let new_rules = List.fold_left remove_epsilon_one_symbol g.ext_rules epsilon_symbols in
        remove_epsilon_rules_except_ext_axiom epsilon_symbols (g.ext_axiom @@@ (List.sort_uniq compare (g.ext_rules@new_rules)))
    end

(* remove the epsilon symbols of a grammar *)
let remove_epsilon_symbols : ext_grammar -> ext_grammar = iterate_until_convergence remove_epsilon_symbols_once

let remove_trivial_rules (g: ext_grammar) : ext_grammar = (* remove the rules a -> ... and A -> A *)
    g.ext_axiom @@@ (List.filter (fun r -> not (is_ext_element_non_terminal r.ext_left_symbol) || r.ext_right_part <> [r.ext_left_symbol]
) g.ext_rules)

(* clean: remove epsilon, trivial rules, unreachable and useless symbols *)
let clean (g : ext_grammar) : ext_grammar = (remove_epsilon_symbols (remove_trivial_rules (remove_unreachable_symbols (remove_useless_symbols g))))


(* clean classical grammar *)
let clean_grammar (g: grammar) : grammar = g |> ext_grammar_of_grammar |> clean |> grammar_of_ext_grammar

let simplify (g: grammar) : grammar =
    g |> remove_duplicated_rules |> simplify_nonterminals |> merge_consecutive_terminals |> clean_grammar
