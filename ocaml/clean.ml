open Base

(* TODO : nettoyage:
    - retirer règles avec symbole inutiles (non-terminaux pas à gauche d'une production)
    - retirer les symboles inaccessibles depuis l'axiom
    - pas nécessaire : retirer epsilon-rule:
        - laisser axiom -> ε si ça existe
        - trouver les symboles annulables. Un symbole A est annulable si:
            - il y a une règle A -> ε
            - il y a une règle A -> α, avec α composé de symboles annulables
        - créer des toutes les règles où un ou des symboles annulables sont retirés
*)

let rec iterate_until_convergence (f : ext_grammar -> ext_rule list) (g : ext_grammar) : ext_grammar = let old_length = List.length g.ext_rules and new_rules = f g in
    if old_length = List.length new_rules then g else (iterate_until_convergence [@tailcall]) f (g.ext_axiom@@@new_rules)

(* The useful symbols are the non-terminal that are the left side of a production *)
let get_useful_symbols (g : ext_grammar) : ext_element list = List.sort_uniq compare (List.map (fun r -> r.ext_left_symbol) g.ext_rules)

let rec is_rule_useful (useful_s : ext_element list) (t : ext_rule) : bool =
    List.mem t.ext_left_symbol useful_s && List.for_all (fun s -> is_ext_element_terminal s || List.mem s useful_s) t.ext_right_part

let remove_useless_symbols_once (g : ext_grammar) : ext_rule list = List.filter (is_rule_useful (get_useful_symbols g)) g.ext_rules

let remove_useless_symbols : ext_grammar -> ext_grammar = iterate_until_convergence remove_useless_symbols_once

let get_reachable_symbols_once (g : ext_grammar) (slist : ext_element list) : ext_element list = List.sort_uniq compare (List.concat (slist::(List.map (fun r -> r.ext_right_part) (List.filter (fun r -> List.mem r.ext_left_symbol slist) g.ext_rules))))

let rec get_reachable_symbols (g : ext_grammar) (slist : ext_element list) : ext_element list = let old_length = List.length slist and slist2 = get_reachable_symbols_once g slist in
    if old_length = List.length slist2 then slist else (get_reachable_symbols [@tailcall]) g slist2

let remove_unreachable_symbols (g : ext_grammar) : ext_grammar = g.ext_axiom @@@ (List.filter (is_rule_useful (get_reachable_symbols g [g.ext_axiom])) g.ext_rules)

let get_epsilon_symbols (g : ext_grammar) : ext_element list = List.sort_uniq compare (List.map (fun r -> r.ext_left_symbol) (List.filter (fun r -> List.length r.ext_right_part = 0 && r.ext_left_symbol <> g.ext_axiom) g.ext_rules))

let rec power_set : int -> bool list list = function
    | 0 -> [[]]
    | n -> let ps = power_set (n-1) in
        List.map (fun l -> false :: l) ps @ List.map (fun l -> true :: l) ps

let rec get_occurrences_number (s : ext_element) : ext_element list -> int = function
    | [] -> 0
    | t::q when s=t -> 1 + (get_occurrences_number s q)
    | t::q -> get_occurrences_number s q

let rec filter_symbol (s : ext_element) (blist : bool list) : ext_element list -> ext_element list  = function
    | [] -> []
    | t::q when s=t -> (match blist with
                    | f::r -> if f then filter_symbol s r q else t::(filter_symbol s r q)
                    | _ -> failwith "Impossible")
    | t::q -> t::(filter_symbol s blist q)

let rec get_rules_with_symbol (s : ext_element) : ext_rule list -> ext_rule list = function
    | [] -> []
    | r::q when List.mem s (r.ext_right_part) -> r::(get_rules_with_symbol s q)
    | r::q -> get_rules_with_symbol s q

let duplicate_epsilon_symbol_from_rule (s : ext_element) (r : ext_rule) : ext_rule list = let ps = power_set (get_occurrences_number s r.ext_right_part) in
    List.map (fun blist -> r.ext_left_symbol ---> (filter_symbol s blist r.ext_right_part)) ps

let remove_epsilon_rules_except_ext_axiom epsilon_symbols (g : ext_grammar) : ext_rule list = List.filter (fun r -> List.length r.ext_right_part != 0 || (r.ext_left_symbol = g.ext_axiom || not (List.mem r.ext_left_symbol epsilon_symbols))) g.ext_rules

let remove_epsilon_symbols_once (g : ext_grammar) : ext_rule list =
    let epsilon_symbols = get_epsilon_symbols g in
    let new_rules = List.flatten (List.flatten (List.map (fun s -> List.map (duplicate_epsilon_symbol_from_rule s) (get_rules_with_symbol s g.ext_rules)) epsilon_symbols)) in
    remove_epsilon_rules_except_ext_axiom epsilon_symbols (g.ext_axiom @@@ (List.sort_uniq compare (g.ext_rules@new_rules)))

let remove_epsilon_symbols : ext_grammar -> ext_grammar = iterate_until_convergence remove_epsilon_symbols_once

(* Epsilon-removing not used *)
let clean_once (g : ext_grammar) : ext_rule list = ((* remove_epsilon_symbols *) (remove_unreachable_symbols (remove_useless_symbols g))).ext_rules

let clean : ext_grammar -> ext_grammar = iterate_until_convergence clean_once
