open Base

(* TODO : nettoyage:
    - retirer règles avec symbole inutiles (non-terminaux pas à gauche d'une production)
    - retirer les symboles inaccessibles depuis l'axiome
    - pas nécessaire : retirer epsilon-rule:
        - laisser axiome -> ε si ça existe
        - trouver les symboles annulables. Un symbole A est annulable si:
            - il y a une règle A -> ε
            - il y a une règle A -> α, avec α composé de symboles annulables
        - créer des toutes les règles où un ou des symboles annulables sont retirés
*)

let rec iterate_until_convergence f g = let old_length = List.length g.rules and new_rules = f g in
    if old_length = List.length new_rules then g else (iterate_until_convergence [@tailcall]) f (g.axiom@@@new_rules)

(* The useful symbols are the non-terminal that are the left side of a production *)
let get_useful_symbols g = List.sort_uniq compare (List.map (fun r -> r.left_symbol) g.rules)

let rec is_rule_useful useful_s t =
    List.mem t.left_symbol useful_s && List.for_all (fun s -> is_tree_terminal s || List.mem s useful_s) t.right_part

let remove_useless_symbols_once g = List.filter (is_rule_useful (get_useful_symbols g)) g.rules

let remove_useless_symbols = iterate_until_convergence remove_useless_symbols_once

let get_reachable_symbols_once g slist = List.sort_uniq compare (List.concat (slist::(List.map (fun r -> r.right_part) (List.filter (fun r -> List.mem r.left_symbol slist) g.rules))))

let rec get_reachable_symbols g slist = let old_length = List.length slist and slist2 = get_reachable_symbols_once g slist in
    if old_length = List.length slist2 then slist else (get_reachable_symbols [@tailcall]) g slist2

let remove_unreachable_symbols g = g.axiom @@@ (List.filter (is_rule_useful (get_reachable_symbols g [g.axiom])) g.rules)

let get_epsilon_symbols g = List.sort_uniq compare (List.map (fun r -> r.left_symbol) (List.filter (fun r -> List.length r.right_part = 0 && r.left_symbol <> g.axiom) g.rules))

let rec power_set = function
    | 0 -> [[]]
    | n -> let ps = power_set (n-1) in
        List.map (fun l -> false :: l) ps @ List.map (fun l -> true :: l) ps

let power_set_except_one n = List.filter (fun blist -> List.exists (fun a -> a) blist) (power_set n)

let rec get_occurrences_number s = function
    | [] -> 0
    | t::q when s=t -> 1 + (get_occurrences_number s q)
    | t::q -> get_occurrences_number s q

let rec filter_symbol s blist = function
    | [] -> []
    | t::q when s=t -> (match blist with
                    | f::r -> if f then filter_symbol s r q else t::(filter_symbol s r q)
                    | _ -> failwith "Impossible")
    | t::q -> t::(filter_symbol s blist q)

let rec get_rules_with_symbol s = function
    | [] -> []
    | r::q when List.mem s (r.right_part) -> r::(get_rules_with_symbol s q)
    | r::q -> get_rules_with_symbol s q

let duplicate_epsilon_symbol_from_rule s r = let ps = power_set_except_one (get_occurrences_number s r.right_part) in
    List.map (fun blist -> r.left_symbol ---> (filter_symbol s blist r.right_part)) ps

let remove_epsilon_rules_except_axiom epsilon_symbols g = List.filter (fun r -> List.length r.right_part != 0 || (r.left_symbol = g.axiom || not (List.mem r.left_symbol epsilon_symbols))) g.rules

(* TODO: retirer des règles initiales dont on a retiré les duplicatas *)
let remove_epsilon_symbols_once g =
    let epsilon_symbols = get_epsilon_symbols g in
    let new_rules = List.flatten (List.flatten (List.map (fun s -> List.map (duplicate_epsilon_symbol_from_rule s) (get_rules_with_symbol s g.rules)) epsilon_symbols)) in
    remove_epsilon_rules_except_axiom epsilon_symbols (g.axiom @@@ (List.sort_uniq compare (g.rules@new_rules)))

let remove_epsilon_symbols = iterate_until_convergence remove_epsilon_symbols_once

(* Epsilon-removing not used *)
let clean g = remove_unreachable_symbols (remove_useless_symbols g)
