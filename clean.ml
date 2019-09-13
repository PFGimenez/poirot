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

let remove_useless_symbols g = iterate_until_convergence remove_useless_symbols_once g

let get_reachable_symbols_once g slist = List.sort_uniq compare (List.concat (slist::(List.map (fun r -> r.right_part) (List.filter (fun r -> List.mem r.left_symbol slist) g.rules))))

let rec get_reachable_symbols g slist = let old_length = List.length slist and slist2 = get_reachable_symbols_once g slist in
    if old_length = List.length slist2 then slist else (get_reachable_symbols [@tailcall]) g slist2

let remove_unreachable_symbols g = g.axiom @@@ (List.filter (is_rule_useful (get_reachable_symbols g [g.axiom])) g.rules)

let clean g = remove_unreachable_symbols (remove_useless_symbols g)
