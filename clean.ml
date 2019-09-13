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

(* The useful symbols are the non-terminal that are the left side of a production *)
let get_useful_symbols g = List.sort_uniq compare (List.map (fun r -> r.left_symbol) g.rules)

let rec is_rule_useful useful_s t =
    List.for_all (fun s -> is_tree_terminal s || List.mem s useful_s) t.right_part

let remove_useless_symbols_once g = List.filter (is_rule_useful (get_useful_symbols g)) g.rules

let rec remove_useless_symbols g = let old_length = List.length g.rules and new_rules = remove_useless_symbols_once g in
    if old_length = List.length new_rules then g else (remove_useless_symbols [@tailcall]) (g.axiom@@@new_rules)

let clean g = remove_useless_symbols g
