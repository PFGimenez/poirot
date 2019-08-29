open Base
open Quotient

let blackbox prefix suffix grammaire injections = isInLanguageListe grammaire (List.map (fun p -> prefix @ p @ suffix) injections)

type tree_state = partie * element * partie

let print_tree (a,b,c) = print_string ((partie2string a)^" ["^(element2string b)^"] "^(partie2string c)^"\n")

let get_grammar_from_tree grammaire (p,e,s) = genererGrammaireInjectionAveugle p s (e@@grammaire.regles)

(* renvoie les règles dont la partie droite contient l'élément cherché *)

let trouve_regles grammaire elem = List.filter (fun r -> List.mem elem r.partiedroite) grammaire.regles

let rec is_accessible s = function
    | [] -> false
    | r::q -> List.mem s r.partiedroite || List.mem s r.partiegauche || (is_accessible [@tailcall]) s q

let is_symbol_accessible g s = is_accessible s g.regles

let rec get_prefix_suffix_partie elem = function
    | [] -> failwith "Element absent de la règle"
    | t::q when t=elem -> [],q
    | t::q -> let p,s=get_prefix_suffix_partie elem q in t::p,s

let construct_trees grammaire (p,e,s) =
    List.map (fun r -> let p2,s2=get_prefix_suffix_partie e r.partiedroite in (p2@p,List.hd r.partiegauche,s@s2)) (trouve_regles grammaire e)

let get_all_tokens grammaire = List.sort_uniq compare (List.concat (List.map (fun r -> List.filter isTerminal r.partiedroite) grammaire.regles))

(* let fuzzer g = deriverLongueur 10 g [g.axiome] *)

let fuzzer g =
    let term = List.filter (is_symbol_accessible g) (get_all_tokens g) in
    List.map (derive_word_with_symbol g) term

let check_grammar_validity blackbox g = blackbox (fuzzer g)

(* Recherche BFS. Version améliorée : A* avec heuristique (distance à l'objectif) *)

let rec search blackbox interest grammaire visited = function
    | [] -> None
    | t::q -> print_int (List.length q); print_string " search \n"; print_tree t; flush stdout; let g = get_grammar_from_tree grammaire t in
        (*print_string "Grammaire contruite\n"; flush stdout;*)
        if (List.mem t visited) || not (check_grammar_validity blackbox g) then begin (* invalid : ignore *)
            print_string "Nope "; print_bool (List.mem t visited); (search [@tailcall]) blackbox interest grammaire visited q
        end else if (*print_string "AA"; flush stdout;*) is_symbol_accessible g interest then begin (* found ! *)
            print_string "Found!\n"; Some(g)
        end else begin (* we explore in this direction *)
            print_string "Explore\n";
            (search [@tailcall]) blackbox interest grammaire (t::visited) (q@(construct_trees grammaire t))
        end

let getInjection e g = 
    let all = List.filter (fun p -> List.mem e p) (deriver 10 g) in match all with
    | [] -> []
    | t::q -> List.fold_left min_list t q


let get_injection_tokens blackbox grammaire = List.filter (fun p -> blackbox [[p]]) (get_all_tokens grammaire)

let get_injection_leaves blackbox grammaire = List.map (fun e -> ([],e,[])) (get_injection_tokens blackbox grammaire)
