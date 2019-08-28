open Base
open Quotient

let blackbox prefix suffix grammaire injections = isInLanguageListe grammaire (List.map (fun p -> prefix @ p @ suffix) injections)

(* type arbre_deriv = Leaf of element | Node of element * partie * arbre_deriv * partie *)

type tree_state = partie * element * partie


let tree_root_element (a,b,c) = b

(* let tree_root_element = function
    | Leaf(e) -> e
    | Node(e,_,_,_) -> e *)

let get_prefix_suffix_tree (a,b,c) = a,c

(* let rec get_prefix_suffix_tree = function
    | Leaf(t) -> [],[]
    | Node(nt,p,c,s) -> let (pre,suf) = get_prefix_suffix_tree(c) in
            p @ pre, suf @ s *)

let get_grammar_from_tree grammaire tree =
    let p,s=get_prefix_suffix_tree(tree) in
    genererGrammaireInjectionAveugle p s ((tree_root_element tree)@@grammaire.regles)

let get_grammar_from_tree_list grammaire tl =
    List.map (get_grammar_from_tree grammaire) tl


(* renvoie les règles dont la partie droite contient l'élément cherché *)

let trouve_regles grammaire elem = List.filter (fun r -> List.mem elem r.partiedroite) grammaire.regles

let trouve_parents r = List.sort_uniq compare (List.map (fun r -> List.hd r.partiegauche) r)

let rec is_accessible s = function
    | [] -> false
    | r::q -> List.mem s r.partiedroite || List.mem s r.partiegauche || is_accessible s q

let is_symbol_accessible s g = is_accessible s g.regles

let rec get_prefix_suffix_partie elem = function
    | [] -> failwith "Element absent de la règle"
    | t::q when t=elem -> [],q
    | t::q -> let p,s=get_prefix_suffix_partie elem q in t::p,s

let rec construct_trees grammaire (p,e,s) =
    let regles = trouve_regles grammaire e in
    List.map (fun r -> let p2,s2=get_prefix_suffix_partie e r.partiedroite in (p2@p,List.hd r.partiegauche,s@s2)) regles

let construct_trees_from_list grammaire tl =
    List.concat (List.map (construct_trees grammaire) tl)

(* let rec print_tree2 prefix = function
    | Leaf(e) -> print_string (prefix^(element2string e)^"\n")
    | Node(e,p,c,s) -> print_string (prefix^(partie2string p)^"\n"); (*print_string ("["^(element2string e)^"]");*) print_tree2 (" "^prefix) c; print_string (prefix^(partie2string s)^"\n")

let print_tree = print_tree2 ""

let rec print_tree_list = function
    | [] -> ()
    | t::q -> print_string "Arbre:\n"; print_tree t; print_tree_list q *)

let print_tree (a,b,c) =
    print_string ((partie2string a)^" ["^(element2string b)^"] "^(partie2string c)^"\n")

let fuzzer g = deriverLongueur 5 g [g.axiome]

let check_grammar_validity blackbox g = blackbox (fuzzer g)

(* Recherche BFS. Sauvegarder arbre déjà visités. Version améliorée : A* avec heuristique (distance à l'objectif) *)

let rec search blackbox interest grammaire visited = function
    | [] -> None
    | t::q -> print_int (List.length q); print_string " search \n"; print_tree t; flush stdout; let g = get_grammar_from_tree grammaire t in
        (*print_string "Grammaire contruite\n"; flush stdout;*)
        if (List.mem t visited) || not (check_grammar_validity blackbox g) then begin (* invalid : ignore *)
            print_string "Nope "; print_bool (List.mem t visited); search blackbox interest grammaire visited q
        end else if (*print_string "AA"; flush stdout;*) is_symbol_accessible interest g then begin (* found ! *)
            print_string "Found!\n"; Some(g)
        end else begin (* we explore in this direction *)
            print_string "Explore\n";
            search blackbox interest grammaire (t::visited) (q@(construct_trees grammaire t))
        end

let min_list a b = if List.length a < List.length b then a else b

let getInjection e g = 
    let all = List.filter (fun p -> List.mem e p) (deriverLongueur 20 g [g.axiome]) in match all with
    | [] -> []
    | t::q -> List.fold_left min_list t q

(* let rec afficherGrammaireTreesCombined e = function
    | [] -> ()
    | (g,t)::q -> print_string "\n"; afficherGrammaire g; print_tree t; printWords (List.filter (fun p -> List.mem e p) (deriverLongueur 10 g [g.axiome])); afficherGrammaireTreesCombined  e q


let afficherGrammaireTrees e g t =
    afficherGrammaireTreesCombined e (List.combine g t) *)

let get_all_tokens grammaire = List.sort_uniq compare (List.concat (List.map (fun r -> List.filter isTerminal r.partiedroite) grammaire.regles))

let get_injection_tokens blackbox grammaire =
    List.filter (fun p -> blackbox [[p]]) (get_all_tokens grammaire)

let get_injection_leaves blackbox grammaire = List.map (fun e -> ([],e,[])) (get_injection_tokens blackbox grammaire)
