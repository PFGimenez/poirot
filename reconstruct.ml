open Base
open Quotient

(* TODO: vérifier si une phrase est acceptée par une grammaire *)
(* TODO: dériver une phrase qui contiennet un symbole (terminal ou non) de notre choix *)

type arbre_deriv = Leaf of element | Node of element * partie * arbre_deriv * partie

let tree_root_element = function
    | Leaf(e) -> e
    | Node(e,_,_,_) -> e

let rec get_prefix_suffix_tree = function
    | Leaf(t) -> [],[]
    | Node(nt,p,c,s) -> let (pre,suf) = get_prefix_suffix_tree(c) in
            List.append p pre, List.append suf s

let get_grammar_from_tree grammaire tree =
    let p,s=get_prefix_suffix_tree(tree) in
    genererGrammaireInjectionAveugle p s ((tree_root_element tree)@grammaire.regles)

let get_grammar_from_tree_list grammaire tl =
    List.map (get_grammar_from_tree grammaire) tl


(* renvoie les règles dont la partie droite contient l'élément cherché *)

let trouve_regles grammaire elem = List.filter (fun r -> List.mem elem r.partiedroite) grammaire.regles

let trouve_parents r = List.sort_uniq compare (List.map (fun r -> List.hd r.partiegauche) r)

let rec is_accessible s = function
    | [] -> false
    | r::q -> List.mem s r.partiedroite || List.mem s r.partiegauche || is_accessible s q

let rec is_symbol_accessible s g = is_accessible s g.regles

let rec get_prefix_suffix_partie elem = function
    | [] -> failwith "Element absent de la règle"
    | t::q when t=elem -> [],q
    | t::q -> let p,s=get_prefix_suffix_partie elem q in t::p,s

let rec construct_trees_elem grammaire elem child =
    let regles = trouve_regles grammaire elem in
    List.map (fun r -> let p,s=get_prefix_suffix_partie elem r.partiedroite in Node(List.hd r.partiegauche,p,child,s)) regles

let construct_trees grammaire t = match t with
    | Leaf(e) -> construct_trees_elem grammaire e t
    | Node(e,_,_,_) -> construct_trees_elem grammaire e t

let construct_trees_from_list grammaire tl =
    List.concat (List.map (construct_trees grammaire) tl)

let rec print_tree2 prefix = function
    | Leaf(e) -> print_string (prefix^(element2string e)^"\n")
    | Node(e,p,c,s) -> print_string (prefix^(partie2string p)^"\n"); (*print_string ("["^(element2string e)^"]");*) print_tree2 (" "^prefix) c; print_string (prefix^(partie2string s)^"\n")

let print_tree = print_tree2 ""

let rec print_tree_list = function
    | [] -> ()
    | t::q -> print_string "Arbre:\n"; print_tree t; print_tree_list q

let check_grammar_validity prefix suffix grammaire (g,t) =
    let injections = deriverLongueur 10 g [g.axiome] in
    let words = List.map (fun p -> List.append (List.append prefix p) suffix) injections in
    List.for_all (isInLanguage grammaire) words

let rec find_grammar prefix suffix s g trees =
    let gt = List.combine (List.map (get_grammar_from_tree g) trees) trees in
    let valid_trees = List.filter (check_grammar_validity prefix suffix g) gt in
    print_string "Total: "; print_int (List.length gt); print_string "\nValides: "; print_int (List.length valid_trees); print_string "\n";
    let good = List.filter (fun (g,t) -> is_symbol_accessible s g) gt in
    match good with
    | [] -> find_grammar prefix suffix s g (construct_trees_from_list g (snd (List.split valid_trees)))
    | l -> good


let rec afficherGrammaireTreesCombined e = function
    | [] -> ()
    | (g,t)::q -> afficherGrammaire g; print_tree t; printWords (List.filter (fun p -> List.mem e p) (deriverLongueur 10 g [g.axiome])); afficherGrammaireTreesCombined e q

let afficherGrammaireTrees e g t =
    afficherGrammaireTreesCombined e (List.combine g t)
