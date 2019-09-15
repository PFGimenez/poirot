open Base
open Quotient
open Hashtbl

type db_type = (tree_state, (rec_rule list)) t
type scored_tree = int * tree_state

let blackbox
    (prefix : element list)
    (suffix : element list)
    (grammaire : grammaire)
    (injections : partie list)
    : bool
    = is_list_in_language grammaire (List.map (fun p -> prefix @ p @ suffix) injections)


let print_tree t = print_string ((string_of_tree t)^"\n")

let get_grammar_from_tree grammaire (p,e,s) = generate_blind_grammar_both_sides p s (e@@grammaire.regles)

(* TODO *)

let get_new_rules (grammaire : grammaire) (t : tree_state) : rec_rule list = []

(* TODO *)

let get_new_axioms (r : rec_rules) : tree_state list = [(List.hd r).left_symbol]

let rec update_grammars (grammars : db_type) (grammaire : grammaire) : tree_state list -> unit = function
    | [] -> ()
    | t::q when Hashtbl.mem grammars t -> (update_grammars [@tailcall]) grammars grammaire q
    | t::q -> let r = get_new_rules grammaire t in Hashtbl.add grammars t r; (update_grammars [@tailcall]) grammars grammaire (q@(get_new_axioms r))

let assemble_grammar (grammars : db_type) (t : tree_state) : rec_grammar = t@@@[]

let get_grammar_from_tree2 (grammars : db_type) (grammaire : grammaire) (t : tree_state) : rec_grammar = update_grammars grammars grammaire [t]; assemble_grammar grammars t

(* renvoie les règles dont la partie droite contient l'élément cherché *)

let trouve_regles grammaire elem = List.filter (fun r -> List.mem elem r.partiedroite) grammaire.regles

let rec is_accessible_from_axiom (grammaire : grammaire) (s : element) (reachable : element list) : bool =
    if List.mem s reachable then true
    else
        let rules = List.filter (fun r -> List.mem r.elementgauche reachable) grammaire.regles in
        let new_reachable = List.sort_uniq compare (List.flatten (List.map (fun r -> r.partiedroite) rules)) in
            if (List.length reachable) = (List.length new_reachable) then false
            else (is_accessible_from_axiom [@tailcall]) grammaire s new_reachable

let symbols_from_parents (grammaire : grammaire) (axiome : element) : element list = List.sort_uniq compare (List.map (fun r -> r.elementgauche) (List.filter (fun r -> List.mem axiome r.partiedroite) grammaire.regles))

let trim = function
    | Terminal(s) -> Terminal(s)
    | Nonterminal(s) -> (*print_string s; flush stdout;*) let index = String.index_opt s '.' in match index with
        | None -> Nonterminal(s)
        | Some(i) -> Nonterminal(String.sub s 0 i)

let rec distance_to_goal (grammaire : grammaire) (goal : element) : (element * int) list -> int = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_accessible_from_axiom grammaire goal [s] -> nb 
    | (s,nb)::q -> (distance_to_goal [@tailcall]) grammaire goal (q@(List.map (fun e -> (e,nb+1)) (symbols_from_parents grammaire s)))

let rec is_accessible s = function
    | [] -> false
    | r::q -> List.mem s r.partiedroite || s = r.elementgauche || (is_accessible [@tailcall]) s q

let is_symbol_accessible g s = is_accessible s g.regles

let rec is_accessible2 (s : element) : rec_rules -> bool = function
    | [] -> false
    | r::q -> List.exists (fun t -> element_of_tree t = s) r.right_part || s = (element_of_tree r.left_symbol) || (is_accessible2 [@tailcall]) s q

let is_symbol_accessible2 (g : rec_grammar) (s : element) : bool = is_accessible2 s g.rules

let rec get_prefix_suffix_partie (elem : element) (prefix : element list) : element list -> (element list * element list) list = function
    | [] -> []
    | t::q when t=elem -> (List.rev prefix,q)::(get_prefix_suffix_partie elem (t::prefix) q)
    | t::q -> get_prefix_suffix_partie elem (t::prefix) q

let construct_trees (grammaire: grammaire) ((p,e,s): tree_state) : tree_state list =
    List.flatten (List.map (fun r -> let l=get_prefix_suffix_partie e [] r.partiedroite in List.map (fun (p2,s2) -> p2@p,r.elementgauche,s@s2) l) (trouve_regles grammaire e))

let get_all_tokens (grammaire : grammaire) : element list = List.sort_uniq compare (List.concat (List.map (fun r -> List.filter is_terminal r.partiedroite) grammaire.regles))

let fuzzer (g : grammaire) : partie list =
    let term = List.filter (is_symbol_accessible g) (get_all_tokens g) in
    List.map (derive_word_with_symbol g) term

let check_grammar_validity (blackbox : partie list -> bool) (g : grammaire) : bool = blackbox (fuzzer g)

let check_grammar_validity2 (blackbox : partie list -> bool) (g : rec_grammar) = true

(* A* algorithm *)

let rec insert_in_list (distance : int) (tree : tree_state) : scored_tree list -> scored_tree list = function
    | [] -> [(distance,tree)]
    | (d,t)::q when d > distance -> (distance,tree)::((d,t)::q)
    | t::q -> t::(insert_in_list distance tree q)

let rec insert_all_in_list grammaire interest l = function
    | [] -> l
    | (a,b,c)::q -> (insert_all_in_list [@tailcall]) grammaire interest (insert_in_list (distance_to_goal grammaire interest [trim b,0]) (a,b,c) l) q

let rec insert_all_in_list2 (grammaire : grammaire) (interest : element) (l : scored_tree list) : tree_state list -> scored_tree list = function
    | [] -> l
    | t::q -> (insert_all_in_list2 [@tailcall]) grammaire interest (insert_in_list (distance_to_goal grammaire interest [element_of_tree t,0]) t l) q

let grammar_of_longest_tree (g,(a,b,c)) (g2,(a2,b2,c2)) = if (List.length a + List.length c) > (List.length a2 + List.length c2) then g,(a,b,c) else g2,(a2,b2,c2)

let rec search blackbox interest grammaire step visited = function
    | [] -> None
    | (_,t)::q -> print_string ("Search "^(string_of_int step)^" (queue: "^(string_of_int ((List.length q) + 1))^")\n"); print_tree t; flush stdout;
        if (List.exists (fun (_,tree) -> tree=t) visited) then begin
            print_string "Visited\n"; (search [@tailcall]) blackbox interest grammaire (step+1) visited q
        end else begin
            let g = get_grammar_from_tree grammaire t in
            print_grammar g;
            (*print_string "Grammaire contruite\n"; flush stdout;*)
            (*print_string ("Accessible from "^(element2string g.axiome)^": "); print_bool (is_accessible_from_axiom grammaire interest [g.axiome]); flush stdout;*)
            (*print_string ("Distance: "^(string_of_int (distance_to_goal grammaire interest [(trim g.axiome,0)])));*)
            if not (check_grammar_validity blackbox g) then begin (* invalid : ignore *)
                print_string "Invalid\n"; (search [@tailcall]) blackbox interest grammaire (step+1) visited q
            end else if (*print_string "AA"; flush stdout;*) is_symbol_accessible g interest then begin (* found ! *)
                print_string "Found!\n"; Some(g)
            end else begin (* we explore in this direction *)
                print_string "Explore\n";
                (search [@tailcall]) blackbox interest grammaire (step+1) ((g,t)::visited) (insert_all_in_list grammaire interest q (construct_trees grammaire t))
            end
        end

let search_api blackbox interest grammaire init_tokens =
    search blackbox interest grammaire 0 [] (insert_all_in_list grammaire interest [] init_tokens)


let rec search2
    (blackbox : partie list -> bool)
    (interest : element)
    (init_grammaire : grammaire)
    (step : int)
    (visited : tree_state list)
    (grammars : db_type)
    : scored_tree list -> rec_grammar option = function
    | [] -> None
    | (_,t)::q -> print_string ("Search "^(string_of_int step)^" (queue: "^(string_of_int ((List.length q) + 1))^")\n"); flush stdout;
        if (List.exists (fun tree -> tree=t) visited) then begin
            print_string "Visited\n"; (search2 [@tailcall]) blackbox interest init_grammaire (step+1) visited grammars q
        end else begin
            let g = get_grammar_from_tree2 grammars init_grammaire t in
            print_string (string_of_rec_grammar g);
            (*print_string "Grammaire contruite\n"; flush stdout;*)
            (*print_string ("Accessible from "^(element2string g.axiome)^": "); print_bool (is_accessible_from_axiom init_grammaire interest [g.axiome]); flush stdout;*)
            (*print_string ("Distance: "^(string_of_int (distance_to_goal init_grammaire interest [(trim g.axiome,0)])));*)
            if not (check_grammar_validity2 blackbox g) then begin (* invalid : ignore *)
                print_string "Invalid\n"; (search2 [@tailcall]) blackbox interest init_grammaire (step+1) visited grammars q
            end else if (*print_string "AA"; flush stdout;*) is_symbol_accessible2 g interest then begin (* found ! *)
                print_string "Found!\n"; Some(g)
            end else begin (* we explore in this direction *)
                print_string "Explore\n";
                (search2 [@tailcall]) blackbox interest init_grammaire (step+1) (t::visited) grammars (insert_all_in_list2 init_grammaire interest q (construct_trees init_grammaire t))
            end
        end

let search2_api blackbox interest init_grammaire init_tokens =
        search2 blackbox interest init_grammaire 0 [] (Hashtbl.create 100) (insert_all_in_list2 init_grammaire interest [] init_tokens)


let get_injection_tokens blackbox grammaire = List.filter (fun p -> blackbox [[p]]) (get_all_tokens grammaire)

let get_injection_leaves blackbox grammaire = List.map (fun e -> ([],e,[])) (get_injection_tokens blackbox grammaire)
