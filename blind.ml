open Base
open Quotient
open Hashtbl

type db_type = (tree_state, (rec_rule list)) t

let blackbox prefix suffix grammaire injections = is_list_in_language grammaire (List.map (fun p -> prefix @ p @ suffix) injections)


let print_tree t = print_string ((string_of_tree t)^"\n")

let get_grammar_from_tree grammaire (p,e,s) = generate_blind_grammar_both_sides p s (e@@grammaire.regles)

(* TODO *)

let get_new_rules grammaire t = grammaire.rules

(* cf construct_trees2 *)

let get_new_axioms (r : rec_rules) : tree_state list = [(List.hd r).left_symbol]

let rec update_grammars (grammars : db_type) (grammaire : rec_grammar) : tree_state list -> unit = function
    | [] -> ()
    | t::q when Hashtbl.mem grammars t -> (update_grammars [@tailcall]) grammars grammaire q
    | t::q -> let r = get_new_rules grammaire t in Hashtbl.add grammars t r; (update_grammars [@tailcall]) grammars grammaire (q@(get_new_axioms r))

let assemble_grammar (grammars : db_type) (t : tree_state) : rec_grammar = t@@@[]

let get_grammar_from_tree2 grammars grammaire t = update_grammars grammars grammaire [t]; assemble_grammar grammars t

(* renvoie les règles dont la partie droite contient l'élément cherché *)

let trouve_regles grammaire elem = List.filter (fun r -> List.mem elem r.partiedroite) grammaire.regles

let trouve_regles2 grammaire elem = List.filter (fun r -> List.mem elem r.right_part) grammaire.rules

let rec is_accessible_from_axiom grammaire s reachable =
    if List.mem s reachable then true
    else
        let rules = List.filter (fun r -> List.mem r.elementgauche reachable) grammaire.regles in
        let new_reachable = List.sort_uniq compare (List.flatten (List.map (fun r -> r.partiedroite) rules)) in
            if (List.length reachable) = (List.length new_reachable) then false
            else (is_accessible_from_axiom [@tailcall]) grammaire s new_reachable

let rec is_accessible_from_axiom2 grammaire s reachable =
    if List.mem s reachable then true
    else
        let rules = List.filter (fun r -> List.mem r.left_symbol reachable) grammaire.rules in
        let new_reachable = List.sort_uniq compare (List.flatten (List.map (fun r -> r.right_part) rules)) in
            if (List.length reachable) = (List.length new_reachable) then false
            else (is_accessible_from_axiom2 [@tailcall]) grammaire s new_reachable

let symbols_from_parents grammaire axiome = List.sort_uniq compare (List.map (fun r -> r.elementgauche) (List.filter (fun r -> List.mem axiome r.partiedroite) grammaire.regles))

let symbols_from_parents2 grammaire axiome = List.sort_uniq compare (List.map (fun r -> r.left_symbol) (List.filter (fun r -> List.mem axiome r.right_part) grammaire.rules))

let trim = function
    | Terminal(s) -> Terminal(s)
    | Nonterminal(s) -> (*print_string s; flush stdout;*) let index = String.index_opt s '.' in match index with
        | None -> Nonterminal(s)
        | Some(i) -> Nonterminal(String.sub s 0 i)

let rec distance_to_goal grammaire goal = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_accessible_from_axiom grammaire goal [s] -> nb 
    | (s,nb)::q -> (distance_to_goal [@tailcall]) grammaire goal (q@(List.map (fun e -> (e,nb+1)) (symbols_from_parents grammaire s)))

let rec distance_to_goal2 grammaire goal = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_accessible_from_axiom2 grammaire goal [s] -> nb 
    | (s,nb)::q -> (distance_to_goal2 [@tailcall]) grammaire goal (q@(List.map (fun e -> (e,nb+1)) (symbols_from_parents2 grammaire s)))

let rec is_accessible s = function
    | [] -> false
    | r::q -> List.mem s r.partiedroite || s = r.elementgauche || (is_accessible [@tailcall]) s q

let is_symbol_accessible g s = is_accessible s g.regles

let rec is_accessible2 s = function
    | [] -> false
    | r::q -> List.mem s r.right_part || s = r.left_symbol || (is_accessible2 [@tailcall]) s q

let is_symbol_accessible2 g s = is_accessible2 s g.rules

let rec get_prefix_suffix_partie (elem : 'a) (prefix : 'a list) : 'a list -> ('a list * 'a list) list = function
    | [] -> []
    | t::q when t=elem -> (List.rev prefix,q)::(get_prefix_suffix_partie elem (t::prefix) q)
    | t::q -> get_prefix_suffix_partie elem (t::prefix) q

let construct_trees (grammaire: grammaire) ((p,e,s): tree_state) : tree_state list =
(*    List.to_seq (trouve_regles grammaire e)
    |> Seq.map (fun r -> get_prefix_suffix_partie e [] r.partiedroite)
    |> Seq.map (fun (p2,s2) -> p2@p,r.elementgauche,s@s2)
    |> List.of_seq |> List.flatten *)
    List.flatten (List.map (fun r -> let l=get_prefix_suffix_partie e [] r.partiedroite in List.map (fun (p2,s2) -> p2@p,r.elementgauche,s@s2) l) (trouve_regles grammaire e))



let get_all_tokens grammaire = List.sort_uniq compare (List.concat (List.map (fun r -> List.filter is_terminal r.partiedroite) grammaire.regles))

(* let fuzzer g = derive_within_length 10 g [g.axiome] *)

let fuzzer g =
    let term = List.filter (is_symbol_accessible g) (get_all_tokens g) in
    List.map (derive_word_with_symbol g) term

let check_grammar_validity blackbox g = blackbox (fuzzer g)

let check_grammar_validity2 blackbox g = true

(* A* avec heuristique : distance à l'objectif *)

let rec insert_in_list distance tree = function
    | [] -> [(distance,tree)]
    | (d,t)::q when d > distance -> (distance,tree)::((d,t)::q)
    | t::q -> t::(insert_in_list distance tree q)

let rec insert_all_in_list grammaire interest l = function
    | [] -> l
    | (a,b,c)::q -> (insert_all_in_list [@tailcall]) grammaire interest (insert_in_list (distance_to_goal grammaire interest [trim b,0]) (a,b,c) l) q

let rec insert_all_in_list2 grammaire interest l = function
    | [] -> l
    | t::q -> (insert_all_in_list2 [@tailcall]) grammaire interest (insert_in_list (distance_to_goal2 grammaire interest [trim2 t,0]) t l) q



(* TODO: conserver les arbres/grammaires visitées pour les réutiliser *)

let rec is_prefix a l = match a with
    | [] -> true
    | t::q -> match l with
        | [] -> false
        | t2::q2 when t=t2 -> (is_prefix [@tailcall]) q q2
        | _ -> false

let is_suffix c c2 = is_prefix (List.rev c) (List.rev c2)

let is_extension (a,b,c) (a2,b2,c2) = b = b2 && (is_prefix a2 a) && (is_suffix c2 c)

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


let rec search2 blackbox interest init_grammaire grammaire step visited grammars : (int * tree_state) list -> rec_grammar option = function
    | [] -> None
    | (_,t)::q -> print_string ("Search "^(string_of_int step)^" (queue: "^(string_of_int ((List.length q) + 1))^")\n"); flush stdout;
        if (List.exists (fun (_,tree) -> tree=t) visited) then begin
            print_string "Visited\n"; (search2 [@tailcall]) blackbox interest init_grammaire grammaire (step+1) visited grammars q
        end else begin
            let g = get_grammar_from_tree2 grammars grammaire t in
            print_string (string_of_rec_grammar g);
            (*print_string "Grammaire contruite\n"; flush stdout;*)
            (*print_string ("Accessible from "^(element2string g.axiome)^": "); print_bool (is_accessible_from_axiom grammaire interest [g.axiome]); flush stdout;*)
            (*print_string ("Distance: "^(string_of_int (distance_to_goal grammaire interest [(trim g.axiome,0)])));*)
            if not (check_grammar_validity2 blackbox g) then begin (* invalid : ignore *)
                print_string "Invalid\n"; (search2 [@tailcall]) blackbox interest init_grammaire grammaire (step+1) visited grammars q
            end else if (*print_string "AA"; flush stdout;*) is_symbol_accessible2 g interest then begin (* found ! *)
                print_string "Found!\n"; Some(g)
            end else begin (* we explore in this direction *)
                print_string "Explore\n"; None
(*                (search2 [@tailcall]) blackbox interest init_grammaire grammaire (step+1) ((g,t)::visited) grammars (insert_all_in_list2 grammaire interest q (construct_trees init_grammaire t)) *)
            end
        end

let search2_api blackbox interest init_grammaire init_tokens =
    let grammaire = rec_grammar_of_grammar init_grammaire in
        search2 blackbox interest init_grammaire grammaire 0 [] (Hashtbl.create 100) (insert_all_in_list2 grammaire interest [] init_tokens)


let get_injection_tokens blackbox grammaire = List.filter (fun p -> blackbox [[p]]) (get_all_tokens grammaire)

let get_injection_leaves blackbox grammaire = List.map (fun e -> ([],e,[])) (get_injection_tokens blackbox grammaire)
