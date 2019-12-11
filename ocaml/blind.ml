(* TODO: pour la génération d'injection, ne générer que des injections qui ne sont pas des formes syntactiques valides (i.e. qui ne sont pas dérivables d'un seul non-terminal *)

open Base

type db_type = (ext_element, (ext_rule list)) Hashtbl.t
type scored_ext_element = int * ext_element

(* TODO: blackbox linked to interface *)

let blackbox_template
    (prefix : element list)
    (suffix : element list)
    (grammar : grammar)
    (injections : partie list)
    : bool
    = Fuzzer.is_list_in_language grammar (List.map (fun p -> prefix @ p @ suffix) injections)


let print_ext_element t = print_string ((string_of_ext_element t)^"\n")

let get_grammar_from_ext_element grammar (p,e,s) = Quotient.generate_blind_grammar_both_sides p s (e@@grammar.regles)

(* TODO *)

let get_new_rules (grammar : grammar) (t : ext_element) : ext_rule list = []

(* TODO *)

let get_new_axioms (r : ext_rules) : ext_element list = [(List.hd r).ext_left_symbol]

let rec update_grammars (grammars : db_type) (grammar : grammar) : ext_element list -> unit = function
    | [] -> ()
    | t::q when Hashtbl.mem grammars t -> (update_grammars [@tailcall]) grammars grammar q
    | t::q -> let r = get_new_rules grammar t in Hashtbl.add grammars t r; (update_grammars [@tailcall]) grammars grammar (q@(get_new_axioms r))

let assemble_grammar (grammars : db_type) (t : ext_element) : ext_grammar = t@@@[]

let get_grammar_from_ext_element2 (grammars : db_type) (grammar : grammar) (t : ext_element) : ext_grammar = update_grammars grammars grammar [t]; assemble_grammar grammars t

(* renvoie les règles dont la partie droite contient l'élément cherché *)

let trouve_regles grammar elem = List.filter (fun r -> List.mem elem r.right_part) grammar.regles

let rec is_accessible_from_axiom (grammar : grammar) (s : element) (reachable : element list) : bool =
    if List.mem s reachable then true
    else
        let rules = List.filter (fun r -> List.mem r.left_symbol reachable) grammar.regles in
        let new_reachable = List.sort_uniq compare (List.flatten (List.map (fun r -> r.right_part) rules)) in
            if (List.length reachable) = (List.length new_reachable) then false
            else (is_accessible_from_axiom [@tailcall]) grammar s new_reachable

let symbols_from_parents (grammar : grammar) (axiome : element) : element list = List.sort_uniq compare (List.map (fun r -> r.left_symbol) (List.filter (fun r -> List.mem axiome r.right_part) grammar.regles))

let trim = function
    | Terminal(s) -> Terminal(s)
    | Nonterminal(s) -> (*print_string s; flush stdout;*) let index = String.index_opt s '.' in match index with
        | None -> Nonterminal(s)
        | Some(i) -> Nonterminal(String.sub s 0 i)

let rec distance_to_goal (grammar : grammar) (goal : element) : (element * int) list -> int = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_accessible_from_axiom grammar goal [s] -> nb 
    | (s,nb)::q -> (distance_to_goal [@tailcall]) grammar goal (q@(List.map (fun e -> (e,nb+1)) (symbols_from_parents grammar s)))

let rec is_accessible s = function
    | [] -> false
    | r::q -> List.mem s r.right_part || s = r.left_symbol || (is_accessible [@tailcall]) s q

let is_symbol_accessible g s = is_accessible s g.regles

let rec is_accessible2 (s : element) : ext_rules -> bool = function
    | [] -> false
    | r::q -> List.exists (fun t -> element_of_ext_element t = s) r.ext_right_part || s = (element_of_ext_element r.ext_left_symbol) || (is_accessible2 [@tailcall]) s q

let is_symbol_accessible2 (g : ext_grammar) (s : element) : bool = is_accessible2 s g.rules

let rec get_prefix_suffix_partie (elem : element) (prefix : element list) : element list -> (element list * element list) list = function
    | [] -> []
    | t::q when t=elem -> (List.rev prefix,q)::(get_prefix_suffix_partie elem (t::prefix) q)
    | t::q -> get_prefix_suffix_partie elem (t::prefix) q

let construct_ext_elements (grammar: grammar) ((p,e,s): ext_element) : ext_element list =
    List.flatten (List.map (fun r -> let l=get_prefix_suffix_partie e [] r.right_part in List.map (fun (p2,s2) -> p2@p,r.left_symbol,s@s2) l) (trouve_regles grammar e))

let get_all_tokens (grammar : grammar) : element list = List.sort_uniq compare (List.concat (List.map (fun r -> List.filter is_terminal r.right_part) grammar.regles))

let fuzzer (g : grammar) : partie list =
    let term = List.filter (is_symbol_accessible g) (get_all_tokens g) in
    List.map (Fuzzer.derive_word_with_symbol g) term

let check_grammar_validity (blackbox : partie list -> bool) (g : grammar) : bool = blackbox (fuzzer g)

let check_grammar_validity2 (blackbox : partie list -> bool) (g : ext_grammar) = true

(* A* algorithm *)

let rec insert_in_list (distance : int) (ext_element : ext_element) : scored_ext_element list -> scored_ext_element list = function
    | [] -> [(distance,ext_element)]
    | ((d,t) as h)::q when d > distance -> (distance,ext_element)::(h::q)
    | t::q -> t::(insert_in_list distance ext_element q)

let rec insert_all_in_list grammar interest l = function
    | [] -> l
    | ((a,b,c) as t)::q -> (insert_all_in_list [@tailcall]) grammar interest (insert_in_list (distance_to_goal grammar interest [trim b,0]) t l) q

let rec insert_all_in_list2 (grammar : grammar) (interest : element) (l : scored_ext_element list) : ext_element list -> scored_ext_element list = function
    | [] -> l
    | t::q -> (insert_all_in_list2 [@tailcall]) grammar interest (insert_in_list (distance_to_goal grammar interest [element_of_ext_element t,0]) t l) q

let grammar_of_longest_ext_element (g,(a,b,c)) (g2,(a2,b2,c2)) = if (List.length a + List.length c) > (List.length a2 + List.length c2) then g,(a,b,c) else g2,(a2,b2,c2)

let rec search blackbox interest grammar step visited = function
    | [] -> None
    | (_,t)::q -> print_string ("Search "^(string_of_int step)^" (queue: "^(string_of_int ((List.length q) + 1))^")\n"); print_ext_element t; flush stdout;
        if (List.exists (fun (_,ext_element) -> ext_element=t) visited) then begin
            print_string "Visited\n"; (search [@tailcall]) blackbox interest grammar (step+1) visited q
        end else begin
            let g = get_grammar_from_ext_element grammar t in
            print_grammar g;
            (*print_string "grammar contruite\n"; flush stdout;*)
            (*print_string ("Accessible from "^(element2string g.axiome)^": "); print_bool (is_accessible_from_axiom grammar interest [g.axiome]); flush stdout;*)
            (*print_string ("Distance: "^(string_of_int (distance_to_goal grammar interest [(trim g.axiome,0)])));*)
            if not (check_grammar_validity blackbox g) then begin (* invalid : ignore *)
                print_string "Invalid\n"; (search [@tailcall]) blackbox interest grammar (step+1) visited q
            end else if (*print_string "AA"; flush stdout;*) is_symbol_accessible g interest then begin (* found ! *)
                print_string "Found!\n"; Some(g)
            end else begin (* we explore in this direction *)
                print_string "Explore\n";
                (search [@tailcall]) blackbox interest grammar (step+1) ((g,t)::visited) (insert_all_in_list grammar interest q (construct_ext_elements grammar t))
            end
        end

let search_api blackbox interest grammar init_tokens =
    search blackbox interest grammar 0 [] (insert_all_in_list grammar interest [] init_tokens)


let rec search2
    (blackbox : partie list -> bool)
    (interest : element)
    (init_grammaire : grammar)
    (step : int)
    (visited : ext_element list)
    (grammars : db_type)
    : scored_ext_element list -> ext_grammar option = function
    | [] -> None
    | (_,t)::q -> print_string ("Search "^(string_of_int step)^" (queue: "^(string_of_int ((List.length q) + 1))^")\n"); flush stdout;
        if (List.exists (fun ext_element -> ext_element=t) visited) then begin
            print_string "Visited\n"; (search2 [@tailcall]) blackbox interest init_grammaire (step+1) visited grammars q
        end else begin
            let g = get_grammar_from_ext_element2 grammars init_grammaire t in
            print_string (string_of_ext_grammar g);
            (*print_string "grammar contruite\n"; flush stdout;*)
            (*print_string ("Accessible from "^(element2string g.axiome)^": "); print_bool (is_accessible_from_axiom init_grammaire interest [g.axiome]); flush stdout;*)
            (*print_string ("Distance: "^(string_of_int (distance_to_goal init_grammaire interest [(trim g.axiome,0)])));*)
            if not (check_grammar_validity2 blackbox g) then begin (* invalid : ignore *)
                print_string "Invalid\n"; (search2 [@tailcall]) blackbox interest init_grammaire (step+1) visited grammars q
            end else if (*print_string "AA"; flush stdout;*) is_symbol_accessible2 g interest then begin (* found ! *)
                print_string "Found!\n"; Some(g)
            end else begin (* we explore in this direction *)
                print_string "Explore\n";
                (search2 [@tailcall]) blackbox interest init_grammaire (step+1) (t::visited) grammars (insert_all_in_list2 init_grammaire interest q (construct_ext_elements init_grammaire t))
            end
        end

let search2_api
    (blackbox : partie list -> bool)
    (interest : element)
    (init_grammaire : grammar)
    (init_tokens : ext_element list)
    : ext_grammar option =
        search2 blackbox interest init_grammaire 0 [] (Hashtbl.create 100) (insert_all_in_list2 init_grammaire interest [] init_tokens)

let get_injection_tokens (blackbox : partie list -> bool) (grammar : grammar) : element list = List.filter (fun p -> blackbox [[p]]) (get_all_tokens grammar)

let get_injection_leaves (blackbox : partie list -> bool) (grammar : grammar) : ext_element list = List.map ext_element_of_element (get_injection_tokens blackbox grammar)
