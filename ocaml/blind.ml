open Base

(* TODO: vérifier distance to goal *)
(* TODO: intégrer la découverte automatique des tokens d'injection *)
(* TODO: résoudre fuzzer pour exemple avec parenthèse *)

let rec is_reachable (g: grammar) (s: element) (reachable : element list) : bool =
    if List.mem s reachable then true
    else
        let ext_rules = List.filter (fun r -> List.mem r.left_symbol reachable) g.rules in
        let new_reachable = ext_rules |> List.map (fun r -> r.right_part) |> List.flatten |> List.append reachable |> List.sort_uniq compare in
        if (List.compare_lengths reachable new_reachable) = 0 then false
        else (is_reachable [@tailcall]) g s new_reachable

let get_all_tokens (grammar : grammar) : element list = List.sort_uniq compare (List.concat (List.map (fun r -> List.filter is_terminal r.right_part) grammar.rules))

let get_injection_tokens (oracle : part list -> bool) (grammar : grammar) : element list = List.filter (fun p -> oracle [[p]]) (get_all_tokens grammar)

let get_injection_leaves (oracle : part list -> bool) (grammar : grammar) : element list = get_injection_tokens oracle grammar

let fuzzer (g : grammar) : part list =
    let term = List.filter (fun e -> is_reachable g e []) (get_all_tokens g) in
    List.map (Fuzzer.derive_word_with_symbol g) term

let oracle_template
    (prefix : element list)
    (suffix : element list)
    (grammar : grammar)
    (injections : part list)
    : bool
    = Fuzzer.is_list_in_language grammar (List.map (fun p -> prefix @ p @ suffix) injections)

let search (fuzzer: grammar -> part list) (oracle: part list -> bool) (g: grammar) (goal: element) (max_depth: int) (injection_tokens: element list) : ext_grammar option =
    let quotient = Rec_quotient.quotient_mem g
    and distance_to_goal : (element * element, int) Hashtbl.t = Hashtbl.create 100
    and all_sym = g.rules |> List.map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare
    and ext_g = ext_grammar_of_grammar g in

    let symbols_from_parents (axiom : element) : element list = List.sort_uniq compare (List.map (fun r -> r.left_symbol) (List.filter (fun r -> List.mem axiom r.right_part) g.rules)) in

    let rec compute_distance_to_goal (e : element) : (element * int) list -> int = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_reachable g e [s] -> nb 
    | (s,nb)::q -> (compute_distance_to_goal [@tailcall]) e (q@(List.map (fun e -> (e,nb+1)) (symbols_from_parents s))) in

    let compute_one_distance (a: element) (b: element) : unit =
        Hashtbl.add distance_to_goal (a,b) (compute_distance_to_goal a [b,0]) in

    let get_distance_to_goal (e: element) : int =
        Hashtbl.find distance_to_goal (e,goal) in

    all_sym |> List.iter (fun e -> all_sym |> List.iter (compute_one_distance e));

    let compare_with_score (a: int * int * ext_element) (b: int * int * ext_element) : int = match a,b with
        | (ag,ah,_),(bg,bh,_) when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
        | (ag,ah,_),(bg,bh,_) when ag=bg && ah=bh -> 0
        | _ -> 1 in

    let add_in_list g (openset: (int * int * ext_element) list) (new_elems: ext_element list) : (int * int * ext_element) list =
        (* openset is already sorted *)
        List.merge compare_with_score (List.sort compare_with_score (List.map (fun (e: ext_element) : (int * int * ext_element) -> (g,(Hashtbl.find distance_to_goal (element_of_ext_element e, goal))+Random.int 2,e)) new_elems)) openset in

    let split (elem : element) (original_rule: rule) : (element list * element list * element) list =
        let rec split_aux (prefix : element list) (acc: (element list * element list * element) list) (rhs: part) : (element list * element list * element) list = match rhs with
        | [] -> acc
        | t::q when t=elem -> (split_aux [@tailcall]) (t::prefix) ((prefix,q,original_rule.left_symbol)::acc) q
        | t::q -> (split_aux [@tailcall]) (t::prefix) acc q in
    split_aux [] [] original_rule.right_part in

    let build_ext_elements (e: ext_element) : ext_element list =
        g.rules |> List.filter (fun r -> List.exists (fun s -> s=e.e) r.right_part) |> List.map (split e.e) |> List.flatten |> List.map (fun (pf,sf,lhs) -> {pf=e.pf@pf;e=lhs;sf=e.sf@sf}) in

(*    let print_openset (openset: (int * int * ext_element) list) : unit =
        List.iter (fun (g,h,t) -> print_endline ((string_of_ext_element t)^", f="^(string_of_int (g+h))^", h="^(string_of_int h))) openset in*)

    let rec search_aux (visited: (ext_element, bool) Hashtbl.t) (step: int) (openset: (int * int * ext_element) list) : ext_grammar option = match openset with
    | [] -> None
    | (distance,_,t)::q ->
        print_endline ("Search "^(string_of_int step)^" (queue: "^(string_of_int (List.length q))^"): "^(string_of_ext_element t));
        if Hashtbl.mem visited t then
            (print_endline "Visited"; (search_aux [@tailcall]) visited (step + 1) q)
(*        else if distance > max_depth then
            (print_endline "Too deep"; (search_aux [@tailcall]) visited (step + 1) q) *)
        else begin
            Hashtbl.add visited t true;
            if is_ext_element_non_terminal t then begin
                let inj_g = quotient t in
                print_endline (string_of_ext_grammar inj_g);
                let h = get_distance_to_goal t.e in
                print_endline "Grammar built";
                (*print_endline ("Accessible from "^(element2string g.axiom)^": "); print_bool (is_accessible_from_ext_axiom init_grammaire interest [g.axiom]); flush stdout;*)
                print_endline ("Distance: "^(string_of_int h));
                let rules = inj_g.ext_rules |> List.filter (fun r -> t=r.ext_left_symbol) in
                assert ((List.compare_length_with rules 1) >= 0);
                if (List.compare_length_with rules 1) > 0 then begin (* testable *)
                    if not (ext_g |> grammar_of_ext_grammar |> fuzzer |> oracle) then (* invalid : ignore *)
                        (print_endline "Invalid"; (search_aux [@tailcall]) visited (step + 1) q)
                    else if is_reachable (grammar_of_ext_grammar inj_g) goal [full_element_of_ext_element inj_g.ext_axiom] then (* found ! *)
                        (print_endline "Found!"; Some(inj_g))
                    else if distance = max_depth then
                        (print_endline "Depth max"; (search_aux [@tailcall]) visited (step + 1) q)
                    else (* we explore in this direction *)
                        (print_endline "Explore";
                        (search_aux [@tailcall]) visited (step + 1) (add_in_list (distance+1) q (build_ext_elements t)))
                end else if distance = max_depth then (* not testable *)
                    (print_endline "Depth max"; (search_aux [@tailcall]) visited (step + 1) q)
                else
                    (print_endline "Not testable"; (search_aux [@tailcall]) visited (step + 1) (add_in_list (distance + 1) q (build_ext_elements t)))
            end
            else (* t is terminal *)
                (search_aux [@tailcall]) visited (step + 1) (add_in_list (distance+1) q (build_ext_elements t))
            end in
    injection_tokens |> List.map ext_element_of_element |> add_in_list 0 [] |> search_aux (Hashtbl.create 100) 0

