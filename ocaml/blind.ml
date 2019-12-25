open Base

(* TODO: résoudre fuzzer pour exemple avec parenthèse *)
exception No_trivial_injection
exception Unknown_goal

let search (fuzzer: grammar -> part list) (oracle: part list -> bool) (g: grammar) (goal: element) (max_depth: int) : ext_grammar option =
    let quotient = Rec_quotient.quotient_mem g
    and distance_to_goal : (element * element, int) Hashtbl.t = Hashtbl.create 100
    and all_sym = g.rules |> List.map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare in

    let get_injection_tokens (oracle : part list -> bool) (grammar : grammar) : element list = List.filter (fun p -> oracle [[p]]) (Base.get_all_tokens grammar) in

    let symbols_from_parents (axiom : element) : element list =
        g.rules |> List.filter (fun r -> List.mem axiom r.right_part) |> List.map (fun r -> r.left_symbol) |> List.sort_uniq compare in

    let rec compute_distance_to_goal (e : element) : (element * int) list -> int = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_reachable g e [s] -> nb
    | (s,nb)::q -> (compute_distance_to_goal [@tailcall]) e (q@(List.map (fun e -> (e,nb+1)) (symbols_from_parents s))) in

    let compute_one_distance (a: element) (b: element) : unit =
        Hashtbl.add distance_to_goal (a,b) (compute_distance_to_goal b [a,0]) in

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
                    if not (inj_g |> grammar_of_ext_grammar |> fuzzer |> oracle) then (* invalid : ignore *)
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
    let inj = get_injection_tokens oracle g in
    if not (is_reachable g goal [g.axiom]) then raise Unknown_goal
    else if inj = [] then raise No_trivial_injection
    else inj |> List.map ext_element_of_element |> add_in_list 0 [] |> search_aux (Hashtbl.create 100) 0

