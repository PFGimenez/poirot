open Base

let search (fuzzer: ext_grammar -> bool) (g: grammar) (goal: element) (injection_tokens: element list) : ext_grammar option =
    let quotient = Rec_quotient.quotient_mem g
    and distance_to_goal : (element * element, int) Hashtbl.t = Hashtbl.create 100
    and all_sym = g.rules |> List.map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare in

    let rec is_reachable (s : element) (reachable : element list) : bool =
        if List.mem s reachable then true
        else
            let ext_rules = List.filter (fun r -> List.mem r.left_symbol reachable) g.rules in
            let new_reachable = List.sort_uniq compare (List.flatten (List.map (fun r -> r.right_part) ext_rules)) in
                if (List.length reachable) = (List.length new_reachable) then false
                else (is_reachable [@tailcall]) s new_reachable in

    let symbols_from_parents (axiom : element) : element list = List.sort_uniq compare (List.map (fun r -> r.left_symbol) (List.filter (fun r -> List.mem axiom r.right_part) g.rules)) in

    let rec get_distance_to_goal (e : element) : (element * int) list -> int = function
    | [] -> failwith "Can't reach at all"
    | (s,nb)::q when is_reachable e [s] -> nb 
    | (s,nb)::q -> (get_distance_to_goal [@tailcall]) e (q@(List.map (fun e -> (e,nb+1)) (symbols_from_parents s))) in

    let compute_one_distance (a: element) (b: element) : unit =
        Hashtbl.add distance_to_goal (a,b) (get_distance_to_goal a [b,0]) in

    all_sym |> List.iter (fun e -> all_sym |> List.iter (compute_one_distance e));

    let compare_with_score (a: int * int * rule * ext_element) (b: int * int * rule * ext_element) : int = match a,b with
        | (ag,ah,_,_),(bg,bh,_,_) when ag+ah < bg+bh || (ag+ah = bg+bh && ah < bh) -> -1
        | (ag,ah,_,_),(bg,bh,_,_) when ag=bg && bh=bh -> 0
        | _ -> 1 in

    let add_in_list g (openset: (int * int * rule * ext_element) list) (new_elems: (rule * ext_element) list) : (int * int * rule * ext_element) list =
        List.merge compare_with_score (List.sort compare_with_score (List.map (fun ((r,e): rule * ext_element) : (int * int * rule * ext_element) -> (g,Hashtbl.find distance_to_goal (element_of_ext_element e, goal),r,e)) new_elems)) openset in

    let split (elem : element) (original_rule: rule) : (element list * element list * rule) list =
        let rec split_aux (prefix : element list) (acc: (element list * element list * rule) list) (rhs: part) : (element list * element list * rule) list = match rhs with
        | [] -> acc
        | t::q when t=elem -> (split_aux [@tailcall]) (t::prefix) ((prefix,q,original_rule)::acc) q
        | t::q -> (split_aux [@tailcall]) (t::prefix) acc q in
    split_aux [] [] original_rule.right_part in

    let build_ext_elements (e: ext_element) : (rule * ext_element) list =
        g.rules |> List.filter (fun r -> List.exists (fun s -> s=e.e) r.right_part) |> List.map (split e.e) |> List.flatten |> List.map (fun (pf,sf,r) -> (r,{pf=pf@e.pf;e=e.e;sf=sf@e.sf})) in

    let rec search_aux (visited: (ext_element, bool) Hashtbl.t) (step: int) : (int * int * rule * ext_element) list -> ext_grammar option = function
    | [] -> None
    | (g,_,r,t)::q ->
        print_endline ("Search "^(string_of_int step)^" (queue: "^(string_of_int ((List.length q) + 1))^")");
        if Hashtbl.mem visited t then begin
            print_endline "Visited"; (search_aux [@tailcall]) visited (step + 1) q
        end else begin
            let ext_g = quotient t in
            print_endline (string_of_ext_grammar ext_g);
            (*print_endline "Grammar built"; flush stdout;*)
            (*print_endline ("Accessible from "^(element2string g.axiom)^": "); print_bool (is_accessible_from_ext_axiom init_grammaire interest [g.axiom]); flush stdout;*)
            (*print_endline ("Distance: "^(string_of_int (distance_to_goal init_grammaire interest [(trim g.axiom,0)])));*)
            let rules = ext_g.ext_rules |> List.filter (fun r -> r.ext_left_symbol = t) in
            assert ((List.compare_length_with rules 1) >= 0);
            if (List.compare_length_with rules 1) > 0 then begin (* testable *)
            if not (fuzzer ext_g) then begin (* invalid : ignore *)
                print_endline "Invalid"; (search_aux [@tailcall]) visited (step + 1) q
            end else if g == 0 then begin (* found ! *)
                print_endline "Found!"; Some(ext_g)
            end else begin (* we explore in this direction *)
                print_endline "Explore";
                Hashtbl.add visited t true;
                (search_aux [@tailcall]) visited (step + 1) (add_in_list (g+1) q (build_ext_elements t))
            end
            end else
                (search_aux [@tailcall]) visited (step + 1) (add_in_list (g+1) q (build_ext_elements t))
        end in
    injection_tokens |> List.map (fun e -> (e-->[e],ext_element_of_element e)) |> add_in_list 0 [] |> search_aux (Hashtbl.create 100) 0


