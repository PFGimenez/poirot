open Base

let quotient_mem (g: grammar) =
    let ext_g = ext_grammar_of_grammar g
    (* all the computed rules *)
    and mem : (ext_element, ext_part list) Hashtbl.t = Hashtbl.create 1000 in
(*    let print_mem () = print_endline "Mem status: "; Hashtbl.iter (fun e rlist -> print_endline (string_of_ext_element e); print_endline (string_of_ext_rules rlist)) mem 
    in *)

    let get_reachable_symbols_once (slist : ext_element list) : ext_element list = List.filter is_ext_element_non_terminal (List.sort_uniq compare (List.concat (slist::(List.concat (List.map (Hashtbl.find mem) slist)))))
    in

    let rec get_reachable_symbols (slist : ext_element list) : ext_element list =
        let slist2 = get_reachable_symbols_once slist in
        if List.compare_lengths slist slist2 == 0 then slist else (get_reachable_symbols [@tailcall]) slist2
    in

    let is_seen (e: ext_element) : bool = Hashtbl.find_opt mem e <> None in

    let is_useless (e: ext_element) : bool = Hashtbl.find_opt mem e = Some([]) in

    let set_useless (e: ext_element) : unit = Hashtbl.replace mem e [] in

    let grammar_of_mem (axiom : ext_element) : ext_grammar =
        let sym = get_reachable_symbols [axiom] in
        axiom @@@ (List.concat (List.map (fun (e : ext_element) : ext_rule list -> List.map (fun (r: ext_part) : ext_rule -> (e--->r)) (Hashtbl.find mem e)) sym))
    in

    let add_rules_in_mem (lhs: ext_element) (rlist: ext_part list) : unit = match rlist with
        | [] -> ()
        | t::q -> print_endline ("New rules: "^(string_of_ext_rules (List.map (fun r -> lhs ---> r) rlist))); let l = Hashtbl.find mem lhs in
            Hashtbl.replace mem lhs (rlist@l)
    in

    let add_rule_in_mem lhs (r: ext_part) : unit = add_rules_in_mem lhs [r] in

    let is_epsilon (e: ext_element) : bool = assert (is_seen e); List.for_all (fun r -> r = []) (Hashtbl.find mem e)
    in

    let can_epsilon (e: ext_element) : bool = assert (is_seen e); List.exists (fun r -> r = []) (Hashtbl.find mem e)
    in

    let remove_epsilon : ext_part -> ext_part list = function
        | [] -> []
        | (t::q) as l when is_ext_element_terminal t -> [l]
        | t::q when is_epsilon t -> [q]
        | (t::q) as l when can_epsilon t -> [l;q]
        | l -> [l]
    in

    (* initialize the memory with the grammar *)
    List.iter (fun r -> set_useless r.ext_left_symbol) ext_g.ext_rules;
    List.iter (fun r -> add_rule_in_mem r.ext_left_symbol r.ext_right_part) ext_g.ext_rules;

    (* apply a quotient of a single rule with a prefix that is a single element *)
    let quotient_by_one_element (pf: element) (new_lhs: ext_element) (r: ext_part) : ext_element option =
        assert (is_seen new_lhs);
        match r with
        | [] -> None
        (* A -> aBC with prefix = a *)
        | t::q when t.e=pf && is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); add_rule_in_mem new_lhs q; None
        (* A -> aBC with prefix != a *)
        | t::q when is_ext_element_terminal t -> None
        (* A -> BC with prefix = B *)
        | t::q when t.e=pf && t.pf=[] -> let new_elem = {pf=[pf];e=t.e;sf=t.sf} in
            (* if B_{B|} is a dead end *)
            if is_useless new_elem then
                (add_rule_in_mem new_lhs q; None)
            (* if B can derive a leftmost B *)
            else
                (add_rules_in_mem new_lhs [q;new_elem::q]; Some(new_elem))
        (* A -> B_{D|}C *)
        | t::q -> let new_elem = {pf=pf::t.pf;e=t.e;sf=t.sf} in
            if is_useless new_elem then None
            else (add_rule_in_mem new_lhs (new_elem::q); Some(new_elem))
    in

    (* compute the rules of e with a new prefix (a single element) pf *)
    (* the new rules (left symbol: new_lhs) are based on the rules of the previous_lhs *)
    let quotient_by_one_element_mem (pf: element) (new_lhs: ext_element) (previous_lhs: ext_element) : (ext_element list) =
        assert (not (is_seen new_lhs) && is_seen previous_lhs);
        set_useless new_lhs; (* all lhs is useless before it gains some rules *)
        List.sort_uniq compare (List.filter (fun e -> not (is_seen e)) (List.filter_map (quotient_by_one_element pf new_lhs) (Hashtbl.find mem previous_lhs)))
    in

    (* compute the rules of a ext_element and do it recursively with every new symbol *)
    let rec quotient_symbols (nb_iter: int) (elist: ext_element list) : int =
        match elist with
        | [] -> nb_iter
        | lhs::q -> print_endline ("Work on: "^(string_of_ext_element lhs));
            match lhs.pf with
            | [] -> quotient_symbols (nb_iter + 1) q
            | tpf::qpf -> let base_lhs = {pf=qpf;e=lhs.e;sf=lhs.sf} in
                    if is_seen lhs then
                        (print_endline " Already known"; quotient_symbols (nb_iter + 1) q)
                    else if is_useless base_lhs then
                        (* we ignore this element *)
                        (set_useless lhs; print_endline " Useless"; quotient_symbols (nb_iter + 1) q)
                    else if qpf = [] || is_seen base_lhs then begin
                        (* we can compute the current symbol *)
                        print_endline "  Compute";
                        let new_elist = quotient_by_one_element_mem tpf lhs base_lhs in
                        let new_nb_iter = quotient_symbols nb_iter new_elist in
                        if new_elist <> [] then begin
                            (* verify uselessness of the rhs *)
                            let rules = Hashtbl.find mem lhs in
                            let necessary_rules = List.filter (List.for_all (fun e -> not (is_useless e))) rules in
                            (* if A -> BC and B can derive an epsilon, then add a rule A -> C. Necessary for quotient. *)
                            let nonepsilon_rules = List.sort_uniq compare (List.flatten (List.map remove_epsilon necessary_rules)) in
                            print_endline ("Updated rules: "^(string_of_ext_rules (List.map (fun r -> lhs ---> r) nonepsilon_rules)));
                            Hashtbl.replace mem lhs nonepsilon_rules
                        end;
                        if is_useless lhs then
                            print_endline ("Useless: "^(string_of_ext_element lhs));
                        (quotient_symbols [@tailcall]) (new_nb_iter + 1) q
                        end
                    else begin
                        print_endline "  Postpone";
                        (* we can't compute the current symbol so we keep it on the list *)
                        ((quotient_symbols [@tailcall]) (nb_iter + 1) (base_lhs::elist)) end
    in 
    (fun (e: ext_element) : ext_grammar -> print_endline ("Nb iter: "^(string_of_int (quotient_symbols 0 [e])));
    let out : ext_grammar = grammar_of_mem e in
    assert (out = (Clean.clean out)); out)
