open Base

(* get the first symbol of the rhs of a rule *)
let extract_left_symbol (r : rule) : element option = match r.right_part with
    | [] -> None
    | t::_ -> Some(t)

(* get the last symbol of the rhs of a rule *)
let extract_right_symbol (r : rule) : element option = match List.length r.right_part with
    | 0 -> None
    | n -> assert (n > 0); Some(List.nth r.right_part (n-1))

(* get the list of proper prefix/suffix. By proper prefix, it means that A is a proper prefix of A if A is recursively reachable from A *)
let rec symbol_list_aux (f : rule -> element option) (g: grammar) (tasks: element list) (seen: element list) (first: bool): element list = match tasks with
    | [] -> seen
    | t::q when List.mem t seen -> (symbol_list_aux [@tailcall]) f g q seen false
    | t::q -> let new_seen = (List.filter_map f (List.filter (fun r -> r.left_symbol = t) g.rules)) in
        if first then
            (symbol_list_aux [@tailcall]) f g (new_seen@q) [] false
        else (symbol_list_aux [@tailcall]) f g (new_seen@q) (t::seen) false

(* get the list of symbols that can be a prefix of the derivation of element *)
let left_symbol_list (g: grammar) (task: element) : element list =
    symbol_list_aux extract_left_symbol g [task] [] true

(* get the list of symbols that can be a suffix of the derivation of element *)
let right_symbol_list (g: grammar) (task: element) : element list =
    symbol_list_aux extract_right_symbol g [task] [] true

(* get the list of rules that can directly derive this element *)
(* TODO: bouger dans blind *)
let get_generative_rules (g: grammar) (e: element) : ext_rule list =
    List.filter_map (fun r -> if List.mem e r.right_part then Some(ext_rule_of_rule r) else None) g.rules

let quotient_mem (g: grammar) =
    let ext_g = ext_grammar_of_grammar g
    (* all the computed rules *)
    and mem : (ext_element, ext_part list) Hashtbl.t = Hashtbl.create 1000
    and all_sym : (element list) = get_all_symbols g in

    (* the base grammar is already computed *)

(*    let print_mem () = print_endline "Mem status: "; Hashtbl.iter (fun e rlist -> print_endline (string_of_ext_element e); print_endline (string_of_ext_rules rlist)) mem 
    in *)

    let all_non_terminal : (element list) = List.filter (is_non_terminal) all_sym
    and create_useless (e: element) : ext_element list =
        let useful = left_symbol_list g e in
        let check_useless (p : element) : ext_element option =
            if not (List.mem p useful) then Some({pf=[p];e=e;sf=[]})
            else None
        in
        List.filter_map check_useless all_sym
    in

    let get_reachable_symbols_once (slist : ext_element list) : ext_element list = List.filter is_ext_element_non_terminal (List.sort_uniq compare (List.concat (slist::(List.concat (List.map (Hashtbl.find mem) slist)))))
    in

    let rec get_reachable_symbols (slist : ext_element list) : ext_element list =
        let slist2 = get_reachable_symbols_once slist in
        if List.compare_lengths slist slist2 == 0 then slist else (get_reachable_symbols [@tailcall]) slist2
    in

    let is_seen (e: ext_element) = Hashtbl.find_opt mem e <> None in

    let is_useless (e: ext_element) = Hashtbl.find_opt mem e = Some([]) in

    let set_useless (e: ext_element) = Hashtbl.replace mem e [] in

    let grammar_of_mem (axiom : ext_element) : ext_grammar =
        let sym = get_reachable_symbols [axiom] in
        axiom @@@ (List.concat (List.map (fun (e : ext_element) : ext_rule list -> List.map (fun (r: ext_part) : ext_rule -> (e--->r)) (Hashtbl.find mem e)) sym))
    in

    let add_rules_in_mem (lhs: ext_element) (rlist: ext_part list) = match rlist with
        | [] -> ()
        | t::q -> print_endline ("New rules: "^(string_of_ext_rules (List.map (fun r -> lhs ---> r) rlist))); let l = Hashtbl.find mem lhs in
            Hashtbl.replace mem lhs (rlist@l)
    in

    let add_rule_in_mem lhs (r: ext_part) = add_rules_in_mem lhs [r] in

    let is_epsilon (e: ext_element) = is_seen e && List.exists (fun r -> r = []) (Hashtbl.find mem e)
    in

    List.iter (fun e -> Hashtbl.add mem e (List.map (fun r -> r.ext_right_part) (Base.get_rules ext_g.ext_rules e))) (List.map ext_element_of_element all_non_terminal);
    (* we don't need to compute the useless of terminal since their "quotientability" is trivial *)
    List.iter set_useless (List.flatten (List.map create_useless all_non_terminal));

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
    let rec quotient_symbols (elist: ext_element list) : unit =
        match elist with
        | [] -> ()
        | lhs::q -> print_endline ("Work on: "^(string_of_ext_element lhs));
            match lhs.pf with
            | [] -> quotient_symbols q
            | tpf::qpf -> let base_lhs = {pf=qpf;e=lhs.e;sf=lhs.sf} in
                    if is_useless base_lhs || is_seen lhs then
                        (* we ignore this element *)
                        (set_useless lhs; print_endline " Ignored";quotient_symbols q)
                    else if qpf = [] || is_seen base_lhs then begin
                        (* we can compute the current symbol *)
                        print_endline "  Compute";
                        let new_elist = quotient_by_one_element_mem tpf lhs base_lhs in
                        quotient_symbols new_elist;
                        if new_elist <> [] then begin
                            (* verify uselessness *)
                            let rules = Hashtbl.find mem lhs in
                            let necessary_rules = List.filter (List.for_all (fun e -> not (is_useless e))) rules in
                            Hashtbl.replace mem lhs necessary_rules
                        end;
                        if is_useless lhs then
                            print_endline ("Useless: "^(string_of_ext_element lhs));
                        (quotient_symbols [@tailcall]) q
                        end
                    else begin
                        print_endline "  Postpone";
                        (* we can't compute the current symbol so we keep it on the list *)
                        ((quotient_symbols [@tailcall]) (base_lhs::elist)) end
    in 
    (fun (e: ext_element) : ext_grammar -> quotient_symbols [e];
    let out : ext_grammar = grammar_of_mem e in
    assert (out = (Clean.clean out)); out)
