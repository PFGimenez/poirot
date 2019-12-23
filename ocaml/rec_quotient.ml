open Base

type rev = Nonrev | Rev

let quotient_mem (g: grammar) : ext_element -> ext_grammar  =
    let ext_g : ext_grammar = ext_grammar_of_grammar g
    (* all the computed rules *)
    and mem : (ext_element, ext_part list) Hashtbl.t = Hashtbl.create 1000 in
(*    let print_mem () = print_endline "Mem status: "; Hashtbl.iter (fun e rlist -> print_endline (string_of_ext_element e); print_endline (string_of_ext_rules rlist)) mem 
    in *)

    let rec get_reachable_symbols (slist : ext_element list) : ext_element list =
        let get_reachable_symbols_once (slist : ext_element list) : ext_element list = slist |> List.rev_map (Hashtbl.find mem) |> List.concat |> List.cons slist |> List.concat |> List.sort_uniq compare |> List.filter is_ext_element_non_terminal in
            let slist2 = get_reachable_symbols_once slist in
            if List.compare_lengths slist slist2 == 0 then slist else (get_reachable_symbols [@tailcall]) slist2
    in

    let is_seen (e: ext_element) : bool = Hashtbl.find_opt mem e <> None in

    let is_useless (e: ext_element) : bool = Hashtbl.find_opt mem e = Some([]) in

    let set_useless (e: ext_element) : unit = Hashtbl.replace mem e [] in

    let grammar_of_mem (axiom : ext_element) : ext_grammar =
        let rules_of_element = fun e -> Hashtbl.find mem e |> List.rev_map (fun (r: ext_part) : ext_rule -> (e--->r)) in
        let rules = get_reachable_symbols [axiom] |> List.rev_map rules_of_element |> List.concat in
        axiom @@@ rules
    in

    let reverse_ext_elem (rv: rev) (e: ext_element) = match rv with
        | Rev -> {pf=e.sf;e=e.e;sf=e.pf}
        | Nonrev -> e
    in

    let reverse_rule (rv: rev) (r: ext_part) = match rv with
        | Nonrev -> r
        | Rev -> r |> List.map (reverse_ext_elem rv) |> List.rev
    in

    let get_rules (rv: rev) (lhs: ext_element) = 
        Hashtbl.find mem lhs |> List.map (reverse_rule rv)
    in

    let add_rules_in_mem (rv: rev) (lhs: ext_element) (rlist: ext_part list) : unit = match rlist with
        | [] -> ()
        | t::q -> let l = Hashtbl.find mem lhs
            and rlist2 = List.map (reverse_rule rv) rlist in
            print_endline ("New rules: "^(string_of_ext_rules (List.rev_map (fun r -> lhs ---> r) rlist2)));
            Hashtbl.replace mem lhs (rlist2@l)
    in

    let add_rule_in_mem (rv: rev) (lhs: ext_element) (r: ext_part) : unit = add_rules_in_mem rv lhs [r] in

    let remove_epsilon : ext_part -> ext_part list =
        let is_epsilon (e: ext_element) : bool = assert (is_seen e); List.for_all (fun r -> r = []) (Hashtbl.find mem e)
        and can_epsilon (e: ext_element) : bool = assert (is_seen e); List.exists (fun r -> r = []) (Hashtbl.find mem e)
        in
        function
        | [] -> []
        | (t::q) as l when is_ext_element_terminal t -> [l] (* a terminal can't derive an epsilon *)
        | t::q when is_epsilon t -> [q] (* t always derive an epsilon: we remove it *)
        | (t::q) as l when can_epsilon t -> [l;q] (* t can derive an epsilon: we add a new rule without it *)
        | l -> [l]
    in

    (* initialize the memory with the grammar *)
    List.iter (fun r -> set_useless r.ext_left_symbol) ext_g.ext_rules;
    List.iter (fun r -> add_rule_in_mem Nonrev r.ext_left_symbol r.ext_right_part) ext_g.ext_rules;

    (* apply a quotient of a single rule with a prefix that is a single element *)
    let quotient_by_one_element (rv: rev) (pf: element) (new_lhs: ext_element) (r: ext_part) : ext_element option =
        assert (is_seen new_lhs);
        match r with
        | [] -> None
        (* A -> aBC with prefix = a *)
        | t::q when t.e=pf && is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); add_rule_in_mem rv new_lhs q; None
        (* A -> aBC with prefix != a *)
        | t::q when is_ext_element_terminal t -> None
        (* A -> BC with prefix = B *)
        | t::q when t.e=pf && t.pf=[] -> let new_elem = {pf=[pf];e=t.e;sf=t.sf} in
            (* if B_{B|} is a dead end *)
            if is_useless new_elem then
                (add_rule_in_mem rv new_lhs q; None)
            (* if B can derive a leftmost B *)
            else
                (add_rules_in_mem rv new_lhs [q;new_elem::q]; Some(new_elem))
        (* A -> B_{D|}C *)
        | t::q -> let new_elem = {pf=pf::t.pf;e=t.e;sf=t.sf} in
            if is_useless new_elem then None
            else (add_rule_in_mem rv new_lhs (new_elem::q); Some(new_elem))
    in

    (* compute the rules of e with a new prefix (a single element) pf *)
    (* the new rules (left symbol: new_lhs) are based on the rules of the previous_lhs *)
    let quotient_by_one_element_mem (rv: rev) (pf: element) (new_lhs: ext_element) (previous_lhs: ext_element) : (ext_element list) =
        assert (not (is_seen new_lhs) && is_seen previous_lhs);
        set_useless new_lhs; (* all lhs is useless before it gains some rules *)
        get_rules rv previous_lhs |> List.filter_map (quotient_by_one_element rv pf new_lhs) |> List.map (reverse_ext_elem rv) |> List.filter (fun e -> not (is_seen e)) |> List.sort_uniq compare
    in

    (* compute the rules of a ext_element and do it recursively with every new symbol *)
    let rec quotient_symbols (nb_iter: int) (elist: ext_element list) : int =
        match elist with
        | [] -> nb_iter
        | lhs::q -> 
            (* Nothing to do *)
            if lhs.pf = [] && lhs.sf = [] then
                (assert (is_seen lhs); quotient_symbols (nb_iter + 1) q)
            else begin
                let (base_lhs,rv,qu) = match lhs.pf,lhs.sf with
                | [],[] -> assert false
                | ((tpf::qpf) as pf),sf when List.compare_lengths pf sf >= 0 -> ({pf=qpf;e=lhs.e;sf=sf},Nonrev,tpf)
                | _,[] -> assert false
                | pf,(tsf::qsf) -> ({pf=pf;e=lhs.e;sf=qsf},Rev,tsf) in
                print_endline ("Work on: "^(string_of_ext_element lhs));
                if rv=Nonrev then print_endline "Nonrev" else print_endline "Rev";
                if is_seen lhs then
                    (print_endline " Already known"; quotient_symbols (nb_iter + 1) q)
                else if is_useless base_lhs then
                    (* we ignore this element *)
                    (set_useless lhs; print_endline " Useless"; quotient_symbols (nb_iter + 1) q)
                else if is_seen base_lhs then begin
                    (* we can compute the current symbol *)
                    print_endline "  Compute";
                    let new_elist = quotient_by_one_element_mem rv qu lhs base_lhs in
                    print_endline "New elements:";
                    List.iter (fun e -> print_endline (string_of_ext_element e)) new_elist;
                    let new_nb_iter = quotient_symbols nb_iter new_elist in
                    if new_elist <> [] then begin
                        (* verify uselessness of the rhs *)
                        let rules = Hashtbl.find mem lhs |> List.filter (List.for_all (fun e -> not (is_useless e))) |> List.rev_map remove_epsilon |> List.flatten |> List.sort_uniq compare in
                        print_endline ("Updated rules: "^(string_of_ext_rules (List.rev_map (fun r -> lhs ---> r) rules)));
                        Hashtbl.replace mem lhs rules
                    end;
                    if is_useless lhs then
                        print_endline ("Useless: "^(string_of_ext_element lhs));
                    (quotient_symbols [@tailcall]) (new_nb_iter + 1) q
                    end
                else begin
                    print_endline "  Postpone";
                    (* we can't compute the current symbol so we keep it on the list *)
                    ((quotient_symbols [@tailcall]) (nb_iter + 1) (base_lhs::elist)) end
            end
    in 
    (fun (e: ext_element) : ext_grammar -> print_endline ("Nb iter: "^(string_of_int (quotient_symbols 0 [e])));
    let out : ext_grammar = grammar_of_mem e in
        assert (out = (Clean.clean out)); out)
