open Grammar

type rev = Nonrev | Rev

let quotient_mem (g: grammar) : ext_element -> ext_grammar  =
    let ext_g : ext_grammar = ext_grammar_of_grammar g
    and da = {pf=[];e=Nonterminal("dummy_axiom");sf=[]}
    (* all the computed rules *)
    and mem : (ext_element, ext_part list) Hashtbl.t = Hashtbl.create 1000
    and sure_useful : (ext_element, bool) Hashtbl.t = Hashtbl.create 1000 in

    let rec get_reachable_symbols (slist : ext_element list) : ext_element list =
        let get_reachable_symbols_once (slist : ext_element list) : ext_element list = slist |> List.rev_map (Hashtbl.find mem) |> List.concat |> List.cons slist |> List.concat |> List.sort_uniq compare |> List.filter is_ext_element_non_terminal in
            let slist2 = get_reachable_symbols_once slist in
            if List.compare_lengths slist slist2 = 0 then slist else (get_reachable_symbols [@tailcall]) slist2
    in

    (* has the element been seen / processed ? *)
    let is_seen (e: ext_element) : bool = Hashtbl.find_opt mem e <> None in

    (* is the element useless, i.e. it is processed but is not bound to any rule *)
    let is_useless (e: ext_element) : bool = Hashtbl.find_opt mem e = Some([]) in

    let set_useless (e: ext_element) : unit = Hashtbl.replace mem e [] in

    (* construct a grammar, given an axiom, from the memory *)
    let grammar_of_mem (axiom : ext_element) : ext_grammar =
        if is_ext_element_terminal axiom then
            da @@@ [da ---> [axiom]]
        else begin
            let rules_of_element : ext_element -> ext_rule list = fun e -> (Hashtbl.find mem e |> List.rev_map ((--->) e)) in
            let rules = get_reachable_symbols [axiom] |> List.rev_map rules_of_element |> List.concat in axiom @@@ rules
        end
    in

    (* reverse an extended element depending of the reverse variable *)
    let reverse_ext_elem (rv: rev) (e: ext_element) = match rv with
        | Rev -> {pf=e.sf;e=e.e;sf=e.pf}
        | Nonrev -> e in

    (* reverse a rule depending of the reverse variable *)
    let reverse_rule (rv: rev) (r: ext_part) = match rv with
        | Nonrev -> r
        | Rev -> List.rev_map (reverse_ext_elem Rev) r in (* this rev_map is not an optimization but is mandatory *)

    (* get the rules from mem, reversing them if necessary *)
    let get_rules (rv: rev) (lhs: ext_element) = 
        Hashtbl.find mem lhs |> List.rev_map (reverse_rule rv) in

    (* memorize rules, reversing them if necessary *)
    let add_rules_in_mem (rv: rev) (lhs: ext_element) (rlist: ext_part list) : unit = match rlist with
        | [] -> ()
        | t::q -> let l = Hashtbl.find mem lhs
            and rlist2 = List.rev_map (reverse_rule rv) rlist in
            (*print_endline ("New rules: "^(string_of_ext_rules (List.rev_map (fun r -> lhs ---> r) rlist2)));*)
            Hashtbl.replace mem lhs (rlist2@l) in

    (* memorize a rule, reversing them if necessary *)
    let add_rule_in_mem (rv: rev) (lhs: ext_element) (r: ext_part) : unit = add_rules_in_mem rv lhs [r] in

    (* does e only derive epsilon ? *)
    let is_epsilon (e: ext_element) : bool = assert (is_seen e); List.for_all ((=) []) (Hashtbl.find mem e)
    (* can e derive epsilon ? *)
    and can_epsilon (e: ext_element) : bool = assert (is_seen e); List.exists ((=) []) (Hashtbl.find mem e) in
    (* create variants of a rule depending of whether the first element is or can be epsilon (and only the first element) *)
    let remove_epsilon : ext_part -> ext_part list = function
        | [] -> []
        | (t::q) as l when is_ext_element_terminal t -> [l] (* a terminal can't derive an epsilon *)
        | t::q when is_epsilon t -> [q] (* t always derive an epsilon: we remove it *)
        | (t::q) as l when can_epsilon t -> [l;q] (* t can derive an epsilon: we add a new rule without it *)
        | l -> [l] in

    (* call remove_epsilon with the rules and the reversed rules *)
    let remove_pf_sf_epsilon (rlist: ext_part list): ext_part list =
        rlist |> List.filter (List.for_all (fun e -> not (is_useless e))) 
            |> List.rev_map remove_epsilon |> List.flatten (* remove epsilon at beginning *)
            |> List.rev_map List.rev |> List.rev_map remove_epsilon |> List.flatten |> List.rev_map List.rev (* remove epsilon at end *)
            |> List.sort_uniq compare in

    (* initialize the memory with the grammar *)
    List.iter (fun r -> set_useless r.ext_left_symbol) ext_g.ext_rules;

    (* we add the rules A -> A so A_[A|] can derive epsilon *)
    ext_g.ext_rules |> List.rev_map lhs_of_ext_rule |> List.sort_uniq compare |> List.iter (fun e -> add_rule_in_mem Nonrev e [e]);

    (* we add the rules of the base grammar *)
    List.iter (fun r -> add_rule_in_mem Nonrev r.ext_left_symbol r.ext_right_part) ext_g.ext_rules;

    (* we assume the base grammar contains only useful symbols *)
    ext_g.ext_rules |> List.rev_map (fun r -> r.ext_left_symbol::r.ext_right_part) |> List.flatten |> List.iter (fun e -> Hashtbl.replace sure_useful e true);

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
            else (add_rule_in_mem rv new_lhs (new_elem::q); Some(new_elem)) in

    (* compute the rules of e with a new prefix (a single element) pf *)
    (* the new rules (left symbol: new_lhs) are based on the rules of the previous_lhs *)
    let quotient_by_one_element_mem (rv: rev) (pf: element) (new_lhs: ext_element) (previous_lhs: ext_element) : (ext_element list) =
        assert (not (is_seen new_lhs) && is_seen previous_lhs);
        set_useless new_lhs; (* all lhs is useless before it gains some rules *)
        get_rules rv previous_lhs |> List.filter_map (quotient_by_one_element rv pf new_lhs) |> List.rev_map (reverse_ext_elem rv) |> List.filter (fun e -> not (is_seen e)) |> List.sort_uniq compare in

    (* compute the rules of a ext_element and do it recursively with every new symbol *)
    (* not completely tail-recursive *)
    let rec quotient_symbols (nb_iter: int) (elist: ext_element list) : int =
        match elist with
        | [] -> nb_iter
        | lhs::q ->
            (* Nothing to do *)
            (* print_endline ("Work on: "^(string_of_ext_element lhs)); *)
            if lhs.pf = [] && lhs.sf = [] then
                (assert (is_ext_element_terminal lhs || is_seen lhs); (quotient_symbols [@tailcall]) (nb_iter + 1) q)
            else begin
                let (base_lhs,rv,qu) = match lhs.pf,lhs.sf with
                | [],[] -> assert false (* impossible : case verified just before *)
                | ((tpf::qpf) as pf),sf when List.compare_lengths pf sf >= 0 -> ({pf=qpf;e=lhs.e;sf=sf},Nonrev,tpf)
                | _,[] -> assert false (* impossible because of the previous case *)
                | pf,(tsf::qsf) -> ({pf=pf;e=lhs.e;sf=qsf},Rev,tsf) in
                (*if rv=Nonrev then print_endline "Nonrev" else print_endline "Rev";*)
                if is_seen lhs then
                    ((*print_endline " Already known";*) (quotient_symbols [@tailcall]) (nb_iter + 1) q)
                else if is_useless base_lhs then
                    (* we ignore this element *)
                    (set_useless lhs; (*print_endline (" Useless because of "^(string_of_ext_element base_lhs));*) (quotient_symbols [@tailcall]) (nb_iter + 1) q)
                else if is_seen base_lhs then begin
                    (* we can compute the current symbol *)
                    (*print_endline "  Compute";*)
                    let new_elist = quotient_by_one_element_mem rv qu lhs base_lhs in
                    (* quick check : if a nonterminal is present in the rhs of all its rules, it is useless *)
                    if Hashtbl.find mem lhs |> List.for_all (List.exists ((=) lhs)) then set_useless lhs;
                    (* before we compute all the new elements, we verify if the current lhs is useful. Indeed, there could be an circular dependency structure *)
                    if Hashtbl.find mem lhs |> List.exists (List.for_all (fun e -> (Hashtbl.find_opt sure_useful e) <> None)) then
                        ((*print_endline "Already useful";*) Hashtbl.add sure_useful lhs true);
                    (*print_endline "New elements:";
                    List.iter (fun e -> print_endline (string_of_ext_element e)) new_elist;*)
                    let new_nb_iter = quotient_symbols nb_iter new_elist in
                    if not (is_useless lhs) && new_elist <> [] then begin
                        (* verify uselessness of the rhs *)
                        let rules = remove_pf_sf_epsilon (Hashtbl.find mem lhs) in
                        let rules = if can_epsilon lhs then remove_pf_sf_epsilon (Hashtbl.find mem lhs) else rules in
                        Hashtbl.replace mem lhs rules;
                        (* maybe rhs is now epsilon-capable itself *)
                        (*print_endline ("Updated rules: "^(string_of_ext_rules (List.rev_map (fun r -> lhs ---> r) rules)));*)
                    end;
                    (* lhs is not useful and no rhs has only useful elements : lhs is useless *)
                    if (Hashtbl.find_opt sure_useful lhs) = None then begin
                        if Hashtbl.find mem lhs |> List.for_all (List.exists (fun e -> (Hashtbl.find_opt sure_useful e) = None)) then
                            ((*print_endline "Not useful !";*)
                            set_useless lhs)
                        else (* otherwise, we know that lhs is useful *)
                            (assert ((Hashtbl.find_opt sure_useful lhs) = None); Hashtbl.add sure_useful lhs true)
                    end;
                    (*if is_useless lhs then
                        print_endline ("Useless: "^(string_of_ext_element lhs));*)
                    (quotient_symbols [@tailcall]) (new_nb_iter + 1) q
                    end
                else begin
                    (*print_endline "  Postpone";*)
                    (* we can't compute the current symbol so we keep it on the list *)
                    ((quotient_symbols [@tailcall]) (nb_iter + 1) (base_lhs::elist)) end
            end
    in
    fun (e: ext_element) : ext_grammar ->
        (* the case when e is terminal is handled separately *)
        if is_ext_element_terminal e then begin
            (* derive epsilon *)
            if (e.pf=[e.e] && e.sf=[]) || (e.pf=[] && e.sf=[e.e]) then
                da@@@[da--->[]]
            (* derive this terminal *)
            else if e.pf=[] && e.sf=[] then
                da@@@[da--->[e]]
            (* empty language *)
            else
                da@@@[]
        end else begin
            print_endline ("Nb iter: "^(string_of_int (quotient_symbols 0 [e]))^", memory size: "^(string_of_int (Hashtbl.length mem))); 
            (* clean the grammar: remove useless, trivial rules, epsilon, etc. *)
            Clean.clean (grammar_of_mem e)
        end
