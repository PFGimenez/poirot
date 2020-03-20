open Grammar

type side = Left | Right

(* a small finite-state automata for the element. not in status hashtable -> In_progress -> Processed *)
type elem_status = In_progress | Processed

let quotient_mem (g: grammar) (graph_channel: out_channel option) (*(verbose: bool)*): ext_element -> ext_grammar  =
    (* the grammar must be clean ! *)
    let ext_g : ext_grammar = ext_grammar_of_grammar g
    and da = {pf=[];e=Nonterminal("dummy_axiom");sf=[]}
    (* all the computed rules *)
    and mem : (ext_element, ext_part list) Hashtbl.t = Hashtbl.create 100000
    and status : (ext_element, elem_status) Hashtbl.t = Hashtbl.create 100000 in

    let rec get_reachable_symbols (slist : ext_element list) : ext_element list =
        let get_reachable_symbols_once (slist : ext_element list) : ext_element list = slist |> List.rev_map (Hashtbl.find mem) |> List.concat |> List.cons slist |> List.concat |> List.sort_uniq compare |> List.filter is_ext_element_non_terminal in
            let slist2 = get_reachable_symbols_once slist in
            if List.compare_lengths slist slist2 = 0 then slist else (get_reachable_symbols [@tailcall]) slist2
    in

    let is_in_progress (e: ext_element) : bool = Hashtbl.find_opt status e = Some In_progress in

    let is_processed (e: ext_element) : bool = Hashtbl.find_opt status e = Some Processed in

    (* is the element useless, i.e. it is processed but is not bound to any rule *)
    let is_useless (e: ext_element) : bool = is_processed e && Hashtbl.find_opt mem e = Some [] in

    let set_useless (e: ext_element) : unit = Hashtbl.replace mem e [] in

    let set_color (c: string) (e: ext_element) : unit = Grammar_io.set_node_color_in_graph graph_channel e c in

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
    let reverse_ext_elem (sd: side) (e: ext_element) = match sd with
        | Right -> {pf=e.sf;e=e.e;sf=e.pf}
        | Left -> e in

    (* reverse a rule depending of the reverse variable *)
    let reverse_rule (sd: side) (r: ext_part) = match sd with
        | Right -> List.rev_map (reverse_ext_elem Right) r (* this rev_map is not an optimization but is mandatory *)
        | Left -> r in

    (* get the rules from mem, reversing them if necessary *)
    let get_rules (sd: side) (lhs: ext_element) =
        Hashtbl.find mem lhs |> List.rev_map (reverse_rule sd) in

    (* memorize rules, reversing them if necessary *)
    let add_rules_in_mem (sd: side) (lhs: ext_element) (rlist: ext_part list) : unit =
    let rev_new_rules = List.rev_map (reverse_rule sd) rlist in
    match rlist with
        | [] -> ()
        | _ -> let prev_rules = Hashtbl.find_opt mem lhs in
            if prev_rules = None then Hashtbl.add mem lhs rev_new_rules
            else Hashtbl.replace mem lhs (rev_new_rules@(Option.get prev_rules)) in

    (* memorize a rule, reversing them if necessary *)
    let add_rule_in_mem (sd: side) (lhs: ext_element) (r: ext_part) : unit = add_rules_in_mem sd lhs [r] in

    (* does e only derive epsilon ? *)
    let is_epsilon (e: ext_element) : bool = assert (is_in_progress e || is_processed e); List.for_all ((=) []) (Hashtbl.find mem e)

    (* can e derive epsilon ? *)
    and can_epsilon (e: ext_element) : bool = assert (is_in_progress e || is_processed e); List.exists ((=) []) (Hashtbl.find mem e) in

    (* create variants of a rule depending of whether the first element is or can be epsilon (and only the first element) *)
    let remove_epsilon : ext_part -> ext_part list = function
        | [] -> []
        | (t::_) as l when is_ext_element_terminal t -> [l] (* a terminal can't derive an epsilon *)
        | t::q when is_epsilon t -> [q] (* t always derive an epsilon: we remove it *)
        | (t::q) as l when can_epsilon t -> [l;q] (* t can derive an epsilon: we add a new rule without it *)
        | l -> [l] in

    (* call remove_epsilon with the rules and the reversed rules *)
    let rec remove_pf_sf_epsilon (rlist: ext_part list): ext_part list =
        let remove_pf_sf_epsilon_once (rlist: ext_part list): ext_part list =
            rlist |> List.filter (List.for_all (fun e -> not (is_useless e)))
                |> List.rev_map remove_epsilon |> List.flatten (* remove epsilon at beginning *)
                |> List.rev_map List.rev |> List.rev_map remove_epsilon |> List.flatten |> List.rev_map List.rev (* remove epsilon at end *)
                |> List.sort_uniq compare in
        let new_rules = remove_pf_sf_epsilon_once rlist in
        if List.compare_lengths new_rules rlist = 0 then new_rules
        else (remove_pf_sf_epsilon [@tailcall]) new_rules in

    (* we add the rules A -> A so A_[A|] can derive epsilon *)
    ext_g.ext_rules |> List.rev_map lhs_of_ext_rule |> List.sort_uniq compare |> List.iter (fun e -> add_rule_in_mem Left e [e]);

    (* we add the rules of the base grammar *)
    List.iter (fun r -> add_rule_in_mem Left r.ext_left_symbol r.ext_right_part) ext_g.ext_rules;

    (* the base grammar is usable *)
    ext_g.ext_rules |> List.rev_map (fun r -> r.ext_left_symbol::r.ext_right_part) |> List.flatten |> List.iter (fun e -> Hashtbl.add status e Processed);

    (* apply a left quotient of a single rule with a prefix that is a single element *)
    let quotient_by_one_element (sd: side) (pf: element) (new_lhs: ext_element) (r: ext_part) : ext_element option =
        assert (is_in_progress new_lhs);
        match r with
        | [] -> None
        (* A -> aBC with prefix = a *)
        | t::q when t.e=pf && is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); add_rule_in_mem sd new_lhs q; None
        (* A -> aBC with prefix != a *)
        | t::_ when is_ext_element_terminal t -> None
        (* A -> BC with prefix = B *)
        | t::q when t.e=pf && t.pf=[] -> let new_elem = {pf=[pf];e=t.e;sf=t.sf} in
            (* if B_{B|} is a dead end *)
            if is_useless new_elem then
                (add_rule_in_mem sd new_lhs q; None)
            (* if B can derive a leftmost B *)
            else
                (add_rules_in_mem sd new_lhs [q;new_elem::q]; Some new_elem)
        (* A -> B_{D|}C *)
        | t::q -> let new_elem = {pf=pf::t.pf;e=t.e;sf=t.sf} in
            if is_useless new_elem then None
            else (add_rule_in_mem sd new_lhs (new_elem::q); Some new_elem) in

    (* compute the rules of e with a new prefix (a single element) pf *)
    (* the new rules (left symbol: new_lhs) are based on the rules of the previous_lhs *)
    let quotient_by_one_element_mem (sd: side) (pf: element) (new_lhs: ext_element) (previous_lhs: ext_element) : (ext_element list) =
        assert (is_in_progress new_lhs && is_processed previous_lhs);
        let new_elems_with_seen = get_rules sd previous_lhs |> List.filter_map (quotient_by_one_element sd pf new_lhs) |> List.rev_map (reverse_ext_elem sd)  |> List.sort_uniq compare in
        new_elems_with_seen |> List.filter ((<>) new_lhs) |> List.iter (Grammar_io.add_edge_in_graph graph_channel "" new_lhs);
        List.filter (fun e -> not (is_processed e) && not (is_in_progress e)) new_elems_with_seen in

    (* compute the rules of a ext_element and do it recursively with every new symbol *)
    (* not completely tail-recursive *)
    let rec quotient_symbols (nb_iter: int) (elist: ext_element list) : int =
        match elist with
        | [] -> nb_iter
        | lhs::q ->
            (* Nothing to do *)
            (*print_endline ("Work on: "^(string_of_ext_element lhs));*)
            if lhs.pf = [] && lhs.sf = [] then begin
                assert (is_ext_element_terminal lhs || is_processed lhs);
                (quotient_symbols [@tailcall]) (nb_iter + 1) q
            end else if is_processed lhs then
                    ((*print_endline " Already known";*) (quotient_symbols [@tailcall]) (nb_iter + 1) q)
            else begin
                let (base_lhs,sd,qu) = match lhs.pf,lhs.sf with
                | [],[] -> assert false (* impossible : case verified just before *)
                | ((tpf::qpf) as pf),sf when List.compare_lengths pf sf >= 0 -> ({pf=qpf;e=lhs.e;sf=sf},Left,tpf)
                | _,[] -> assert false (* impossible because of the previous case *)
                | pf,(tsf::qsf) -> ({pf=pf;e=lhs.e;sf=qsf},Right,tsf) in
                if is_useless base_lhs then begin
                    (* we ignore this element *)
                    set_useless lhs;
                    set_color "grey" lhs;
                    Hashtbl.add status lhs Processed;
                    (*print_endline (" Useless because of "^(string_of_ext_element base_lhs));*)
                    (quotient_symbols [@tailcall]) (nb_iter + 1) q
                end else if is_processed base_lhs then begin
                    Grammar_io.add_edge_in_graph graph_channel "penwidth=3" lhs base_lhs;
                    (* we can compute the current symbol *)
                    (*print_endline "  Compute";*)
                    Hashtbl.add status lhs In_progress;
                    let new_elist = quotient_by_one_element_mem sd qu lhs base_lhs in
                    (* List.iter (fun p -> print_endline (string_of_ext_rule (lhs ---> p))) (Hashtbl.find mem lhs);*)
                    (* quick check : if a nonterminal is present in the rhs of all its rules, it is useless *)
                    if Hashtbl.find mem lhs |> List.for_all (List.exists ((=) lhs)) then set_useless lhs;
                    (* before we compute all the new elements, we verify if the current lhs is useful. Indeed, there could be an circular dependency structure *)
                    (*print_endline "New elements:";
                    List.iter (fun e -> print_endline (string_of_ext_element e)) new_elist;*)
                    let new_nb_iter = quotient_symbols nb_iter new_elist in
                    Hashtbl.add status lhs Processed;
                    assert (Hashtbl.find_opt mem lhs <> None);
                    if not (is_useless lhs) then begin
                        (* remove epsilon productions at the start and the end of the rhs *)
                        let rules = remove_pf_sf_epsilon (Hashtbl.find mem lhs) in
                        Hashtbl.replace mem lhs rules;
                        (*List.iter (fun p -> print_endline (string_of_ext_rule (lhs ---> p))) (Hashtbl.find mem lhs);*)
                    end;
                    (* we check again: if a nonterminal is present in the rhs of all its rules, it is useless *)
                    if Hashtbl.find mem lhs |> List.for_all (List.exists ((=) lhs)) then set_useless lhs;
                    (* if all rhs contain at least one useless element : lhs is useless *)
                    if not (is_useless lhs) && Hashtbl.find mem lhs |> List.for_all (List.exists (fun e -> is_useless e)) then set_useless lhs;
                    if is_useless lhs then (set_color "grey" lhs; (*print_endline ("Useless: "^(string_of_ext_element lhs))*));
                    (quotient_symbols [@tailcall]) (new_nb_iter + 1) q
                end else begin
                    (*print_endline "  Postpone";*)
                    (* we can't compute the current symbol so we keep it on the list *)
                    (quotient_symbols [@tailcall]) (nb_iter + 1) (base_lhs::elist)
                end
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
            ignore (quotient_symbols 0 [e]);
(*            let nb_iter = quotient_symbols 0 [e] in
            if verbose then print_endline ("Nb iter: "^(string_of_int nb_iter)^", memory size: "^(string_of_int (Hashtbl.length mem)));*)
            grammar_of_mem e
        end
