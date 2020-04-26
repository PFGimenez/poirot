open Grammar

type side = Left | Right

(*let best_rule : (ext_element, ext_part) Hashtbl.t = Hashtbl.create 10000*)
let words : (ext_element, part) Hashtbl.t = Hashtbl.create 10000

(* all the computed rules *)
let mem : (ext_element, ext_part list) Hashtbl.t = Hashtbl.create 10000

let compare_ext_rule (r1: ext_rule) (r2: ext_rule) : int =
    let diff = List.compare_lengths (List.filter is_ext_element_non_terminal r1.ext_right_part) (List.filter is_ext_element_non_terminal r2.ext_right_part) in
    if diff <> 0 then diff
    else begin
        let diff2 = List.compare_lengths r1.ext_right_part r2.ext_right_part in
        if diff2 <> 0 then diff2
        else (Hashtbl.hash r2.ext_right_part) - (Hashtbl.hash r1.ext_right_part) (* to be deterministic we want to compare any couple *)
    end

let memorize_best_rule (r: ext_rule) : unit =
    if not (Hashtbl.mem words r.ext_left_symbol) then begin
(*        Hashtbl.add best_rule r.ext_left_symbol r.ext_right_part;*)
        (* the word is reserved so we can add it easily to the reversed prefix *)
        r.ext_right_part |> List.map (fun e -> if is_ext_element_terminal e then [e.e] else Hashtbl.find words e) |> List.concat |> (fun p -> print_endline ((string_of_ext_element r.ext_left_symbol)^": "^(string_of_word p)); Hashtbl.add words r.ext_left_symbol p)
        (* all the prerequisite for computing the word are already computed ! *)
    end

let quotient_mem (g: grammar) (goal_elem: element option) (values: (element, string) Hashtbl.t option) (graph_channel: out_channel option) (*(verbose: bool)*): ext_element -> ext_grammar * (part option)  =
    (* the grammar must be clean ! *)
    let ext_g : ext_grammar = Clean.clean (ext_grammar_of_grammar g) in
    let goal = Option.map ext_element_of_element goal_elem in

    let rec get_reachable_symbols (slist : ext_element list) : ext_element list =
        let get_reachable_symbols_once (slist : ext_element list) : ext_element list = slist |> List.rev_map (Hashtbl.find mem) |> List.concat |> List.cons slist |> List.concat |> List.sort_uniq compare |> List.filter is_ext_element_non_terminal in
            let slist2 = get_reachable_symbols_once slist in
            if List.compare_lengths slist slist2 = 0 then slist else (get_reachable_symbols [@tailcall]) slist2 in

    let is_processed (e: ext_element) : bool = Hashtbl.mem mem e in

    (* is the element useless, i.e. it is processed but is not bound to any rule *)
    let is_useless (e: ext_element) : bool = Hashtbl.find_opt mem e = Some [] in

    let set_useless (e: ext_element) : unit = Hashtbl.replace mem e [] in

    let initialize_mem (e: ext_element) : unit = assert (not (is_processed e)); Hashtbl.replace mem e [] in

    let set_color (c: string) (e: ext_element) : unit = Grammar_io.set_node_color_in_graph graph_channel e c in

    (* based on level decomposition *)
    let update_words_and_useless (original_rules: ext_rule list) : ext_element list =
        let rec update_words_and_useless_aux (reached_sym: ext_element list) (original_rules: ext_rule list) : ext_element list =
            let usable_rules = List.filter (fun r -> List.for_all (fun s -> is_ext_element_terminal s || Hashtbl.mem words s) r.ext_right_part) original_rules in
            if usable_rules = [] then reached_sym (* the algorithm is done *)
            else begin
                List.iter memorize_best_rule (List.sort compare_ext_rule usable_rules);
                (update_words_and_useless_aux [@tailcall]) ((List.map lhs_of_ext_rule usable_rules)@reached_sym) (List.filter (fun r -> not (Hashtbl.mem words r.ext_left_symbol)) original_rules)
            end in
        let reached = update_words_and_useless_aux [] original_rules in
        (* unreachable symbols are useless *)
        original_rules |> List.map lhs_of_ext_rule |> List.filter (fun e -> not (List.mem e reached)) |> List.map (fun e -> print_endline ("Useless: "^(string_of_ext_element e));e) |> List.iter set_useless;
        reached in

    (* Get the set of rules of a list of ext_element. No reverse *)
    let get_all_rules (elist: ext_element list) : ext_rule list =
        elist |> List.map (fun e -> List.map (fun rhs -> e ---> rhs) (Hashtbl.find mem e)) |> List.concat in

    (* construct a grammar, given an axiom, from the memory *)
    let grammar_of_mem (axiom : ext_element) : ext_grammar =
        assert (is_ext_element_non_terminal axiom);
        let rules_of_element : ext_element -> ext_rule list = fun e -> (Hashtbl.find mem e |> List.rev_map ((--->) e)) in
        let rules = get_reachable_symbols [axiom] |> List.rev_map rules_of_element |> List.concat in
        axiom @@@ rules in

    (* reverse an extended element depending of the reverse variable *)
    let reverse_ext_elem (sd: side) (e: ext_element) : ext_element = match sd with
        | Right -> {pf=e.sf;e=e.e;sf=e.pf}
        | Left -> e in

    (* reverse a rule depending of the reverse variable *)
    let reverse_rule (sd: side) (r: ext_part) : ext_part = match sd with
        | Right -> List.rev_map (reverse_ext_elem Right) r (* this rev_map is not an optimization but is mandatory *)
        | Left -> r in

    (* get the rules from mem, reversing them if necessary *)
    let get_rules (sd: side) (lhs: ext_element) : ext_part list =
        Hashtbl.find mem lhs |> List.rev_map (reverse_rule sd) in

    (* memorize rules, reversing them if necessary *)
    let add_rules_in_mem (sd: side) (lhs: ext_element) (rlist: ext_part list) : unit =
        print_endline ("Add "^(string_of_ext_element lhs)^(if sd=Right then "right" else "left"));
        let rev_new_rules = List.rev_map (reverse_rule sd) rlist in
        List.iter (fun p -> print_endline (string_of_ext_rule (lhs ---> p))) rev_new_rules;
        match rlist with
            | [] -> ()
            | _ -> let prev_rules = Hashtbl.find_opt mem lhs in
                if prev_rules = None then Hashtbl.add mem lhs rev_new_rules
                else Hashtbl.replace mem lhs (rev_new_rules@(Option.get prev_rules)) in

    (* memorize a rule, reversing them if necessary *)
    let add_rule_in_mem (sd: side) (lhs: ext_element) (r: ext_part) : unit = add_rules_in_mem sd lhs [r] in

    (* does e only derive epsilon ? *)
    let is_epsilon (e: ext_element) : bool = List.for_all ((=) []) (Hashtbl.find mem e)

    (* can e derive epsilon ? *)
    and can_epsilon (e: ext_element) : bool = List.exists ((=) []) (Hashtbl.find mem e) in

    let replace_rules_in_mem (rlist: ext_rule list) : unit =
        (* remove the previous rhs *)
        List.iter (fun r -> Hashtbl.replace mem r.ext_left_symbol []) rlist;
        (* add the new rules *)
        List.iter (fun r -> add_rule_in_mem Left r.ext_left_symbol r.ext_right_part) rlist in

    (* create variants of a rule depending of whether the first element is or can be epsilon (and only the first element) *)
    let remove_epsilon (r: ext_rule) : ext_rule list = match r.ext_right_part with
        | [] -> [r.ext_left_symbol ---> []]
        | (t::_) as l when is_ext_element_terminal t -> [r.ext_left_symbol ---> l] (* a terminal can't derive an epsilon *)
        | t::q when is_epsilon t -> [r.ext_left_symbol ---> q] (* t always derive an epsilon: we remove it *)
        | (t::q) as l when can_epsilon t -> [r.ext_left_symbol ---> l;r.ext_left_symbol ---> q] (* t can derive an epsilon: we add a new rule without it *)
        | l -> [r.ext_left_symbol ---> l] in

    (* call remove_epsilon with the rules and the reversed rules *)
    (* doesn't need to reverse as it applies to both sides *)
    let rec remove_pf_sf_epsilon (rlist: ext_rule list): ext_rule list =
        let remove_pf_sf_epsilon_once (rlist: ext_rule list): ext_rule list =
            rlist |> List.rev_map remove_epsilon |> List.flatten (* remove epsilon at beginning *)
                |> List.rev_map (fun r -> r.ext_left_symbol ---> (List.rev r.ext_right_part)) |> List.rev_map remove_epsilon |> List.flatten |> List.rev_map (fun r -> r.ext_left_symbol ---> (List.rev r.ext_right_part)) (* remove epsilon at end *)
                |> List.sort_uniq compare in
        replace_rules_in_mem rlist; (* replace in memory before the epsilon removing *)
        print_endline "Remove epsilon";
        print_endline (string_of_ext_rules rlist);
        let new_rules = remove_pf_sf_epsilon_once rlist in
(*        print_endline "New rules";
        print_endline (string_of_ext_rules new_rules);*)
        print_endline "New rules";
        print_endline (string_of_ext_rules new_rules);
        if List.compare_lengths new_rules rlist > 0 then (remove_pf_sf_epsilon [@tailcall]) new_rules
        else new_rules in

    (* we add the rules A -> A so A_[A|] can derive epsilon *)
    (*ext_g.ext_rules |> List.rev_map lhs_of_ext_rule |> List.sort_uniq compare |> List.iter (fun e -> add_rule_in_mem Left e [e]);*)

    (* we add the rules of the base grammar *)
(*    List.iter (fun r -> add_rule_in_mem Left r.ext_left_symbol r.ext_right_part) ext_g.ext_rules;*)
    replace_rules_in_mem ext_g.ext_rules;
    ignore (update_words_and_useless ext_g.ext_rules);
    (* the base grammar is usable *)
(*    ext_g.ext_rules |> List.rev_map (fun r -> r.ext_left_symbol::r.ext_right_part) |> List.flatten |> List.iter (fun e -> Hashtbl.add status e Processed);*)

    (* apply a left quotient of a single rule with a prefix that is a single element *)
    let quotient_by_one_element (sd: side) (pf: element) (new_lhs: ext_element) (r: ext_part) : ext_element option =
        assert (is_non_terminal new_lhs.e);
        if new_lhs.e = pf && (new_lhs.pf@new_lhs.sf) = [pf] then
            add_rule_in_mem sd new_lhs [];
        match r with
        | [] -> None
        (* A -> aBC with prefix = a *)
        | t::q when t.e=pf && is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); add_rule_in_mem sd new_lhs q; None
        (* A -> aBC with prefix != a *)
        | t::_ when is_ext_element_terminal t -> None
        (* A -> B_{D|}C *)
        | t::q -> let new_elem = {pf=pf::t.pf;e=t.e;sf=t.sf} in
            add_rule_in_mem sd new_lhs (new_elem::q); Some t in

    (* compute new rules with a new prefix *)
    (* tail recursive *)
    let rec quotient_by_one_element_mem (sd: side) (prefix: element) (lhs_list: ext_element list) (all_new_elems: ext_element list) : (ext_element list) =
        match lhs_list with
        | [] -> all_new_elems
        | prev_lhs::q ->
            begin
                let new_lhs = match sd with
                    | Right -> {pf=prev_lhs.pf;e=prev_lhs.e;sf=prefix::prev_lhs.sf}
                    | Left -> {pf=prefix::prev_lhs.pf;e=prev_lhs.e;sf=prev_lhs.sf} in
                print_endline ("New: "^(string_of_ext_element new_lhs));
                if not (is_processed new_lhs) then begin
                    initialize_mem new_lhs;
                    let prev_lhs_to_quotient = get_rules sd prev_lhs |> List.filter_map (quotient_by_one_element sd prefix new_lhs) |> List.rev_map (reverse_ext_elem sd) |> List.sort_uniq compare in
                    (* all new lhs should have been already processed *)
                    assert (List.for_all is_processed prev_lhs_to_quotient);
                    (*List.iter (fun e -> print_endline (string_of_ext_element e)) prev_lhs_to_quotient;*)
                    if Option.is_some graph_channel then begin
                        let new_elems = List.map (fun {pf;e;sf} -> {pf=prefix::pf;e=e;sf=sf}) prev_lhs_to_quotient in
                        List.iter (fun e -> Grammar_io.add_edge_in_graph graph_channel "" new_lhs (reverse_ext_elem sd e)) new_elems;
                    end;
                    (*List.iter (fun p -> print_endline (string_of_ext_rule (new_lhs ---> p))) (Hashtbl.find mem new_lhs);*)
                    (quotient_by_one_element_mem [@tailcall]) sd prefix (prev_lhs_to_quotient@q) (new_lhs::all_new_elems)
                end else
                    (print_endline "Already processed!";
                    print_endline (string_of_ext_rules (get_all_rules [new_lhs]));
                    (quotient_by_one_element_mem [@tailcall]) sd prefix q all_new_elems);
            end in

    (* compute the rules of a ext_element and do it recursively with all the prefix/suffix *)
    (* tail-recursive *)
    let rec quotient_symbols (nb_iter: int) (elist: ext_element list) : int =
        match elist with
        | [] -> print_endline "quotient finished"; nb_iter
        | lhs::q ->
            (* Nothing to do *)
            print_endline ("Work on: "^(string_of_ext_element lhs));
            if lhs.pf = [] && lhs.sf = [] then begin
                assert (is_ext_element_terminal lhs || is_processed lhs);
                (quotient_symbols [@tailcall]) (nb_iter + 1) q
            end else if is_processed lhs then
                    (print_endline " Already known"; (quotient_symbols [@tailcall]) (nb_iter + 1) q)
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
                    print_endline (" Useless because of "^(string_of_ext_element base_lhs));
                    (quotient_symbols [@tailcall]) (nb_iter + 1) q
                end else if is_processed base_lhs then begin
                    print_endline ("ICI "^(string_of_ext_element lhs)^", prefix: "^(string_of_element qu));
                    print_endline ("Base: "^(string_of_ext_rules (get_all_rules [base_lhs])));
                    Grammar_io.add_edge_in_graph graph_channel "penwidth=3" lhs base_lhs;
                    (* we can compute the current symbol *)
                    print_endline "  Compute";
                    let new_elist = quotient_by_one_element_mem sd qu [base_lhs] [] in

                    (* TODO: retirer ceux qui ne produisent rien (symboles inutiles) avec une décomposition en niveau. Il suffit de le faire avec les nouvelles règles car tous les autres symboles déjà connus sont utiles *)


                    print_endline "Before epsilon-free";
                    List.iter (fun new_lhs -> List.iter (fun p -> print_endline (string_of_ext_rule (new_lhs ---> p))) (Hashtbl.find mem new_lhs)) new_elist;
                    (* make the grammar epsilon-free. Only the new rules are concerned *)
                    new_elist |> get_all_rules (* get the new elements *)
(*                        |> List.map (fun r -> print_endline ("A: "^(string_of_ext_rule r)); r)*)
                        |> update_words_and_useless
                        |> get_all_rules
(*                        |> List.map (fun r -> print_endline ("B: "^(string_of_ext_rule r)); r)*)
                        |> List.filter (fun r -> List.for_all (fun e -> not (is_useless e)) r.ext_right_part) (* remove rules with useless symbols *)
                        |> remove_pf_sf_epsilon (* make epsilon-free *)
                        |> replace_rules_in_mem;

                    print_endline "Finally";
                    List.iter (fun new_lhs -> List.iter (fun p -> print_endline (string_of_ext_rule (new_lhs ---> p))) (Hashtbl.find mem new_lhs)) new_elist;
                    print_endline "End";

                    if Option.is_some graph_channel then List.iter (fun e -> if is_useless e then (set_color "grey" e)) new_elist;
                    (quotient_symbols [@tailcall]) (nb_iter + 1) q
                end else begin
                    print_endline "  Postpone";
                    (* we can't compute the current symbol so we keep it on the list *)
                    (quotient_symbols [@tailcall]) (nb_iter + 1) (base_lhs::elist)
                end
            end in

    (* tail-recursive *)
    let rec fuzzer_minimize (goal_rules: ext_rule list) (word_prefix: element list) (sentential_suffix: ext_element list) : element list =
        match sentential_suffix, goal_rules with
        (* verify the sentential form length *)
        | l,_ when List.compare_length_with l 1000000 > 0 -> failwith "Fuzzing failure: word too long"
        (* no more suffix. All goal rules should have been used. *)
        | [],_ -> assert (goal_rules = []); List.rev word_prefix
        (* the suffix starts with a terminal *)
        | t::q,_ when is_ext_element_terminal t -> (fuzzer_minimize [@tailcall]) goal_rules (t.e::word_prefix) q
        (* the suffix starts with a goal rule *)
        | t::q,r::q2 when r.ext_left_symbol=t -> (fuzzer_minimize [@tailcall]) q2 word_prefix (r.ext_right_part@q)
        (* do we have a specific request for the start of the suffix ? *)
        | {pf=[];e=t;sf=[]}::q,_ when Option.map (fun v -> Hashtbl.mem v t) values = Some true ->
                (fuzzer_minimize [@tailcall]) goal_rules ((Terminal (Hashtbl.find (Option.get values) t))::word_prefix) q
        (* do we have a words saved for the start of the suffix ? *)
        | t::q,_ when Hashtbl.mem words t -> (fuzzer_minimize [@tailcall]) goal_rules ((List.rev (Hashtbl.find words t))@word_prefix) q
        (* apply the best rule *)
        | t::q,_ -> failwith ("No words?? "^(string_of_ext_element t)) (*;(fuzzer_minimize [@tailcall]) goal_rules word_prefix ((Hashtbl.find best_rule t)@q)*) in

    let rec has_new (seen: ext_element list) (p: ext_element list) : bool = match p with
        | [] -> false
        | t::_ when not (List.mem t seen) -> true
        | _::q -> (has_new [@tailcall]) seen q in

    (* build all the possible one-step derivation of part p in the grammar g *)
    let build_derivation (p: ext_part) : (ext_rule * ext_part) list =
        (* tail-recursive *)
        let rec build_derivation_aux (sofar: ext_part) (acc: (ext_rule * ext_part) list) (p: ext_part) : (ext_rule * ext_part) list = match p with
            | [] -> acc
            | t::q when is_ext_element_terminal t -> (build_derivation_aux [@tailcall]) (t::sofar) acc q
            | t::q-> let new_parts = Hashtbl.find mem t |> List.rev_map (fun rhs -> (t--->rhs),(List.rev sofar)@rhs@q) in
(*                    let new_parts = g.rules |> List.filter (fun r -> r.left_symbol = t) |> List.rev_map (fun r -> r,(List.rev sofar)@r.right_part@q) in*)
                (build_derivation_aux [@tailcall]) (t::sofar) (new_parts@acc) q in
        build_derivation_aux [] [] p in

    (* get a derivation of ext_element with the "biggest" rule *)
    let get_first_derivation (e: ext_element) : ext_element list =
        (List.hd (List.sort compare_ext_rule (get_all_rules [e]))).ext_right_part in

    let rec find_path_to_goal_aux (seen: ext_element list) (queue : (ext_part * ext_rule list) list) : ext_rule list =
        assert (goal <> None);
        match queue with
        | [] -> assert false (* we know there is a path since the goal is reachable ! *)
        | (form,path)::_ when List.mem (Option.get goal) form -> List.rev path
        | (form,_)::q when not (has_new seen form) -> (find_path_to_goal_aux [@tailcall]) seen q
        | (form,path)::q -> let new_items = List.map (fun (r,p) -> (p,r::path)) (build_derivation form) in
            (find_path_to_goal_aux [@tailcall]) (List.sort_uniq compare (form@seen)) (q@new_items) in

    let find_path_to_goal (axiom : ext_element) : ext_rule list =
        find_path_to_goal_aux [] [([axiom],[])]
    in

    let seen : (ext_element,unit) Hashtbl.t = Hashtbl.create 500 in

    (* is the element s reachable in the grammar g from the element start ? *)
    let is_reachable (s: ext_element) (start: ext_element) : bool =
        let rec is_reachable_aux (queue: ext_element list) : bool = match queue with
            | [] -> false
            | t::_ when t=s -> true
            | t::q when (is_ext_element_terminal t || Hashtbl.mem seen t) -> (is_reachable_aux [@tailcall]) q
            | t::q ->
                    let new_elems = List.concat (Hashtbl.find mem t) in
                    Hashtbl.add seen t ();
                    (is_reachable_aux [@tailcall]) (q@new_elems) in
        Hashtbl.clear seen;
        is_reachable_aux [start] in

    fun (e: ext_element) : (ext_grammar * (part option)) ->
        (* the case when e is terminal is handled separately *)
        if is_ext_element_terminal e then begin
            (* the dummy axiom is only used when the regular axiom is terminal *)
            let da = {pf=[];e=Nonterminal("dummy_axiom");sf=[]} in
            (* derive epsilon *)
            if (e.pf=[e.e] && e.sf=[]) || (e.pf=[] && e.sf=[e.e]) then
                (da@@@[da--->[]], Some [])
            (* derive this terminal *)
            else if e.pf=[] && e.sf=[] then
                (da@@@[da--->[e]], Some [e.e])
            (* empty language *)
            else
                (da@@@[], None)
        end else begin
            let nb_iter = quotient_symbols 0 [e] in
            Log.L.info (fun m -> m "Nb iter: %d, memory size: %d" nb_iter (Hashtbl.length mem));
            let injg = grammar_of_mem e in
            print_endline "Fuzzing with grammar:";
            print_endline (string_of_ext_grammar injg);

            if is_useless e then (injg,None)
            else
                if Option.is_some goal && is_reachable (Option.get goal) e then begin
                    Log.L.debug (fun m -> m "Fuzzing with goal");
                    (injg, Some (fuzzer_minimize (find_path_to_goal e) [] [e]))
                end else begin
                    Log.L.debug (fun m -> m "Fuzzing");
                    (injg, Some (fuzzer_minimize [] [] (get_first_derivation e)))
                end
        end
