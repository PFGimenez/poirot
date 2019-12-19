open Base

(* get the first symbol of the rhs of a rule *)
let extract_left_symbol (r : rule) : element option = match r.right_part with
    | [] -> None
    | t::_ -> Some(t)

(* TODO: faire en sorte que le symbol en lui-même n'apparaisse pas dans la liste (sauf s'il est accessible récursivement *)

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
let get_generative_rules (g: grammar) (e: element) : ext_rule list =
    List.filter_map (fun r -> if List.mem e r.right_part then Some(ext_rule_of_rule r) else None) g.rules

(* get the list of rules with some left-hand side *)
(* TODO: mettre dans une hashtable une fois pour toute *)
let get_rules (rlist: ext_rule list) (e: ext_element) : ext_rule list =
    List.filter_map (fun r -> if r.ext_left_symbol = e then Some(r) else None) rlist

let quotient_mem (g: grammar) =
    (* all the computed rules *)
    let mem : (ext_rule list) ref = ref (List.map ext_rule_of_rule g.rules)
    (* has an ext_element been computed before ? *)
    and seen : (ext_element, bool) Hashtbl.t = Hashtbl.create 100
    and all_sym : (element list) = get_all_symbols g in

    (* the base grammar is already computed *)
    List.iter (fun e -> Hashtbl.add seen (ext_element_of_element e) true) all_sym;

    let all_non_terminal : (element list) = List.filter (is_non_terminal) all_sym
    and create_useless (e: element) : ext_element list =
        let useful = left_symbol_list g e in
        let check_useless (p : element) : ext_element option =
            if not (List.mem p useful) then Some({pf=[p];e=e;sf=[]})
            else None
        in
        List.filter_map check_useless all_sym
    in

    (* we don't need to compute the useless of terminal since their "quotientability" is trivial *)
    let useless : (ext_element, bool) Hashtbl.t = Hashtbl.create 100 in
    List.iter (fun e -> Hashtbl.add useless e true) (List.flatten (List.map create_useless all_non_terminal));

    let update_useless (e: ext_element) =
        let useful = Clean.get_useful_symbols (e@@@(!mem)) in
        let new_useless = List.filter (fun s -> is_ext_element_non_terminal s && Hashtbl.mem seen e && not (Hashtbl.mem useless e) && not (List.mem s useful)) (get_all_symbols_ext_rules !mem) in
        print_endline "New useless:";
        List.iter (fun e -> print_endline (string_of_ext_element e)) new_useless;
        List.iter (fun e -> Hashtbl.replace useless e true) new_useless
    in

    (* apply a quotient of a single rule with a prefix that is a single element *)
    let quotient_by_one_element (pf: element) (new_lhs: ext_element) (r: ext_rule) : (ext_element list * bool) =
        match r.ext_right_part with
        | [] -> [],false
        (* A -> aBC with prefix = a *)
        | t::q when t.e=pf && is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); mem:=(new_lhs ---> q)::!mem; [],true
        (* A -> aBC with prefix != a *)
        | t::q when is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); [],false
        (* A -> BC with prefix = B *)
        | t::q when t.e=pf && t.pf=[] -> let new_elem = {pf=[pf];e=t.e;sf=t.sf} in
            (* if B_{B|} is a dead end *)
            if Hashtbl.mem useless new_elem then
                (mem:=(new_lhs ---> q)::!mem; [],true)
            (* if B can derive a leftmost B *)
            else
                (mem:=(new_lhs ---> q)::(new_lhs ---> (new_elem::q))::!mem; [new_elem],true)
        (* A -> B_{D|}C *)
        | t::q -> let new_elem = {pf=pf::t.pf;e=t.e;sf=t.sf} in
            if Hashtbl.mem useless new_elem then [],false
            else (mem:=(new_lhs ---> (new_elem::q))::!mem; [new_elem],true)
    in

    (* compute the rules of e with a new prefix (a single element) pf *)
    (* the new rules (left symbol: new_lhs) are based on the rules of the previous_lhs *)
    let quotient_by_one_element_mem (pf: element) (new_lhs: ext_element) (previous_lhs: ext_element) : (ext_element list) =
        if Hashtbl.mem seen new_lhs then
            []
        else begin
            Hashtbl.add seen new_lhs true;
            assert (Hashtbl.mem seen previous_lhs);
            let (new_new_sym,useful) = List.split (List.map (quotient_by_one_element pf new_lhs) (get_rules !mem previous_lhs)) in
            (* if List.for_all not useful then (* TODO: corriger *)
                (print_endline "Useless !"; Hashtbl.add useless new_lhs true); *)
            List.sort_uniq compare (List.filter (fun e -> not (Hashtbl.mem seen e)) (List.flatten new_new_sym))
        end
    in

    (* compute the rules of a ext_element and do it recursively with every new symbol *)
    let rec quotient_symbols (elist: ext_element list) : ext_rule list =
        match elist with
        | [] -> !mem
        | lhs::q -> print_endline ("Current LHS: "^(string_of_ext_element lhs)^"\nCurrent elist:"); List.iter (fun e -> print_endline (string_of_ext_element e)) elist; (* print_endline ("Current rules:\n"^(string_of_ext_rules !mem)) ;*)
                match lhs.pf with
            | [] -> quotient_symbols q
            | tpf::qpf -> let base_lhs = {pf=qpf;e=lhs.e;sf=lhs.sf} in
                    print_endline ("Base LHS: "^(string_of_ext_element base_lhs));
                    if Hashtbl.mem useless lhs || Hashtbl.mem useless base_lhs || Hashtbl.mem seen lhs then
                        (* we ignore this element *)
                        (quotient_symbols q)
                    else if qpf = [] || Hashtbl.mem seen base_lhs then begin
                        (* we can compute the current symbol *)
                        (* update_useless lhs; TODO: corriger car il considère useless des non-terminaux dont  *)
                        let new_elist = quotient_by_one_element_mem tpf lhs base_lhs in
                        (quotient_symbols [@tailcall]) (new_elist@q) end
                    else begin
                        (* we can't compute the current symbol so we keep it on the list *)
                        ((quotient_symbols [@tailcall]) (base_lhs::elist)) end
    in
    fun (e: ext_element) : ext_grammar -> Clean.clean (e@@@(quotient_symbols [e]))
