open Grammar

type side = Left | Right

type t = {  words : (ext_element, part) Hashtbl.t;
            mem : (ext_element, ext_part list) Hashtbl.t;
            dict: (element,string) Hashtbl.t option;
            cant_reach_goal: (element,unit) Hashtbl.t;
            forbidden: char list;
            mutable call_time: float;
            mutable fuzzer_time: float;
            graph_channel : out_channel option}

(* compare the ext_rules. Sorting with this comparison puts the "smallest" rules at the beginning. *)
let compare_ext_rule (r1: ext_rule) (r2: ext_rule) : int =
    let diff = List.compare_lengths (List.filter is_ext_element_non_terminal r1.ext_right_part) (List.filter is_ext_element_non_terminal r2.ext_right_part) in
    if diff <> 0 then diff
    else begin
        let diff2 = List.compare_lengths r1.ext_right_part r2.ext_right_part in
        if diff2 <> 0 then diff2
        else (Hashtbl.hash r2.ext_right_part) - (Hashtbl.hash r1.ext_right_part) (* to be deterministic we want to compare any couple *)
    end

let rec get_reachable_symbols (quo: t) (slist : ext_element list) : ext_element list =
    let get_reachable_symbols_once (slist : ext_element list) : ext_element list = slist |> List.rev_map (Hashtbl.find quo.mem) |> List.concat |> List.cons slist |> List.concat |> List.sort_uniq compare |> List.filter is_ext_element_non_terminal in
    let slist2 = get_reachable_symbols_once slist in
    if List.compare_lengths slist slist2 = 0 then slist else (get_reachable_symbols [@tailcall]) quo slist2

let is_processed (quo: t) (e: ext_element) : bool = Hashtbl.mem quo.mem e

(* is the element useless, i.e. it is processed but is not bound to any rule *)
let is_useless (quo: t) (e: ext_element) : bool = Hashtbl.find_opt quo.mem e = Some []

let set_useless (quo: t) (e: ext_element) : unit = Hashtbl.replace quo.mem e []

let initialize_mem (quo: t) (e: ext_element) : unit = assert (not (is_processed quo e)); Hashtbl.replace quo.mem e []

let set_color (quo: t) (c: string) (e: ext_element) : unit = Grammar_io.set_node_color_in_graph quo.graph_channel e c

let is_allowed (quo: t) (e: ext_element): bool = match e with
    | {e=Terminal s;_} -> not (List.exists (String.contains s) quo.forbidden)
    | _ -> true

let update_words (quo: t) (r: ext_rule) : unit =
    let lhs = r.ext_left_symbol in
    if not (Hashtbl.mem quo.words lhs) then begin
        let w = match quo.dict with
            | Some hashtbl when lhs.pf=[] && lhs.sf=[] && Hashtbl.mem hashtbl lhs.e -> [Terminal (Hashtbl.find hashtbl lhs.e)]
            | _ -> r.ext_right_part |> List.map (fun e -> if is_ext_element_terminal e then [e.e] else Hashtbl.find quo.words e) |> List.concat in (* all the prerequisite for computing the word are already computed ! *)
        Hashtbl.add quo.words lhs w
    end


(* based on level decomposition *)
let update_words_and_useless (quo: t) (original_rules: ext_rule list) : ext_element list =
    let rec update_words_and_useless_aux (reached_sym: ext_element list) (original_rules: ext_rule list) : ext_element list =
        let usable_rules = List.filter (fun r -> List.for_all (fun s -> (is_ext_element_terminal s || Hashtbl.mem quo.words s) && is_allowed quo s) r.ext_right_part) original_rules in
        if usable_rules = [] then reached_sym (* the algorithm is done *)
        else begin
            List.iter (update_words quo) (List.sort compare_ext_rule usable_rules);
            (update_words_and_useless_aux [@tailcall]) ((List.rev_map lhs_of_ext_rule usable_rules)@reached_sym) (List.filter (fun r -> not (Hashtbl.mem quo.words r.ext_left_symbol)) original_rules)
        end in
    let reached = update_words_and_useless_aux [] original_rules in
    (* unreachable symbols are useless *)
    original_rules |> List.rev_map lhs_of_ext_rule |> List.filter (fun e -> not (List.mem e reached)) |> (*List.map (fun e -> print_endline ("Useless: "^(string_of_ext_element e));e) |>*) List.iter (set_useless quo);
    reached

(* Get the set of rules of a list of ext_element. No reverse *)
let get_all_rules (quo: t) (elist: ext_element list) : ext_rule list =
    elist |> List.rev_map (fun e -> List.rev_map (fun rhs -> e ---> rhs) (Hashtbl.find quo.mem e)) |> List.concat

(* construct a grammar, given an axiom, from the memory *)
let grammar_of_mem (quo: t) (axiom : ext_element) : ext_grammar =
    assert (is_ext_element_non_terminal axiom);
    let rules_of_element : ext_element -> ext_rule list = fun e -> (Hashtbl.find quo.mem e |> List.rev_map ((--->) e)) in
    let rules = get_reachable_symbols quo [axiom] |> List.rev_map rules_of_element |> List.concat in
    axiom @@@ rules

(* reverse an extended element depending of the reverse variable *)
let reverse_ext_elem (sd: side) (e: ext_element) : ext_element = match sd with
    | Right -> {pf=e.sf;e=e.e;sf=e.pf}
    | Left -> e

(* reverse a rule depending of the reverse variable *)
let reverse_rule (sd: side) (r: ext_part) : ext_part = match sd with
    | Right -> List.rev_map (reverse_ext_elem Right) r (* this rev_map is not an optimization but is mandatory *)
    | Left -> r

(* get the rules from mem, reversing them if necessary *)
let get_rules (quo: t) (sd: side) (lhs: ext_element) : ext_part list =
    Hashtbl.find quo.mem lhs |> List.rev_map (reverse_rule sd)

(* memorize rules, reversing them if necessary *)
let add_rules_in_mem (quo: t) (sd: side) (lhs: ext_element) (rlist: ext_part list) : unit =
    (* print_endline ("Add "^(string_of_ext_element lhs)^(if sd=Right then "right" else "left")); *)
    let rev_new_rules = List.rev_map (reverse_rule sd) rlist in
    (* List.iter (fun p -> print_endline (string_of_ext_rule (lhs ---> p))) rev_new_rules; *)
    match rlist with
        | [] -> ()
        | _ -> let prev_rules = Hashtbl.find_opt quo.mem lhs in
            if prev_rules = None then Hashtbl.add quo.mem lhs rev_new_rules
            else Hashtbl.replace quo.mem lhs (rev_new_rules@(Option.get prev_rules))

(* memorize a rule, reversing them if necessary *)
let add_rule_in_mem (quo: t) (sd: side) (lhs: ext_element) (r: ext_part) : unit = add_rules_in_mem quo sd lhs [r]

(* does e only derive epsilon ? *)
let is_epsilon (quo: t) (e: ext_element) : bool = List.for_all ((=) []) (Hashtbl.find quo.mem e)

(* can e derive epsilon ? *)
let can_epsilon (quo: t) (e: ext_element) : bool = List.exists ((=) []) (Hashtbl.find quo.mem e)

let replace_rules_in_mem (quo: t) (rlist: ext_rule list) : unit =
    (* remove the previous rhs *)
    List.iter (fun r -> Hashtbl.replace quo.mem r.ext_left_symbol []) rlist;
    (* add the new rules *)
    List.iter (fun r -> add_rule_in_mem quo Left r.ext_left_symbol r.ext_right_part) rlist

(* create variants of a rule depending of whether the first element is or can be epsilon (and only the first element) *)
let remove_epsilon (quo: t) (r: ext_rule) : ext_rule list = match r.ext_right_part with
    | [] -> [r.ext_left_symbol ---> []]
    | (t::_) as l when is_ext_element_terminal t -> [r.ext_left_symbol ---> l] (* a terminal can't derive an epsilon *)
    | t::q when is_epsilon quo t -> [r.ext_left_symbol ---> q] (* t always derive an epsilon: we remove it *)
    | (t::q) as l when can_epsilon quo t -> [r.ext_left_symbol ---> l;r.ext_left_symbol ---> q] (* t can derive an epsilon: we add a new rule without it *)
    | l -> [r.ext_left_symbol ---> l]

(* call remove_epsilon with the rules and the reversed rules *)
(* doesn't need to reverse as it applies to both sides *)
let rec remove_pf_sf_epsilon (quo: t) (sd: side) (rlist: ext_rule list): ext_rule list =
    (* the rules must be sorted to compare them to the new rules after epsilon-free *)
    assert ((List.sort_uniq compare rlist) = rlist);
    let remove_sf_epsilon_once (rlist: ext_rule list): ext_rule list =
        rlist
                |> List.rev_map (fun r -> r.ext_left_symbol ---> (List.rev r.ext_right_part))
                |> List.rev_map (remove_epsilon quo)
                |> List.flatten
                |> List.rev_map (fun r -> r.ext_left_symbol ---> (List.rev r.ext_right_part)) (* remove epsilon at end *)
                |> List.sort_uniq compare in

    let remove_pf_epsilon_once (rlist: ext_rule list): ext_rule list =
        rlist
                |> List.rev_map (remove_epsilon quo) (* remove epsilon at beginning *)
                |> List.flatten
                |> List.sort_uniq compare in

    replace_rules_in_mem quo rlist; (* replace in memory before the epsilon removing *)
    let new_rules = List.sort_uniq compare (match sd with
    | Left -> remove_pf_epsilon_once rlist
    | Right -> remove_sf_epsilon_once rlist) in
    if new_rules <> rlist then (remove_pf_sf_epsilon [@tailcall]) quo sd new_rules
    else new_rules

(* apply a left quotient of a single rule with a prefix that is a single element *)
let quotient_by_one_element (quo: t) (sd: side) (pf: element) (new_lhs: ext_element) (r: ext_part) : ext_element option =
    assert (is_non_terminal new_lhs.e);
    if new_lhs.e = pf && (new_lhs.pf@new_lhs.sf) = [pf] then
        add_rule_in_mem quo sd new_lhs [];
    match r with
    | [] -> None
    (* A -> aBC with prefix = a *)
    | t::q when t.e=pf && is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); add_rule_in_mem quo sd new_lhs q; None
    (* A -> aBC with prefix != a *)
    | t::_ when is_ext_element_terminal t -> None
    (* A -> B_{D|}C *)
    | t::q -> let new_elem = {pf=pf::t.pf;e=t.e;sf=t.sf} in
        add_rule_in_mem quo sd new_lhs (new_elem::q); Some t

(* compute new rules with a new prefix *)
(* tail recursive *)
let rec quotient_by_one_element_mem (quo: t) (sd: side) (prefix: element) (lhs_list: ext_element list) (all_new_elems: ext_element list) : (ext_element list) =
    match lhs_list with
    | [] -> all_new_elems
    | prev_lhs::q ->
        begin
            let new_lhs = match sd with
                | Right -> {pf=prev_lhs.pf;e=prev_lhs.e;sf=prefix::prev_lhs.sf}
                | Left -> {pf=prefix::prev_lhs.pf;e=prev_lhs.e;sf=prev_lhs.sf} in
            (*print_endline ("New: "^(string_of_ext_element new_lhs));*)
            if not (is_processed quo new_lhs) then begin
                initialize_mem quo new_lhs;
                let prev_lhs_to_quotient = get_rules quo sd prev_lhs |> List.filter_map (quotient_by_one_element quo sd prefix new_lhs) |> List.rev_map (reverse_ext_elem sd) |> List.sort_uniq compare in
                (* all new lhs should have been already processed *)
                assert (List.for_all (is_processed quo) prev_lhs_to_quotient);
                (*List.iter (fun e -> print_endline (string_of_ext_element e)) prev_lhs_to_quotient;*)
                if Option.is_some quo.graph_channel then begin
                    let new_elems = List.rev_map (fun {pf;e;sf} -> {pf=prefix::pf;e=e;sf=sf}) prev_lhs_to_quotient in
                    List.iter (fun e -> Grammar_io.add_edge_in_graph quo.graph_channel "" new_lhs (reverse_ext_elem sd e)) new_elems;
                end;
                (*List.iter (fun p -> print_endline (string_of_ext_rule (new_lhs ---> p))) (Hashtbl.find mem new_lhs);*)
                (quotient_by_one_element_mem [@tailcall]) quo sd prefix (prev_lhs_to_quotient@q) (new_lhs::all_new_elems)
            end else
                (*(print_endline "Already processed!";
                print_endline (string_of_ext_rules (get_all_rules [new_lhs]));*)
                (quotient_by_one_element_mem [@tailcall]) quo  sd prefix q all_new_elems;
        end

(* compute the rules of a ext_element and do it recursively with all the prefix/suffix *)
(* tail-recursive *)
let rec quotient_symbols (quo: t) (elist: ext_element list) : unit =
    match elist with
    | [] -> ()
    | lhs::q ->
        (* Nothing to do *)
        (*print_endline ("Work on: "^(string_of_ext_element lhs));*)
        if lhs.pf = [] && lhs.sf = [] then begin
            assert (is_ext_element_terminal lhs || is_processed quo lhs);
            (quotient_symbols [@tailcall]) quo q
        end else if is_processed quo lhs then
                (*(print_endline " Already known";*) (quotient_symbols [@tailcall]) quo q
        else if is_ext_element_terminal lhs then begin
            initialize_mem quo lhs;
            if (lhs.pf=[lhs.e] && lhs.sf=[]) || (lhs.pf=[] && lhs.sf=[lhs.e]) then
                add_rule_in_mem quo Left lhs []
        end else begin
            assert (not (is_ext_element_terminal lhs));
            let (base_lhs,sd,qu) = match lhs.pf,lhs.sf with
            | ((tpf::qpf) as pf),sf when List.compare_lengths pf sf >= 0 -> ({pf=qpf;e=lhs.e;sf=sf},Left,tpf) (* we decompose the longest -fix *)
            | _,[] -> assert false (* impossible because of the previous case *)
            | pf,(tsf::qsf) -> ({pf=pf;e=lhs.e;sf=qsf},Right,tsf) in
            if is_useless quo base_lhs then begin
                (* we ignore this element *)
                set_useless quo lhs;
                set_color quo "grey" lhs;
                (* print_endline (" Useless because of "^(string_of_ext_element base_lhs)); *)
                (quotient_symbols [@tailcall]) quo q
            end else if is_processed quo base_lhs then begin
                (* print_endline ("ICI "^(string_of_ext_element lhs)^", prefix: "^(string_of_element qu)); *)
                (* print_endline ("Base: "^(string_of_ext_rules (get_all_rules [base_lhs]))); *)
                Grammar_io.add_edge_in_graph quo.graph_channel "penwidth=3" lhs base_lhs;
                (* we can compute the current symbol *)
                (* print_endline "  Compute"; *)
                let new_elist = quotient_by_one_element_mem quo sd qu [base_lhs] [] in

                (* make the grammar epsilon-free. Only the new rules are concerned *)
                new_elist |> get_all_rules quo (* get the new elements *)
(*                        |> List.map (fun r -> print_endline ("A: "^(string_of_ext_rule r)); r)*)
                    |> update_words_and_useless quo
                    |> get_all_rules quo
(*                        |> List.map (fun r -> print_endline ("B: "^(string_of_ext_rule r)); r)*)
                    |> List.filter (fun r -> List.for_all (fun e -> not (is_useless quo e)) r.ext_right_part) (* remove rules with useless symbols *)
                    |> List.sort_uniq compare
                    |> remove_pf_sf_epsilon quo sd (* make epsilon-free *)
                    |> replace_rules_in_mem quo;

                (* print_endline "Finally"; *)
                (* List.iter (fun new_lhs -> List.iter (fun p -> print_endline (string_of_ext_rule (new_lhs ---> p))) (Hashtbl.find mem new_lhs)) new_elist; *)
                (* print_endline "End"; *)

                if Option.is_some quo.graph_channel then List.iter (fun e -> if is_useless quo e then (set_color quo "grey" e)) new_elist;
                (quotient_symbols [@tailcall]) quo q
            end else begin
                (* print_endline "  Postpone"; *)
                (* we can't compute the current symbol so we keep it on the list *)
                (quotient_symbols [@tailcall]) quo (base_lhs::elist)
            end
        end

(* tail-recursive *)
let rec fuzzer_minimize (quo: t) (goal_rules: ext_rule list) (word_prefix: element list) (sentential_suffix: ext_element list) : element list =
    match sentential_suffix, goal_rules with
    (* verify the sentential form length *)
    | l,_ when List.compare_length_with l 1000000 > 0 -> failwith "Fuzzing failure: word too long"
    (* no more suffix. All goal rules should have been used. *)
    | [],_ -> assert (goal_rules = []); List.rev word_prefix
    (* the suffix starts with a terminal *)
    | t::q,_ when is_ext_element_terminal t -> (fuzzer_minimize [@tailcall]) quo goal_rules (t.e::word_prefix) q
    (* the suffix starts with a goal rule *)
    | t::q,r::q2 when r.ext_left_symbol=t -> (fuzzer_minimize [@tailcall]) quo q2 word_prefix (r.ext_right_part@q)
    (* do we have a specific request for the start of the suffix ? *)
    | {pf=[];e=t;sf=[]}::q,_ when Option.map (fun v -> Hashtbl.mem v t) quo.dict = Some true ->
            (fuzzer_minimize [@tailcall]) quo goal_rules ((Terminal (Hashtbl.find (Option.get quo.dict) t))::word_prefix) q
    (* do we have a words saved for the start of the suffix ? *)
    | t::q,_ when Hashtbl.mem quo.words t -> (fuzzer_minimize [@tailcall]) quo goal_rules ((List.rev (Hashtbl.find quo.words t))@word_prefix) q
    | _ -> failwith "No words?"

let rec has_new (seen: ext_element list) (p: ext_element list) : bool = match p with
    | [] -> false
    | t::_ when not (List.mem t seen) -> true
    | _::q -> (has_new [@tailcall]) seen q

(* build all the possible one-step derivation of part p in the grammar g *)
let build_derivation (quo: t) (p: ext_part) : (ext_rule * ext_part) list =
    (* tail-recursive *)
    let rec build_derivation_aux (sofar: ext_part) (acc: (ext_rule * ext_part) list) (p: ext_part) : (ext_rule * ext_part) list = match p with
        | [] -> acc
        | t::q when Hashtbl.mem quo.cant_reach_goal t.e || is_ext_element_terminal t || t.e = Nonterminal "poirot_nonterminal_comment" ->
                (build_derivation_aux [@tailcall]) (t::sofar) acc q (* don't search in the oneline comment to find the goal *)
        | t::q-> let rhs = Hashtbl.find quo.mem t in
            let new_parts = rhs |> List.rev_map (fun rhs -> (t--->rhs),(List.rev sofar)@rhs@q) in
(*                    let new_parts = g.rules |> List.filter (fun r -> r.left_symbol = t) |> List.rev_map (fun r -> r,(List.rev sofar)@r.right_part@q) in*)
            (build_derivation_aux [@tailcall]) (t::sofar) (new_parts@acc) q in
    build_derivation_aux [] [] p

(* find a path (a list of rule) to produce, and an empty list if there is no path *)
(* tail-recursive *)
let rec find_path_to_goal_aux (quo: t) (goal: ext_element) (seen: ext_element list) (left_to_find: int) (found : ext_rule list list) (queue : (ext_part * ext_rule list) list) : ext_rule list list =
    match queue with
    | [] -> found (* no more path *)
    | (form,path)::q when List.mem goal form ->
            if left_to_find = 1 then found (* we found them all *)
            else (find_path_to_goal_aux [@tailcall]) quo goal seen (left_to_find-1) ((List.rev path)::found) q
    | (form,_)::q when not (has_new seen form) -> (find_path_to_goal_aux [@tailcall]) quo goal seen left_to_find found q
    | (form,path)::q -> let new_items = List.rev_map (fun (r,p) -> (p,r::path)) (build_derivation quo form) in
        (find_path_to_goal_aux [@tailcall]) quo goal (List.sort_uniq compare (form@seen)) left_to_find found (q@new_items)

let find_path_to_goal (quo: t) (goal: ext_element) (axiom : ext_element) : ext_rule list list =
    find_path_to_goal_aux quo goal [] 10 [] [([axiom],[])]

(* get a derivation of ext_element with the "smallest" non-trivial rule *)
let get_first_derivation (quo: t) (e: ext_element) : ext_element list =
    (* smallest to biggest rule *)
    let l = (*List.rev*) (List.sort compare_ext_rule (get_all_rules quo [e])) in
    let l2 = List.filter (fun r -> List.compare_length_with r.ext_right_part 1 > 0) l in
    match l2 with
        | [] -> (List.hd l).ext_right_part;
        | t::_ -> t.ext_right_part

let get_grammar (quo: t) (e: ext_element) : ext_grammar =
    let start_time = Unix.gettimeofday () in
    quotient_symbols quo [e];
    let out = grammar_of_mem quo e in
    quo.call_time <- quo.call_time +. (Unix.gettimeofday () -. start_time);
    out

let get_injection (quo: t) (e: ext_element) (goal: element option) : (part list * bool) =
    let goal = Option.map ext_element_of_element goal in
    let start_time = Unix.gettimeofday () in
    quotient_symbols quo [e];
    quo.call_time <- quo.call_time +. (Unix.gettimeofday () -. start_time);

    if is_useless quo e then ([],false)
    else begin
    let start_time = Unix.gettimeofday () in
        let path = match goal with
            | None -> []
            | Some g -> find_path_to_goal quo g e in
        let out = match path with
        | [] -> ([fuzzer_minimize quo [] [] (get_first_derivation quo e)], false)
        | all_path -> Log.L.debug (fun m -> m "Fuzzing with goal"); (List.sort_uniq compare (List.map (fun l -> fuzzer_minimize quo l [] [e]) all_path), true) in
        quo.fuzzer_time <- quo.fuzzer_time +. (Unix.gettimeofday () -. start_time);
        out
    end

(* tail-recursive *)
let rec split_list (l: element list) (middle_index: int) (aux: element list) : (element list) * (element list) =
    match middle_index,l with
    | n,[] when n>0 -> (List.rev aux, [])
    | n,_ when n<=0 -> (List.rev aux, l)
    | n,t::q -> assert (n>0); (split_list [@tailcall]) q (n-1) (t::aux)
    | _,[] -> assert false (* previous pattern is exhaustive *)

let is_in_language (quo: t) (axiom: ext_element) (word: part) : bool =
    let l1,l2 = split_list word ((List.length (word) + List.length axiom.sf - List.length axiom.pf)/2) [] in
    (* we balance in the prefix and the suffix *)
    let e = {pf=(List.rev l1)@axiom.pf;e=axiom.e;sf=l2@axiom.sf} in
    quotient_symbols quo [e];
    (* print_endline (string_of_word word); *)
    (* print_endline ((string_of_ext_element e)^" "^(string_of_ext_rules (get_all_rules quo [e]))); *)
    (* print_endline (string_of_bool (can_epsilon quo e)); *)
    can_epsilon quo e

let print_statistics (quo: t) : unit =
    Log.L.debug (fun m -> m "Quotient memory size: %d" (Hashtbl.length quo.mem))

let get_call_time (quo: t) : float =
    quo.call_time

let get_fuzzer_time (quo: t) : float =
    quo.fuzzer_time

let refuse_injections (quo: t) (e: element) : unit =
    Hashtbl.add quo.cant_reach_goal e ()

let finalizer (quo: t) : unit =
    Option.iter (fun ch -> Log.L.info (fun m -> m "Save quotient graph."); output_string ch "}"; close_out ch) quo.graph_channel

let init (oneline_comment: string option) (g_initial: grammar) (forbidden: char list) (dict: (element,string) Hashtbl.t option) (qgraph_fname : string option) : t =
    let q = {words = Hashtbl.create 100000;
        mem = Hashtbl.create 100000;
        cant_reach_goal = Hashtbl.create 100;
        dict = dict;
        forbidden = forbidden;
        call_time = 0.;
        fuzzer_time = 0.;
        graph_channel = Option.map open_out qgraph_fname} in

    Option.iter (fun ch -> output_string ch "digraph {\n") q.graph_channel;

    let g_initial = match oneline_comment with
        | Some s -> Grammar.add_comment_quotient g_initial s
        | None -> g_initial in

    (* the grammar must be epsilon-free ! *)
    let g_rules = List.sort_uniq compare (ext_grammar_of_grammar g_initial).ext_rules
                    |> remove_pf_sf_epsilon q Right
                    |> remove_pf_sf_epsilon q Left in


    (* we update the rules of the base grammar *)
    replace_rules_in_mem q g_rules;
    ignore (update_words_and_useless q g_rules);
    q

