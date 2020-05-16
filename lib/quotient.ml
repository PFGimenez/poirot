open Grammar

type side = Left | Right

type t = {  words : (ext_element, bool * part list) Hashtbl.t; (* the words of the fuzzer *)
            mem : (ext_element, ext_part list) Hashtbl.t; (* the rules *)
            dict: (element,string) Hashtbl.t option; (* the semantics dictionary *)
            cant_reach_goal: (element,unit) Hashtbl.t; (* TODO *)
            goal: element option;
            forbidden: char list; (* the set of characters that should not appear in injections *)
            mutable call_time: float;
            mutable fuzzer_time: float;
            graph_channel : out_channel option} (* graphviz graph *)

(* compare the ext_rules. Sorting with this comparison puts the "smallest" rules at the beginning.
 * Puts the rules that produce the goal at the beginning *)
let compare_rhs (quo: t) (r1: ext_part) (r2: ext_part) : int =
    let b1 = List.exists (fun e -> Some e.e = quo.goal) r1 in
    let b2 = List.exists (fun e -> Some e.e = quo.goal) r2 in
    let diff = List.compare_lengths (List.filter is_ext_element_non_terminal r1) (List.filter is_ext_element_non_terminal r2) in
    if b1 && (not b2) then -1
    else if (not b1) && b2 then 1
    else if diff <> 0 then diff
    else begin
        let diff2 = List.compare_lengths r1 r2 in
        if diff2 <> 0 then diff2
        else (Hashtbl.hash r2) - (Hashtbl.hash r1) (* to be deterministic we want to compare any couple *)
    end

(* compare two rules. *)
let compare_ext_rule (quo: t) (r1: ext_rule) (r2: ext_rule) : int =
    compare_rhs quo r1.ext_right_part r2.ext_right_part

let rec get_reachable_symbols (quo: t) (slist : ext_element list) : ext_element list =
    let get_reachable_symbols_once (slist : ext_element list) : ext_element list = slist |> List.rev_map (Hashtbl.find quo.mem) |> List.concat |> List.cons slist |> List.concat |> List.sort_uniq compare |> List.filter is_ext_element_non_terminal in
    let slist2 = get_reachable_symbols_once slist in
    if List.compare_lengths slist slist2 = 0 then slist else (get_reachable_symbols [@tailcall]) quo slist2

let is_processed (quo: t) (e: ext_element) : bool = Hashtbl.mem quo.mem e

(* is the element useless, i.e. it is processed but is not bound to any rule *)
let is_useless (quo: t) (e: ext_element) : bool = Hashtbl.find_opt quo.mem e = Some []

let is_useful (quo: t) (e: ext_element) : bool = is_ext_element_terminal e || (Hashtbl.mem quo.mem e && Option.get (Hashtbl.find_opt quo.mem e) <> [])

let set_useless (quo: t) (e: ext_element) : unit = Hashtbl.replace quo.mem e []

let initialize_mem (quo: t) (e: ext_element) : unit = assert (not (is_processed quo e)); Hashtbl.replace quo.mem e []

let set_color (quo: t) (c: string) (e: ext_element) : unit = Grammar_io.set_node_color_in_graph quo.graph_channel e c

let is_allowed (quo: t) (e: ext_element): bool = match e with
    | {e=Terminal s;_} -> not (List.exists (String.contains s) quo.forbidden)
    | _ -> true

(* Get the set of rules of a list of ext_element. No reverse *)
let get_all_rules (quo: t) (elist: ext_element list) : ext_rule list =
    elist |> List.rev_map (fun e -> List.rev_map (fun rhs -> e ---> rhs) (Hashtbl.find quo.mem e)) |> List.concat

let update_one_word (quo: t) (lhs: ext_element) (r: ext_rule) : part = (* TODO: à revoir *)
    print_endline ("Update one word: "^(string_of_ext_rule r));
    match quo.dict with
        | Some hashtbl when lhs.pf=[] && lhs.sf=[] && Hashtbl.mem hashtbl lhs.e -> [Terminal (Hashtbl.find hashtbl lhs.e)]
        | _ -> r.ext_right_part |> List.map (fun e -> if is_ext_element_terminal e then [e.e] else List.hd (snd (Hashtbl.find quo.words e))) |> List.concat (* all the prerequisite for computing the word are already computed ! *)


(* update the words of some lhs. It will generate a list of words, one for each rule of lhs. There is already a word. *)
let update_words (quo: t) (lhs: ext_element) : unit = 
    print_endline ("Update words of: "^(string_of_ext_element lhs));
    let rlist = get_all_rules quo [lhs] in

    let (goal_reached,_) = Hashtbl.find quo.words lhs in
    if goal_reached then begin
        Hashtbl.add quo.words lhs (goal_reached,List.map (update_one_word quo lhs) rlist)
    end else begin (* if that words can't reach the goal, we generate the shortest word *)
        let word = rlist
            |> List.map (update_one_word quo lhs)
            |> List.sort List.compare_lengths
            |> List.hd in
        Hashtbl.add quo.words lhs (false,[word])
    end

(* populate words with one rule whose rhs have words already *)
let update_first_word (quo: t) (r: ext_rule) : unit =
    print_endline ("First word: "^(string_of_ext_rule r));
    let lhs = r.ext_left_symbol in
    if not (Hashtbl.mem quo.words lhs) then begin
        let (goal_reached,w) = match quo.dict with
            | Some hashtbl when lhs.pf=[] && lhs.sf=[] && Hashtbl.mem hashtbl lhs.e -> (false,[Terminal (Hashtbl.find hashtbl lhs.e)]) (* use the semantics dictionary *)
            | _ -> (List.exists (fun e -> Some e.e = quo.goal || (not (is_ext_element_terminal e) && fst (Hashtbl.find quo.words e))) r.ext_right_part, (* this word can obtain the goal if one of its children can *)
                r.ext_right_part |> List.map (fun e -> if is_ext_element_terminal e then [e.e] else List.hd (snd (Hashtbl.find quo.words e))) |> List.concat) in (* all the prerequisite for computing the word are already computed ! *)
        (* print_endline ("Word: "^(string_of_word w)^". Can reach goal: "^(string_of_bool goal_reached)); *)
        Hashtbl.add quo.words lhs (goal_reached,[w]);
        print_endline "Added !"
    end

let remove_useless_rules (quo: t) (lhs: ext_element) : unit =
    print_endline (string_of_ext_element lhs);
    assert (is_useful quo lhs);
    let rhs = Hashtbl.find quo.mem lhs in
    Hashtbl.replace quo.mem lhs (List.filter (List.for_all (is_useful quo)) rhs);
    assert (is_useful quo lhs)

(* based on level decomposition *)
let update_words_and_useless (quo: t) (original_rules: ext_rule list) : ext_element list =
    let rec update_words_and_useless_aux (reached_sym: ext_element list) (original_rules: ext_rule list) : ext_element list =
        let usable_rules = List.filter (fun r -> List.for_all (fun s -> (is_ext_element_terminal s || Hashtbl.mem quo.words s) && is_allowed quo s) r.ext_right_part) original_rules in
        if usable_rules = [] then reached_sym (* the algorithm is done *)
        else begin
            (* TODO: virer print_endline *)
            List.iter (fun r -> (*print_endline (string_of_ext_rule r);*) update_first_word quo r) (List.sort (compare_ext_rule quo) usable_rules);
            (update_words_and_useless_aux [@tailcall]) ((List.rev_map lhs_of_ext_rule usable_rules)@reached_sym) (List.filter (fun r -> not (Hashtbl.mem quo.words r.ext_left_symbol)) original_rules)
        end in
    let reached = update_words_and_useless_aux [] original_rules in
    (* unreachable symbols are useless *)
    original_rules |> List.rev_map lhs_of_ext_rule |> List.filter (fun e -> not (List.mem e reached)) |> List.iter (set_useless quo);
    original_rules |> List.rev_map lhs_of_ext_rule |> List.filter (is_useful quo) |> List.iter (remove_useless_rules quo); (* let's remove the rules that involve useless symbols. Remark that it can't make a element useless *)
    List.iter (update_words quo) reached;
    reached

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
            if not (is_processed quo new_lhs) then begin
                initialize_mem quo new_lhs;
                let prev_lhs_to_quotient = get_rules quo sd prev_lhs |> List.filter_map (quotient_by_one_element quo sd prefix new_lhs) |> List.rev_map (reverse_ext_elem sd) |> List.sort_uniq compare in
                (* all new lhs should have been already processed *)
                assert (List.for_all (is_processed quo) prev_lhs_to_quotient);
                if Option.is_some quo.graph_channel then begin
                    let new_elems = List.rev_map (fun {pf;e;sf} -> {pf=prefix::pf;e=e;sf=sf}) prev_lhs_to_quotient in
                    List.iter (fun e -> Grammar_io.add_edge_in_graph quo.graph_channel "" new_lhs (reverse_ext_elem sd e)) new_elems;
                end;
                (quotient_by_one_element_mem [@tailcall]) quo sd prefix (prev_lhs_to_quotient@q) (new_lhs::all_new_elems)
            end else
                (quotient_by_one_element_mem [@tailcall]) quo  sd prefix q all_new_elems;
        end

(* compute the rules of a ext_element and do it recursively with all the prefix/suffix *)
(* tail-recursive *)
let rec quotient_symbols (quo: t) (elist: ext_element list) : unit =
    match elist with
    | [] -> ()
    | lhs::q ->
        (* Nothing to do *)
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
                (quotient_symbols [@tailcall]) quo q
            end else if is_processed quo base_lhs then begin
                Grammar_io.add_edge_in_graph quo.graph_channel "penwidth=3" lhs base_lhs;
                let new_elist = quotient_by_one_element_mem quo sd qu [base_lhs] [] in

                (* make the grammar epsilon-free. Only the new rules are concerned *)
                new_elist |> get_all_rules quo (* get the new elements *)
                    |> update_words_and_useless quo
                    |> get_all_rules quo
                    |> List.filter (fun r -> List.for_all (fun e -> not (is_useless quo e)) r.ext_right_part) (* remove rules with useless symbols *)
                    |> List.sort_uniq compare
                    |> remove_pf_sf_epsilon quo sd (* make epsilon-free *)
                    |> replace_rules_in_mem quo;

                if Option.is_some quo.graph_channel then List.iter (fun e -> if is_useless quo e then (set_color quo "grey" e)) new_elist;
                (quotient_symbols [@tailcall]) quo q
            end else begin
                (* we can't compute the current symbol so we keep it on the list *)
                (quotient_symbols [@tailcall]) quo (base_lhs::elist)
            end
        end

let nontrivial_word (quo: t) (lhs: ext_element) : part =
    assert (is_processed quo lhs);
    let all_inj = get_all_rules quo [lhs]
        |> List.map (update_one_word quo lhs)
        |> List.map (fun p -> print_endline (string_of_part p); p)
        |> List.sort List.compare_lengths in
    assert (all_inj <> []); (* there is at least the trivial injection *)
    if List.compare_length_with all_inj 1 = 0 then
        List.hd all_inj (* only the trivial injection… *)
    else
        List.hd (List.tl all_inj) (* TODO: the trivial injection is not necessary the smallest… *)

let get_grammar (quo: t) (e: ext_element) : ext_grammar =
    let start_time = Unix.gettimeofday () in
    quotient_symbols quo [e];
    let out = grammar_of_mem quo e in
    quo.call_time <- quo.call_time +. (Unix.gettimeofday () -. start_time);
    out

let get_injection (quo: t) (e: ext_element) : (bool * part list) =
    let start_time = Unix.gettimeofday () in
    quotient_symbols quo [e];
    quo.call_time <- quo.call_time +. (Unix.gettimeofday () -. start_time);

    if is_useless quo e then (false,[])
    else begin
        let goal_reached, words = Hashtbl.find quo.words e in
        if goal_reached then begin
            List.iter (fun w -> print_endline (string_of_word w)) words;
            (true, words)
        end else begin (* only one non-trivial injection *)
            print_endline ("Non-trivial injection :"^(string_of_word (nontrivial_word quo e)));
            (false, [nontrivial_word quo e])
        end
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
    (* TODO *)
    Hashtbl.add quo.cant_reach_goal e ()

let finalizer (quo: t) : unit =
    Option.iter (fun ch -> Log.L.info (fun m -> m "Save quotient graph."); output_string ch "}"; close_out ch) quo.graph_channel

let init (oneline_comment: string option) (g_initial: grammar) (forbidden: char list) (dict: (element,string) Hashtbl.t option) (qgraph_fname : string option) (goal: element option) : t =
    let q = {words = Hashtbl.create 100000;
        mem = Hashtbl.create 100000;
        cant_reach_goal = Hashtbl.create 100;
        dict = dict;
        forbidden = forbidden;
        call_time = 0.;
        fuzzer_time = 0.;
        goal = goal;
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

