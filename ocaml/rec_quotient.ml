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

let rec symbol_list_aux (f : rule -> element option) (g: grammar) (tasks: element list) (seen: element list) : element list = match tasks with
    | [] -> seen
    | t::q when List.mem t seen -> (symbol_list_aux [@tailcall]) f g q seen
    | t::q -> (symbol_list_aux [@tailcall]) f g (q@(List.filter_map f (List.filter (fun r -> r.left_symbol = t) g.rules))) (t::seen)

(* get the list of symbols that can be a prefix of the derivation of element *)
let left_symbol_list (g: grammar) (task: element) : element list =
    symbol_list_aux extract_left_symbol g [task] []

(* get the list of symbols that can be a suffix of the derivation of element *)
let right_symbol_list (g: grammar) (task: element) : element list =
    symbol_list_aux extract_right_symbol g [task] []

(* get the list of rules that can directly derive this element *)
let get_generative_rules (g: grammar) (e: element) : rule list =
    List.filter (fun r -> List.mem e r.right_part) g.rules

let quotient_mem (g: grammar) =
    let mem_size = 100 in
    let mem : ext_grammar = ext_grammar_of_grammar g
    (* A -> [B,C] means that B, C, etc. can be prefix of derivations of A *)
    (* if there is no key, prefix are unknown *)
    and rsym : (ext_element, element list) Hashtbl.t = Hashtbl.create mem_size
    and lsym : (ext_element, element list) Hashtbl.t = Hashtbl.create mem_size in

    (* apply a quotient of a single rule with a prefix that is a single element *)
    let quotient_by_one_element (pf: element) (r: ext_rule) : (ext_rule list) * (ext_element list)  =
        let new_lhs = {pf=r.ext_left_symbol.pf@[pf]; e=r.ext_left_symbol.e; sf=r.ext_left_symbol.sf} in
        match r.ext_right_part with
        | [] -> [],[]
        (* A -> aBC with prefix = a *)
        | t::q when t.e=pf && is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); [new_lhs ---> q],[]
        (* A -> aBC with prefix != a *)
        | t::q when is_ext_element_terminal t -> assert (t.pf=[] && t.sf=[]); [],[]
        (* A -> BC with prefix = B *)
        | t::q when t.e=pf && t.pf=[] -> let new_elem = {pf=[pf];e=t.e;sf=t.sf} in [new_lhs ---> q; new_lhs ---> (new_elem::q)],[new_elem]
        (* A -> B_{D|}C *)
        | t::q -> let possible = Hashtbl.find_opt lsym t in
            if possible = None || List.mem pf (Option.get possible) then
                let new_elem = {pf=t.pf@[pf];e=t.e;sf=t.sf} in [new_lhs ---> (new_elem::q)],[new_elem]
            else
                [],[]
    in

    (* apply a quotient of rules with a prefix *)
    let rec quotient_rules (rlist: ext_rule list) (pf: part) (new_sym: ext_element list) : ext_element list = match pf with
        | [] -> new_sym
        | t::q -> let (new_rlist,new_new_sym) = List.split (List.map (quotient_by_one_element t) rlist) in
            let new_rlist = List.flatten new_rlist and new_new_sym = List.flatten new_new_sym in
            (quotient_rules [@tailcall]) new_rlist q (new_new_sym@new_sym)
    in

    (* compute the rules of a ext_element and do it recursively with every new symbol *)
    let rec quotient_symbols (elist: ext_element list) (g: grammar) = match elist with
        | [] -> ()
        | t::q -> let new_elist = quotient_rules (List.map ext_rule_of_rule (get_generative_rules g t.e)) t.pf [] in
            (quotient_symbols [@tailcall]) (new_elist@elist) g
    in

        fun (g: grammar) (e: ext_element) : grammar -> g (* TODO *)

let quotient (g : grammar) = quotient_mem g
