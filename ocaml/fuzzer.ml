open Grammar

let string_inst_of_element (values : (element, string) Hashtbl.t) : element -> string  = function
    | s when Hashtbl.mem values s -> Hashtbl.find values s
    | s -> string_of_element s

let string_inst_of_part values = string_of_list "" "" (string_inst_of_element values)

let rec first_non_terminal = function
    | [] -> None
    | Terminal(x)::rest -> (first_non_terminal [@tailcall]) rest
    | Nonterminal(x)::rest -> Some (Nonterminal(x))

let rec first_non_terminal_ext_part : ext_element list -> ext_element option = function
    | [] -> None
    | {pf=pre; e=Terminal(x); sf=suf}::rest -> (first_non_terminal_ext_part [@tailcall]) rest
    | e::rest -> Some e


(* Dérivation gauche d'un mot intermédiaire "dérivation" par une règle "rule" *)
let rec left_derivation derivation rule =
	let {left_symbol=base;right_part=transformation} = rule in
	match derivation with
	| [] -> []
	| Nonterminal(x)::rest when Nonterminal(x)=base -> List.append transformation rest
	| x::rest -> x::left_derivation rest rule

let rec left_derivation2 (derivation : ext_element list) (rule : ext_rule) =
    let {ext_left_symbol=base;ext_right_part=transformation} = rule in
	match derivation with
	| [] -> []
    | ({pf=pre; e=Nonterminal(x); sf=suf} as e)::rest when e=base -> List.append transformation rest
	| x::rest -> x::left_derivation2 rest rule


(* Renvoie toutes les règles possibles parmi un liste de règles pour dériver motintermediaire *)
let rec possible_rules motintermediaire rules =
	let premier = first_non_terminal motintermediaire in
	match premier with
	| None -> []
	| Some x ->
	match rules with
	| [] -> []
    | {left_symbol=gauche;right_part=droite}::rest when x = gauche ->
		gauche-->droite::(possible_rules motintermediaire rest)
    | _::rest -> possible_rules motintermediaire rest

let rec possible_rules2 (motintermediaire : ext_element list) (rules : ext_rule list) : ext_rule list =
	let premier = first_non_terminal_ext_part motintermediaire in
	match premier with
	| None -> []
	| Some x ->
	match rules with
	| [] -> []
    | {ext_left_symbol=ls;ext_right_part=rp}::rest when x = ls ->
		ls--->rp::(possible_rules2 motintermediaire rest)
    | _::rest -> possible_rules2 motintermediaire rest


(* Prédicat indiquant si le mot intermédiaire en argument est un mot (soit si il ne contient que des terminaux) *)
let rec is_word = function
	| [] -> true
	| Terminal(x)::rest -> is_word rest
	| Nonterminal(x)::rest -> false

let rec is_word2 : ext_element list -> bool = function
	| [] -> true
    | {pf=[]; e=Terminal(x); sf=[]}::rest -> is_word2 rest
	| _ -> false

let rec derive_within_depth profondeur grammar motintermediaire =
    if is_word motintermediaire then [motintermediaire]
	else if (profondeur <> 1) then
		let possibles = possible_rules motintermediaire grammar.rules in
		let rec deriverLesPossibles grammar mot = function
            | [] -> []
			| rule::reste -> List.append (derive_within_depth (profondeur-1) grammar (left_derivation mot rule))
					  (deriverLesPossibles grammar mot reste) in
		deriverLesPossibles grammar motintermediaire possibles
    else []

(* DeriverTout depuis l'axiom de la grammar fournie *)
let deriver profondeur grammar = derive_within_depth profondeur grammar [grammar.axiom]

let derive_and_print profondeur grammar = List.iter (fun r -> print_endline ("Mot: "^(string_of_part r))) (deriver profondeur grammar)

(** Vérification de la dérivabilité d'une phrase à partir d'une grammar **)

let rec derive_within_length longueur grammar motintermediaire =
	if (List.length motintermediaire <= longueur) then begin
        if is_word motintermediaire then [motintermediaire]
        else
            let possibles = possible_rules motintermediaire grammar.rules in
            let rec deriverLesPossibles grammar mot = function
                | [] -> []
                | rule::reste -> List.append (List.filter (fun l -> List.length l <= longueur) (derive_within_length longueur grammar (left_derivation mot rule)))
                        (deriverLesPossibles grammar mot reste) in
            deriverLesPossibles grammar motintermediaire possibles
    end
    else []

(** Vérifie si un ensemble de mots fait part d'un langage. Plus rapide que de vérifier chaque mot indépendamment **)

let is_list_in_language grammar parties =
    let len = List.fold_left max 0 (List.map List.length parties) in
    let words = derive_within_length len grammar [grammar.axiom] in
    List.for_all (fun p -> List.mem p words) parties

let is_in_language grammar part = (* print_endline ((part2string part));*) List.mem part (derive_within_length (List.length part) grammar [grammar.axiom])

let derive_within_lengthPrint longueur grammar = List.iter (fun r -> print_endline ("Mot: "^(string_of_part r)) (*; print_bool (is_in_language grammar r)*)) (derive_within_length longueur grammar [grammar.axiom])

let min_list a b = if List.length a < List.length b then a else b

let length_non_terminal r = List.length (List.filter (fun s -> not (is_terminal s)) r.right_part)

let min_rule a b = if length_non_terminal a < length_non_terminal b then a else b

let rec find_path_symbol grammar = function
    | [] -> failwith "No path"
    | (obj,path)::_ when obj=grammar.axiom -> path
    | (obj,path)::q -> let ext_rules = List.filter (fun r -> List.mem obj r.right_part) grammar.rules in
        (find_path_symbol [@tailcall]) grammar (q@(List.map (fun r -> (r.left_symbol, r::path)) ext_rules))

let rec derive_with_path grammar = function
    | [] -> failwith "No derivation with path"
    | (w, path)::q when is_word w -> assert (List.length path = 0); w
    | (w, path)::q -> let ext_rules = possible_rules w grammar.rules in
        if List.length path = 0 || not (List.mem (List.hd path) ext_rules) then
            (derive_with_path [@tailcall]) grammar (q@(List.map (fun r -> (left_derivation w r, path)) ext_rules))
        else
            (derive_with_path [@tailcall]) grammar (q@[left_derivation w (List.hd path), List.tl path])

let derive_word_with_symbol grammar s = derive_with_path grammar [[grammar.axiom],find_path_symbol grammar [s,[]]]

let rec find_path_symbol2 (grammar : ext_grammar) : (element*ext_rule list) list -> ext_rule list = function
    | [] -> failwith "No path"
    | (obj,path)::_ when obj=element_of_ext_element grammar.ext_axiom -> path
    | (obj,path)::q -> let ext_rules = List.filter (fun r -> List.exists (fun t -> obj = element_of_ext_element t) r.ext_right_part) grammar.ext_rules in
        (find_path_symbol2 [@tailcall]) grammar (q@(List.map (fun r -> (element_of_ext_element r.ext_left_symbol, r::path)) ext_rules))

let rec derive_with_path2 (grammar : ext_grammar) : (ext_element list * ext_rule list) list -> part = function
    | [] -> failwith "No derivation with path"
    | (w, path)::q when is_word2 w -> assert (List.length path = 0); List.map element_of_ext_element w
    | (w, path)::q -> let ext_rules = possible_rules2 w grammar.ext_rules in
        if List.length path = 0 || not (List.mem (List.hd path) ext_rules) then
            (derive_with_path2 [@tailcall]) grammar (q@(List.map (fun r -> (left_derivation2 w r, path)) ext_rules))
        else
            (derive_with_path2 [@tailcall]) grammar (q@[left_derivation2 w (List.hd path), List.tl path])

let derive_word_with_symbol2 (grammar : ext_grammar) (s : element) : element list = derive_with_path2 grammar [[grammar.ext_axiom],find_path_symbol2 grammar [s,[]]]

let get_all_non_terminal (grammar : grammar) : element list = List.sort_uniq compare (List.flatten (List.map (fun r -> List.filter is_non_terminal r.right_part) grammar.rules))

let get_parents (g : grammar) (all : element list) (x : element) : element * (element list) =
    let ext_right_parts = List.map (fun r -> r.right_part) (List.filter (fun r -> r.left_symbol = x) g.rules) in
    (x, List.filter (fun t -> is_non_terminal t && List.for_all (fun tl -> List.mem t tl) ext_right_parts) all)

let rec get_order_from_par (par : (element * (element list)) list) (output : element list) : element list = match par with
    | [] -> output
    | l -> let l_nopar = List.filter (fun (t,tl) -> (first_non_terminal tl = None)) par in match l_nopar with
        | [] -> failwith "Impossible, entirely recursive grammar"
        | (ht,_)::q -> let new_par = List.filter (fun (t,tl) -> t <> ht) par in
                let new_par_filtered = List.map (fun (t,tl) -> (t,List.filter (fun t -> t<>ht) tl)) new_par in
                (get_order_from_par [@tailcall]) new_par_filtered (ht::output)

let get_order (g : grammar) : element list =
    let all = get_all_non_terminal g in
    let par = List.map (get_parents g all) all in
    get_order_from_par par []

let delete_recursion (g : grammar) : grammar =
    let rec del_rec (rl : rule list) (forbidden : element list): element list -> rule list = function
        | [] -> rl
        | t::q -> (del_rec [@tailcall]) (List.filter (fun r -> r.left_symbol<>t || List.for_all (fun s -> not (List.mem s forbidden || s=t)) r.right_part) rl) (t::forbidden) q
    and order = get_order g in
    g.axiom @@ (del_rec g.rules [] order)

let fuzzer (g : grammar) : part list =
    get_all_tokens g |> List.filter (fun e -> is_reachable g e g.axiom) |> List.map (derive_word_with_symbol g)

let oracle
    (prefix : element list)
    (suffix : element list)
    (grammar : grammar)
    (injections : part)
    : bool
    =   (*List.iter (fun p -> print_endline (string_of_part p)) injections;*)
        [injections] |> List.map (fun p -> prefix @ p @ suffix) |> is_list_in_language grammar

