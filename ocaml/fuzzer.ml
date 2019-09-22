open Base

(* Dérivation gauche d'un mot intermédiaire "dérivation" par une règle "regle" *)
let rec left_derivation derivation regle =
	let {elementgauche=base;partiedroite=transformation} = regle in
	match derivation with
	| [] -> []
	| Nonterminal(x)::rest when Nonterminal(x)=base -> List.append transformation rest
	| x::rest -> x::left_derivation rest regle

let rec left_derivation2 (derivation : tree_state list) (regle : rec_rule) =
    let {left_symbol=base;right_part=transformation} = regle in
	match derivation with
	| [] -> []
	| (pre,Nonterminal(x),suf)::rest when (pre,Nonterminal(x),suf)=base -> List.append transformation rest
	| x::rest -> x::left_derivation2 rest regle


(* Renvoie toutes les règles possibles parmi un liste de règles pour dériver motintermediaire *)
let rec possible_rules motintermediaire regles =
	let premier = first_non_terminal motintermediaire in
	match premier with
	| None -> []
	| Some(x) ->
	match regles with
	| [] -> []
    | {elementgauche=gauche;partiedroite=droite}::rest when x = gauche ->
		gauche-->droite::(possible_rules motintermediaire rest)
    | _::rest -> possible_rules motintermediaire rest

let rec possible_rules2 (motintermediaire : tree_state list) (regles : rec_rules) : rec_rules =
	let premier = first_non_terminal2 motintermediaire in
	match premier with
	| None -> []
	| Some(x) ->
	match regles with
	| [] -> []
    | {left_symbol=ls;right_part=rp}::rest when x = ls ->
		ls--->rp::(possible_rules2 motintermediaire rest)
    | _::rest -> possible_rules2 motintermediaire rest


(* Prédicat indiquant si le mot intermédiaire en argument est un mot (soit si il ne contient que des terminaux) *)
let rec is_word = function
	| [] -> true
	| Terminal(x)::rest -> is_word rest
	| Nonterminal(x)::rest -> false

let rec is_word2 : tree_state list -> bool = function
	| [] -> true
    | ([],Terminal(x),[])::rest -> is_word2 rest
	| _ -> false

let rec derive_within_depth profondeur grammaire motintermediaire =
    if is_word motintermediaire then [motintermediaire]
	else if (profondeur != 1) then
		let possibles = possible_rules motintermediaire grammaire.regles in
		let rec deriverLesPossibles grammaire mot = function
            | [] -> []
			| regle::reste -> List.append (derive_within_depth (profondeur-1) grammaire (left_derivation mot regle))
					  (deriverLesPossibles grammaire mot reste) in
		deriverLesPossibles grammaire motintermediaire possibles
    else []

(* DeriverTout depuis l'axiome de la grammaire fournie *)
let deriver profondeur grammaire = derive_within_depth profondeur grammaire [grammaire.axiome]

let derive_and_print profondeur grammaire = List.iter (fun r -> print_string ("Mot: "^(partie2string r)^"\n")) (deriver profondeur grammaire)

(** Vérification de la dérivabilité d'une phrase à partir d'une grammaire **)

let rec derive_within_length longueur grammaire motintermediaire =
	if (List.length motintermediaire <= longueur) then begin
        if is_word motintermediaire then [motintermediaire]
        else
            let possibles = possible_rules motintermediaire grammaire.regles in
            let rec deriverLesPossibles grammaire mot = function
                | [] -> []
                | regle::reste -> List.append (List.filter (fun l -> List.length l <= longueur) (derive_within_length longueur grammaire (left_derivation mot regle)))
                        (deriverLesPossibles grammaire mot reste) in
            deriverLesPossibles grammaire motintermediaire possibles
    end
    else []

(** Vérifie si un ensemble de mots fait partie d'un langage. Plus rapide que de vérifier chaque mot indépendamment **)

let is_list_in_language grammaire parties =
    let len = List.fold_left max 0 (List.map List.length parties) in
    let words = derive_within_length len grammaire [grammaire.axiome] in
    List.for_all (fun p -> List.mem p words) parties

let is_in_language grammaire partie = (* print_string ((partie2string partie)^"\n");*) List.mem partie (derive_within_length (List.length partie) grammaire [grammaire.axiome])

let derive_within_lengthPrint longueur grammaire = List.iter (fun r -> print_string ("Mot: "^(partie2string r)^"\n") (*; print_bool (is_in_language grammaire r)*)) (derive_within_length longueur grammaire [grammaire.axiome])

let min_list a b = if List.length a < List.length b then a else b

let length_non_terminal r = List.length (List.filter (fun s -> not (is_terminal s)) r.partiedroite)

let min_rule a b = if length_non_terminal a < length_non_terminal b then a else b

let rec find_path_symbol grammaire = function
    | [] -> failwith "No path"
    | (obj,path)::_ when obj=grammaire.axiome -> path
    | (obj,path)::q -> let rules = List.filter (fun r -> List.mem obj r.partiedroite) grammaire.regles in
        (find_path_symbol [@tailcall]) grammaire (q@(List.map (fun r -> (r.elementgauche, r::path)) rules))

let rec derive_with_path grammaire = function
    | [] -> failwith "No derivation with path"
    | (w, path)::q when is_word w -> assert (List.length path = 0); w
    | (w, path)::q -> let rules = possible_rules w grammaire.regles in
        if List.length path = 0 || not (List.mem (List.hd path) rules) then
            (derive_with_path [@tailcall]) grammaire (q@(List.map (fun r -> (left_derivation w r, path)) rules))
        else
            (derive_with_path [@tailcall]) grammaire (q@[left_derivation w (List.hd path), List.tl path])

let derive_word_with_symbol grammaire s = derive_with_path grammaire [[grammaire.axiome],find_path_symbol grammaire [s,[]]]

let rec find_path_symbol2 (grammaire : rec_grammar) : (element*rec_rule list) list -> rec_rule list = function
    | [] -> failwith "No path"
    | (obj,path)::_ when obj=element_of_tree grammaire.axiom -> path
    | (obj,path)::q -> let rules = List.filter (fun r -> List.exists (fun t -> obj = element_of_tree t) r.right_part) grammaire.rules in
        (find_path_symbol2 [@tailcall]) grammaire (q@(List.map (fun r -> (element_of_tree r.left_symbol, r::path)) rules))

let rec derive_with_path2 (grammaire : rec_grammar) : (tree_state list * rec_rule list) list -> partie = function
    | [] -> failwith "No derivation with path"
    | (w, path)::q when is_word2 w -> assert (List.length path = 0); word_of_trees w
    | (w, path)::q -> let rules = possible_rules2 w grammaire.rules in
        if List.length path = 0 || not (List.mem (List.hd path) rules) then
            (derive_with_path2 [@tailcall]) grammaire (q@(List.map (fun r -> (left_derivation2 w r, path)) rules))
        else
            (derive_with_path2 [@tailcall]) grammaire (q@[left_derivation2 w (List.hd path), List.tl path])

let derive_word_with_symbol2 (grammaire : rec_grammar) (s : element) : element list = derive_with_path2 grammaire [[grammaire.axiom],find_path_symbol2 grammaire [s,[]]]


