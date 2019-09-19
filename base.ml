open Hashtbl

(* TODO : renommer rec_* en ext_* *)

let print_bool = function
            | true -> print_string "true\n"
            | false -> print_string "false\n"

(** Définitions des types **)
(* Le type element représente un élément de règle, et peut être soit Terminal soit Nonterminal *)
type element = 	| Terminal of string
		| Nonterminal of string

(* Une partie représente une liste d'éléments.
 Elle peut représenter une partie de règle ou un mot intermédiaire dans une dérivation *)
type partie = element list

(* Un axiome et ses préfixe et suffixe *)
type tree_state = partie * element * partie

(* Une règle est composée d'une partie gauche et d'une partie droite, ces *)
type regle =  { elementgauche : element; partiedroite : partie }

(* Une grammaire est composée d'un élément axiome et d'une liste de règles *)
type grammaire = {axiome: element; regles : regle list }

type rec_part = tree_state list

type rec_rule = {left_symbol : tree_state; right_part : rec_part}

type rec_rules = rec_rule list

let (--->) g d = {left_symbol=g;right_part=d}

type rec_grammar = {axiom: tree_state; rules: rec_rules}

let tree_of_element e = ([],e,[])

let trim2 (pre,e,suf) = ([],e,[])

let element_of_tree ((pre,e,suf) : tree_state) : element = e

let word_of_trees (tree_list: tree_state list) : partie = List.map element_of_tree tree_list

let rec_rule_of_regle r = (tree_of_element r.elementgauche) ---> (List.map tree_of_element r.partiedroite)

let rec_grammar_of_grammar g = {axiom = tree_of_element g.axiome; rules = List.map rec_rule_of_regle g.regles}

let is_tree_in_rules t r = List.exists t (List.map (fun r -> r.left_symbol) r)

(** Fonctions de conversion en chaîne de caractères **)

(* Conversion d'un élément en chaîne de caractères *)
let element2string = function
		| Terminal(x) -> x
		| Nonterminal(x) -> x

let string_of_element = element2string

let element2string2 = function
		| Terminal(x) -> "(T) "^x
		| Nonterminal(x) -> "(NT) "^x

let is_terminal = function
    | Terminal(_) -> true
    | _ -> false

let is_tree_terminal (pre,s,suf) = is_terminal s

(* Conversion d'une partie en chaîne de caractères *)
let concat_with_delimiter d s1 s2 = s1 ^ d ^ s2

let concat_space = concat_with_delimiter " "

let concat_new_line = concat_with_delimiter "\n"

let partie2string = function
    | t::q -> List.fold_left concat_space (element2string2 t) (List.map element2string2 q)
    | [] -> "ε"

let string_of_tree (l,s,r) = let str=element2string s in match l,r with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (partie2string l) ^ "],[" ^ (partie2string r) ^ "]"

let string_of_rec_part = function
    | t::q -> List.fold_left concat_space (string_of_tree t) (List.map string_of_tree q)
    | [] -> "ε"

let string_of_rec_rule r = (string_of_tree r.left_symbol) ^ " ---> " ^ (string_of_rec_part r.right_part)

let string_of_rec_rules = function
    | t::q -> List.fold_left concat_new_line (string_of_rec_rule t) (List.map string_of_rec_rule q)
    | [] -> "(no rules)"

let string_of_rec_grammar g = "Axiom: " ^ (string_of_tree g.axiom) ^ "\nRules: " ^ (string_of_rec_rules g.rules)
(* Conversion d'une règle en chaîne de caractère *)
let regle2string = function
	| {elementgauche=g;partiedroite=d} -> element2string2 g ^ " --> " ^ partie2string d

(* Conversion d'une liste de règles en chaîne de caractère *)
let rec reglelist2string = function
    | [] -> "Nothing to display."
    | h::[] -> regle2string h
    | h::t -> regle2string h ^ "\n" ^ reglelist2string t


(** Fonctions utilitaire **)

(* L'opérateur --> permet de créer une règle à la volée (facilité syntaxique) à partie de deux parties *)
let (-->) g d = {elementgauche=g;partiedroite=d}

(* Création d'une grammaire à la volée *)
let (@@) axiome regles = {axiome=axiome;regles=regles}

let (@@@) axiome regles = {axiom=axiome;rules=regles}

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



(* Récupération d'une monade option contenant le premier non terminal d'une partie (si il existe !) *)
let rec first_non_terminal = function
	| [] -> None
	| Terminal(x)::rest -> first_non_terminal rest
	| Nonterminal(x)::rest -> Some(Nonterminal(x))

let rec first_non_terminal2 : tree_state list -> tree_state option = function
	| [] -> None
	| (pre,Terminal(x),suf)::rest -> first_non_terminal2 rest
	| (pre,Nonterminal(x),suf)::rest -> Some((pre,Nonterminal(x),suf))

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

let string_inst_of_element (values : (element, string) t) : element -> string  = function
    | s when Hashtbl.mem values s -> Hashtbl.find values s
    | s -> string_of_element s

let string_inst_of_part (values : (element, string) t) : element list -> string = function
    | t::q -> List.fold_left concat_space (string_inst_of_element values t) (List.map (string_inst_of_element values) q)
    | [] -> "ε"

let print_words (w : partie list) : unit = List.iter (fun r -> print_string ("Mot: "^(partie2string r)^"\n")) w

(** Fonctions d'affichage **)

(* Affichage d'une liste de règles *)
let print_rules regles = List.iter (Printf.printf "%s\n") (List.map regle2string regles)

(* Affichage d'une grammaire *)
let print_grammar grammaire = Printf.printf "Axiome : %s \nRegles : \n" (element2string grammaire.axiome);
				  print_rules grammaire.regles

let rec print_grammars = function
    | [] -> print_string "(vide)"
    | t::[] -> print_grammar t
    | t::q -> print_grammar t; print_grammars q


let rec read_part (part : (bool*string) list) : partie = match part with
    | [] -> []
    | t::q when fst t -> Terminal(snd t)::(read_part q)
    | t::q -> Nonterminal(snd t)::(read_part q)

let rec read_rules (rules : ((bool*string) * ((bool*string) list)) list) : regle list = match rules with
    | [] -> []
    | (n,l)::q -> assert (not (fst n)); (Nonterminal(snd n) --> read_part l)::read_rules q

let read_grammar (tokens : ((bool*string) * (((bool*string) * ((bool*string) list)) list))) : grammaire =
    assert (not (fst (fst tokens)));
    Nonterminal(snd (fst tokens)) @@ read_rules (snd tokens)
