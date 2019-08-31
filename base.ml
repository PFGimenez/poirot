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

(* Une règle est composée d'une partie gauche et d'une partie droite, ces *)
type regle =  { elementgauche : element; partiedroite : partie }

(* Une grammaire est composée d'un élément axiome et d'une liste de règles *)
type grammaire = {axiome: element; regles : regle list }

(** Fonctions de conversion en chaîne de caractères **)

(* Conversion d'un élément en chaîne de caractères *)
let element2string = function
		| Terminal(x) -> x
		| Nonterminal(x) -> x

let element2string2 = function
		| Terminal(x) -> "(T) "^x^" "
		| Nonterminal(x) -> "(NT) "^x^" "

let element2string3 = function
		| Terminal(x) -> x^" "
		| Nonterminal(x) -> x^" "

let is_terminal = function
    | Terminal(x) -> true
    | _ -> false

(* Conversion d'une partie en chaîne de caractères *)
let partie2string partie = match partie with
    | [] -> "(vide)"
    | _ -> List.fold_left (^) "" (List.map element2string3 partie)


(* Conversion d'une règle en chaîne de caractère *)
let regle2string = function
	| {elementgauche=g;partiedroite=d} -> element2string g ^ " --> " ^ partie2string d

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

(* Dérivation gauche d'un mot intermédiaire "dérivation" par une règle "regle" *)
let rec left_derivation derivation regle =
	let {elementgauche=base;partiedroite=transformation} = regle in
	match derivation with
	| [] -> []
	| Nonterminal(x)::rest when Nonterminal(x)=base -> List.append transformation rest
	| x::rest -> x::left_derivation rest regle

(* Récupération d'une monade option contenant le premier non terminal d'une partie (si il existe !) *)
let rec first_non_terminal = function
	| [] -> None
	| Terminal(x)::rest -> first_non_terminal rest
	| Nonterminal(x)::rest -> Some(Nonterminal(x))

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

(* Prédicat indiquant si le mot intermédiaire en argument est un mot (soit si il ne contient que des terminaux) *)
let rec is_word = function
	| [] -> true
	| Terminal(x)::rest -> is_word rest
	| Nonterminal(x)::rest -> false

(* /!\ Faire une version générique renvoyant une liste ! *)
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

let print_words w = List.iter (fun r -> print_string ("Mot: "^(partie2string r)^"\n")) w

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
