(** Définitions des types **)
(* Le type element représente un élément de règle, et peut être soit Terminal soit Nonterminal *)
type element = 	| Terminal of string
		| Nonterminal of string

(* Une partie représente une liste d'éléments.
 Elle peut représenter une partie de règle ou un mot intermédiaire dans une dérivation *)
type partie = element list

(* Une règle est composée d'une partie gauche et d'une partie droite, ces *)
type regle =  { partiegauche : partie; partiedroite : partie }

(* Une grammaire est composée d'un élément axiome et d'une liste de règles *)
type grammaire = {axiome: element; regles : regle list }

(* Type permettant de définir une requête : Mot est un mot de la requête et Entree désigne le point d'injection *)
type requete = | Mot of string
	       | Entree

(** Fonctions de conversion en chaîne de caractères **)

(* Conversion d'un élément en chaîne de caractères *)
let element2string = function
		| Terminal(x) -> x
		| Nonterminal(x) -> x

(* Conversion d'une partie en chaîne de caractères *)
let partie2string partie = List.fold_left (^) "" (List.map element2string partie)


(* Conversion d'une règle en chaîne de caractère *)
let regle2string = function
	| {partiegauche=g;partiedroite=d} -> partie2string g ^ " --> " ^ partie2string d



(** Fonctions utilitaire **)

(* L'opérateur --> permet de créer une règle à la volée (facilité syntaxique) à partie de deux parties *)
let (-->) g d = {partiegauche=g;partiedroite=d}

(* Création d'une grammaire à la volée *)
let (@) axiome regles = {axiome=axiome;regles=regles}

(* Dérivation gauche d'un mot intermédiaire "dérivation" par une règle "regle" *)
let rec derivationGauche derivation regle =
	let {partiegauche=base;partiedroite=transformation} = regle in
	match derivation with
	| [] -> []
	| Nonterminal(x)::rest when [Nonterminal(x)]=base -> List.append transformation rest
	| x::rest -> x::derivationGauche rest regle

(* Récupération d'une monade option contenant le premier non terminal d'une partie (si il existe !) *)
let rec premierNonTerminal = function
	| [] -> None
	| Terminal(x)::rest -> premierNonTerminal rest
	| Nonterminal(x)::rest -> Some(Nonterminal(x))

(* Renvoie toutes les règles possibles parmi un liste de règles pour dériver motintermediaire *)
let rec reglesPossibles motintermediaire regles =
	let premier = premierNonTerminal motintermediaire in
	match premier with
	| None -> []
	| Some(x) ->
	match regles with
	| [] -> []
	| {partiegauche=gauche;partiedroite=droite}::rest when Some(x) = premierNonTerminal gauche ->
		gauche-->droite::(reglesPossibles motintermediaire rest)
	| _::rest -> reglesPossibles motintermediaire rest

(* Prédicat indiquant si le mot intermédiaire en argument est un mot (soit si il ne contient que des terminaux) *)
let rec estMot = function
	| [] -> true
	| Terminal(x)::rest -> estMot rest
	| Nonterminal(x)::rest -> false


(* Dérive et affiche tout les mots possibles pour une grammaire donnée, un mot de départ et une profondeur *)
(* /!\ Faire une version générique renvoyant une liste ! *)
let rec deriverTout profondeur grammaire motintermediaire =
	if estMot motintermediaire then Printf.printf "Mot : %s%!\n" (partie2string motintermediaire)
	else if (profondeur != 1) then
		let possibles = reglesPossibles motintermediaire grammaire.regles in
		let rec deriverLesPossibles grammaire mot = function
			| [] -> ()
			| regle::reste -> deriverTout (profondeur-1) grammaire (derivationGauche mot regle);
					  deriverLesPossibles grammaire mot reste in
		deriverLesPossibles grammaire motintermediaire possibles
	else ()

(* DeriverTout depuis l'axiome de la grammaire fournie *)
let deriver profondeur grammaire = deriverTout profondeur grammaire [grammaire.axiome]


(** Fonctions d'affichage **)

(* Affichage d'une partie (équivalent à l'affichage d'un mot intermédiaire) *)
let afficherPartie partie = Printf.printf "%s\n" (partie2string partie)
let afficherMot = afficherPartie

(* Affichage d'une liste de règles *)
let afficherRegles regles = List.iter (Printf.printf "%s\n") (List.map regle2string regles)

(* Affichage d'une grammaire *)
let afficherGrammaire grammaire = Printf.printf "Axiome : %s \nRegles : \n" (element2string grammaire.axiome);
				  afficherRegles grammaire.regles
