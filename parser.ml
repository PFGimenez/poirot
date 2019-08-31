open Base

(** Alias pour la lib Str (expressions régulières) **)

(* remplacer input par output *)
let replace input output =
    Str.global_replace (Str.regexp_string input) output

(* Exploser une chaîne de caractère en fonction d'une expression régulière ou d'une sous chaine *)
let split regexp source = Str.split (Str.regexp regexp) source

(** Traitement/Conversion du fichier décrivant la grammaire **)
(* Renvoie un booléen indiquant si la chaîne de caractère représente un non terminal (si elle commence par une majuscule) *)
let is_string_non_terminal word =
	let is_uppercase = function
	| 'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'
	|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z' -> true
	| _ -> false in
	is_uppercase (String.get word 0)

(* Transforme une chaîne de caractère en liste de tokens *)
let string2tokens = split " +"

(* transforme une liste de token en partie de règle : tail recursive *)
let rec token2partie acc = function
	| [] -> List.rev acc
	| x::rest when is_string_non_terminal x-> token2partie (Nonterminal(x)::acc) rest
	| x::rest -> token2partie (Terminal(x)::acc) rest

(* transforme une chaîne de caractère en règle *)
let string2regle input =
	let (partiegauche,partiedroite) =
	match split "->" input with
	[g;d] -> (g,d)
	|[g] -> (g,"")
	| _ -> failwith "Regle mal formée" in
	let listegauche = string2tokens partiegauche in
	let listedroite = string2tokens partiedroite in
	(List.hd (token2partie [] listegauche))-->(token2partie [] listedroite)

(* transforme une liste de chaînes de caractères en liste de règles *)
let rec strings2regles acc = function
	| [] -> List.rev acc
	| regle::rest -> strings2regles ((string2regle regle)::acc) rest

(* lis un fichier et renvoie une liste de chaînes de caractères composée des lignes du fichier *)
let read_file filename =
	if (filename="") then [] else
	let canal_entree = open_in filename in
	let rec readfile canal =
		try
			List.append (readfile canal) [replace "\t" "" (input_line canal)]
		with End_of_file -> [] in
	List.rev (readfile canal_entree)

(* extraction d'une grammaire depuis le nom de fichier et l'axiome passés en arguments *)
let read_grammar_from_file fichier axiome =
	let string_rules = read_file fichier in
	Nonterminal(axiome) @@ (strings2regles [] string_rules)

let string2partie s = token2partie [] (string2tokens s)
