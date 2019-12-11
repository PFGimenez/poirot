open Base


(* Ancienne version, écrite par Romain *)

(** Fonctions de suppression des doublons dans les listes **)

let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let remove_duplicates xs = List.rev (List.fold_left cons_uniq [] xs)

(** Algorithme de nettoyage des symboles inaccessibles **)

let rec extract_non_terminal_part = function
	|[] -> []
	| Terminal(x)::rest -> extract_non_terminal_part rest
	| Nonterminal(x)::rest -> Nonterminal(x) :: extract_non_terminal_part rest

let rec get_reachable = function
	{elementgauche=_;partiedroite=d} -> extract_non_terminal_part d

let rec reachable_symbols = function
	|[] -> []
	|rule::rest -> (get_reachable rule) @ (reachable_symbols rest)

let rec reachable_rules_from_non_terminal nt = function
	| [] -> []
    |{elementgauche=g;partiedroite=d}::rest when g=nt ->
			g-->d::(reachable_rules_from_non_terminal nt rest)
	|_::rest -> (reachable_rules_from_non_terminal nt rest)

let rec reachable_rules regles = function
	| [] -> []
	| x::rest -> (reachable_rules_from_non_terminal x regles) @ (reachable_rules regles rest)

let uniq_reachable_rules regles ensemblent = remove_duplicates (reachable_rules regles ensemblent)
let reachable_symbolsSansDoublons listeregles = remove_duplicates (reachable_symbols listeregles)

let rec algoAccessibles reglesdepart accregles accsymbole =
	let regles = uniq_reachable_rules reglesdepart accsymbole in
	let newregles = remove_duplicates (accregles @ regles) in
	let symboles = reachable_symbolsSansDoublons regles in
	let newsymboles = remove_duplicates (accsymbole @ symboles) in
	if (newsymboles = accsymbole) then newregles
	else algoAccessibles reglesdepart newregles newsymboles

let clean_unreachable_rules grammar =
	(* Printf.printf "==Nettoyage des règles inaccessibles==\n"; *)
	let {axiome=axiome;regles=regles}=grammar in
	axiome @@ (algoAccessibles regles [] [axiome])

(** Algorithme de nettoyage des règles inutiles **)

let rule_directly_useful = function
	| {elementgauche = _;partiedroite = d} when None = first_non_terminal d -> true
	| _ -> false

let rec appartient x = function
	| [] -> false
	| e::rest when e=x -> true
	| _::rest -> appartient x rest

let useful_rule nonterminal rule = appartient nonterminal rule.partiedroite

let useful_rules_non_terminal regles nonterminal = List.filter (useful_rule nonterminal) regles

let rec useful_rules regles = function
	| [] -> []
	| x::rest -> (useful_rules_non_terminal regles x) @ (useful_rules regles rest)

let rec useful_symbols = function
	| [] -> []
	| x::rest -> x.elementgauche :: (useful_symbols rest)

let rec algoUtile regles r s =
	let newsymboles = remove_duplicates (List.append (useful_symbols r) s) in
	(*Printf.printf "%s\n" (partie2string newsymboles); DEBUG *)
	let newregles = remove_duplicates (List.append r (useful_rules regles newsymboles)) in
	if (newsymboles = s) then newregles
	else algoUtile regles newregles newsymboles

let get_useful_rules regles =
	let r = List.filter rule_directly_useful regles in
	algoUtile regles r []

let clean_useless_rules grammar =
	(* Printf.printf "==Nettoyage des règles inutiles==\n"; *)
	let {axiome=axiome;regles=regles}=grammar in
	axiome @@ (get_useful_rules regles)

(* Autre algorithme de tri une règle contient à droite un symbole qui n'apparaît jamais à gauche, on peut la retire *)

let rec get_all_left_elements = function
    | [] -> []
    | r::t -> r.elementgauche :: (get_all_left_elements t)

let rec check_useful_symbols left_parts = function
    | [] -> true
    | Terminal(s)::t -> check_useful_symbols left_parts t
    | h::t -> List.mem h left_parts && check_useful_symbols left_parts t

let check_useful_symbols_rule left_parts r = check_useful_symbols left_parts r.partiedroite

let clean_useless_symbols grammar =
	(* Printf.printf "==Nettoyage des symboles inutiles==\n"; *)
    let left_parts=List.sort_uniq compare (get_all_left_elements grammar.regles) in
    grammar.axiome@@(List.filter (check_useful_symbols_rule left_parts) grammar.regles)

(** Algorithme de tri des règles pour faciliter la dérivation gauche **)
let weight_compare un deux =
	match (un,deux) with
	| ((p1,_),(p2,_)) when p1=p2 -> 0
	| ((p1,_),(p2,_)) when p1>p2 -> 1
	| ((p1,_),(p2,_)) when p1<p2 -> -1
	| _ -> failwith "Exhaustivité du pattern"

let count_non_terminal rule = let {elementgauche=g;partiedroite=d} = rule in
		   let poids = List.fold_left (+) 0 (List.map (fun x->match x with | Terminal(_) -> 0 | Nonterminal(_) -> 1) d) in (poids,rule)

(* utilité ? *)

let sort_rules regles = let weighted_list = List.map count_non_terminal regles in
			 let triee = List.sort weight_compare weighted_list in
			 List.map (fun x -> let (_,r) = x in r) triee

let sort_grammar grammar =
	grammar.axiome @@ (sort_rules grammar.regles)

(** Epsilon production **)

let rec compute_epsilonAux regles reglesAux epsPlus =
match reglesAux with
| [] -> epsPlus
| {elementgauche=gauche;partiedroite=droite}::rest ->
		if( (not(List.mem gauche epsPlus)) && (((List.length droite) = 0) ||
		 (List.for_all (fun x -> (List.mem x epsPlus)) droite) )) then
		  compute_epsilonAux regles regles (gauche::epsPlus)
		  else compute_epsilonAux regles rest epsPlus

let compute_epsilon regles = compute_epsilonAux regles regles []

let remove_duplicate l =
	let rec loopAux acu = function
		| [] -> acu
		| x::rest -> if(List.mem x rest) then loopAux acu rest else loopAux (x::acu) rest
	in loopAux [] l

let remove_epsilon grammar =
(* Printf.printf "Nombre d'element produisant epsilon : %d \n" (List.length (compute_epsilon grammar.regles)); *)
let rec loop regles axiome = function
| [] -> {axiome = axiome ; regles = remove_duplicate regles}
| element::restElementsEps -> let rec loop1 newRegles axiome = function
		| [] -> loop newRegles axiome restElementsEps
		| {elementgauche=gauche;partiedroite=droite}::restRegles ->
		if( List.mem element droite ) then
			begin
				(* Printf.printf "Duplication \n";*)
				loop1 (({elementgauche = gauche ; partiedroite = (List.filter (fun x -> x <> element) droite)})::((gauche-->droite)::newRegles)) axiome restRegles
			end
		else if ( (element = gauche) && (droite=[]) ) then
			begin
				if (element = axiome) then
					begin
						(* Printf.printf "S-> Eps, Nouvel axiome \n"; *)
						let naxiome = match axiome with
							| Terminal(x) -> Terminal(x ^ "'")
							| Nonterminal(x) -> Nonterminal(x ^ "'")
						in
						loop1 ((naxiome-->[axiome])::(naxiome-->[])::newRegles) naxiome restRegles
					end
				else
					begin
						(* Printf.printf "X -> Eps, rien faire\n" ; *)
						loop1 newRegles axiome restRegles
					end
			end
		else
			begin
				if (not(List.mem ({elementgauche = gauche ; partiedroite = droite}) newRegles)) then
					begin
					  (* Printf.printf "Ajout de la règle telle quelle \n" ; *)
					  loop1 ((gauche-->droite)::newRegles) axiome restRegles
					end
				else loop1 newRegles axiome restRegles
			end
	 in loop1 [] axiome regles
in loop grammar.regles grammar.axiome (compute_epsilon grammar.regles)

(** Algorithme de nettoyage général**)
let nettoyage grammar =
	sort_grammar (clean_unreachable_rules (clean_useless_rules (clean_useless_symbols grammar)))
