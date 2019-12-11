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
	{left_symbol=_;right_part=d} -> extract_non_terminal_part d

let rec reachable_symbols = function
	|[] -> []
	|rule::rest -> (get_reachable rule) @ (reachable_symbols rest)

let rec reachable_rules_from_non_terminal nt = function
	| [] -> []
    |{left_symbol=g;right_part=d}::rest when g=nt ->
			g-->d::(reachable_rules_from_non_terminal nt rest)
	|_::rest -> (reachable_rules_from_non_terminal nt rest)

let rec reachable_rules rules = function
	| [] -> []
	| x::rest -> (reachable_rules_from_non_terminal x rules) @ (reachable_rules rules rest)

let uniq_reachable_rules rules ensemblent = remove_duplicates (reachable_rules rules ensemblent)
let reachable_symbolsSansDoublons listeregles = remove_duplicates (reachable_symbols listeregles)

let rec algoAccessibles reglesdepart accregles accsymbole =
	let rules = uniq_reachable_rules reglesdepart accsymbole in
	let newregles = remove_duplicates (accregles @ rules) in
	let symboles = reachable_symbolsSansDoublons rules in
	let newsymboles = remove_duplicates (accsymbole @ symboles) in
	if (newsymboles = accsymbole) then newregles
	else algoAccessibles reglesdepart newregles newsymboles

let clean_unreachable_rules grammar =
	(* Printf.printf "==Nettoyage des règles inaccessibles==\n"; *)
	let {axiom=axiom;rules=rules}=grammar in
	axiom @@ (algoAccessibles rules [] [axiom])

(** Algorithme de nettoyage des règles inutiles **)

let rule_directly_useful = function
	| {left_symbol = _;right_part = d} when None = first_non_terminal d -> true
	| _ -> false

let rec appartient x = function
	| [] -> false
	| e::rest when e=x -> true
	| _::rest -> appartient x rest

let useful_rule nonterminal rule = appartient nonterminal rule.right_part

let useful_rules_non_terminal rules nonterminal = List.filter (useful_rule nonterminal) rules

let rec useful_rules rules = function
	| [] -> []
	| x::rest -> (useful_rules_non_terminal rules x) @ (useful_rules rules rest)

let rec useful_symbols = function
	| [] -> []
	| x::rest -> x.left_symbol :: (useful_symbols rest)

let rec algoUtile rules r s =
	let newsymboles = remove_duplicates (List.append (useful_symbols r) s) in
	(*Printf.printf "%s\n" (partie2string newsymboles); DEBUG *)
	let newregles = remove_duplicates (List.append r (useful_rules rules newsymboles)) in
	if (newsymboles = s) then newregles
	else algoUtile rules newregles newsymboles

let get_useful_rules rules =
	let r = List.filter rule_directly_useful rules in
	algoUtile rules r []

let clean_useless_rules grammar =
	(* Printf.printf "==Nettoyage des règles inutiles==\n"; *)
	let {axiom=axiom;rules=rules}=grammar in
	axiom @@ (get_useful_rules rules)

(* Autre algorithme de tri une règle contient à droite un symbole qui n'apparaît jamais à gauche, on peut la retire *)

let rec get_all_left_elements = function
    | [] -> []
    | r::t -> r.left_symbol :: (get_all_left_elements t)

let rec check_useful_symbols left_parts = function
    | [] -> true
    | Terminal(s)::t -> check_useful_symbols left_parts t
    | h::t -> List.mem h left_parts && check_useful_symbols left_parts t

let check_useful_symbols_rule left_parts r = check_useful_symbols left_parts r.right_part

let clean_useless_symbols grammar =
	(* Printf.printf "==Nettoyage des symboles inutiles==\n"; *)
    let left_parts=List.sort_uniq compare (get_all_left_elements grammar.rules) in
    grammar.axiom@@(List.filter (check_useful_symbols_rule left_parts) grammar.rules)

(** Algorithme de tri des règles pour faciliter la dérivation gauche **)
let weight_compare un deux =
	match (un,deux) with
	| ((p1,_),(p2,_)) when p1=p2 -> 0
	| ((p1,_),(p2,_)) when p1>p2 -> 1
	| ((p1,_),(p2,_)) when p1<p2 -> -1
	| _ -> failwith "Exhaustivité du pattern"

let count_non_terminal rule = let {left_symbol=g;right_part=d} = rule in
		   let poids = List.fold_left (+) 0 (List.map (fun x->match x with | Terminal(_) -> 0 | Nonterminal(_) -> 1) d) in (poids,rule)

(* utilité ? *)

let sort_rules rules = let weighted_list = List.map count_non_terminal rules in
			 let triee = List.sort weight_compare weighted_list in
			 List.map (fun x -> let (_,r) = x in r) triee

let sort_grammar grammar =
	grammar.axiom @@ (sort_rules grammar.rules)

(** Epsilon production **)

let rec compute_epsilonAux rules reglesAux epsPlus =
match reglesAux with
| [] -> epsPlus
| {left_symbol=gauche;right_part=droite}::rest ->
		if( (not(List.mem gauche epsPlus)) && (((List.length droite) = 0) ||
		 (List.for_all (fun x -> (List.mem x epsPlus)) droite) )) then
		  compute_epsilonAux rules rules (gauche::epsPlus)
		  else compute_epsilonAux rules rest epsPlus

let compute_epsilon rules = compute_epsilonAux rules rules []

let remove_duplicate l =
	let rec loopAux acu = function
		| [] -> acu
		| x::rest -> if(List.mem x rest) then loopAux acu rest else loopAux (x::acu) rest
	in loopAux [] l

let remove_epsilon grammar =
(* Printf.printf "Nombre d'element produisant epsilon : %d \n" (List.length (compute_epsilon grammar.rules)); *)
let rec loop rules axiom = function
| [] -> {axiom = axiom ; rules = remove_duplicate rules}
| element::restElementsEps -> let rec loop1 newRegles axiom = function
		| [] -> loop newRegles axiom restElementsEps
		| {left_symbol=gauche;right_part=droite}::restRegles ->
		if( List.mem element droite ) then
			begin
				(* Printf.printf "Duplication \n";*)
				loop1 (({left_symbol = gauche ; right_part = (List.filter (fun x -> x <> element) droite)})::((gauche-->droite)::newRegles)) axiom restRegles
			end
		else if ( (element = gauche) && (droite=[]) ) then
			begin
				if (element = axiom) then
					begin
						(* Printf.printf "S-> Eps, Nouvel axiom \n"; *)
						let naxiom = match axiom with
							| Terminal(x) -> Terminal(x ^ "'")
							| Nonterminal(x) -> Nonterminal(x ^ "'")
						in
						loop1 ((naxiom-->[axiom])::(naxiom-->[])::newRegles) naxiom restRegles
					end
				else
					begin
						(* Printf.printf "X -> Eps, rien faire\n" ; *)
						loop1 newRegles axiom restRegles
					end
			end
		else
			begin
				if (not(List.mem ({left_symbol = gauche ; right_part = droite}) newRegles)) then
					begin
					  (* Printf.printf "Ajout de la règle telle quelle \n" ; *)
					  loop1 ((gauche-->droite)::newRegles) axiom restRegles
					end
				else loop1 newRegles axiom restRegles
			end
	 in loop1 [] axiom rules
in loop grammar.rules grammar.axiom (compute_epsilon grammar.rules)

(** Algorithme de nettoyage général**)
let nettoyage grammar =
	sort_grammar (clean_unreachable_rules (clean_useless_rules (clean_useless_symbols grammar)))
