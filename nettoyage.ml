open Base

(** Fonctions de suppression des doublons dans les listes **)

let consUniq xs x = if List.mem x xs then xs else x :: xs
let supprimerDoublons xs = List.rev (List.fold_left consUniq [] xs)

(** Algorithme de nettoyage des symboles inaccessibles **)

let rec extraireNTPartie = function
	|[] -> []
	| Terminal(x)::rest -> extraireNTPartie rest
	| Nonterminal(x)::rest -> Nonterminal(x) :: extraireNTPartie rest

let rec extraireAccessibles = function
	{partiegauche=_;partiedroite=d} -> extraireNTPartie d

let rec symbolesAccessibles = function
	|[] -> []
	|regle::rest -> (extraireAccessibles regle) @ (symbolesAccessibles rest)

let rec reglesAccessiblesNT nt = function
	| [] -> []
	|{partiegauche=g;partiedroite=d}::rest when (premierNonTerminal g)=Some(nt) ->
			g-->d::(reglesAccessiblesNT nt rest)
	|_::rest -> (reglesAccessiblesNT nt rest)

let rec reglesAccessibles regles = function
	| [] -> []
	| x::rest -> (reglesAccessiblesNT x regles) @ (reglesAccessibles regles rest)

let reglesAccessiblesSansDoublons regles ensemblent = supprimerDoublons (reglesAccessibles regles ensemblent)
let symbolesAccessiblesSansDoublons listeregles = supprimerDoublons (symbolesAccessibles listeregles)

let rec algoAccessibles reglesdepart accregles accsymbole =
	let regles = reglesAccessiblesSansDoublons reglesdepart accsymbole in
	let newregles = supprimerDoublons (accregles @ regles) in
	let symboles = symbolesAccessiblesSansDoublons regles in
	let newsymboles = supprimerDoublons (accsymbole @ symboles) in
	if (newsymboles = accsymbole) then newregles
	else algoAccessibles reglesdepart newregles newsymboles

let nettoyerGrammaireReglesInaccessibles grammaire =
	(* Printf.printf "==Nettoyage des règles inaccessibles==\n"; *)
	let {axiome=axiome;regles=regles}=grammaire in
	axiome @@ (algoAccessibles regles [] [axiome])

(** Algorithme de nettoyage des règles inutiles **)

let regleDirectementUtile = function
	| {partiegauche = _;partiedroite = d} when None = premierNonTerminal d -> true
	| _ -> false

let reglesDirectementUtiles regles = List.filter regleDirectementUtile regles

let extrairePartieGauche = function
	| {partiegauche=g;partiedroite=_} -> g

let rec appartient x = function
	| [] -> false
	| e::rest when e=x -> true
	| _::rest -> appartient x rest

let regleUtile nonterminal regle = appartient nonterminal regle.partiedroite

let reglesUtilesNT regles nonterminal = List.filter (regleUtile nonterminal) regles

let rec reglesUtiles regles = function
	| [] -> []
	| x::rest -> (reglesUtilesNT regles x) @ (reglesUtiles regles rest)

let rec symbolesUtiles = function
	| [] -> []
	| x::rest -> (extrairePartieGauche x) @ (symbolesUtiles rest)

let rec algoUtile regles r s =
	let newsymboles = supprimerDoublons (List.append (symbolesUtiles r) s) in
	(*Printf.printf "%s\n" (partie2string newsymboles); DEBUG *)
	let newregles = supprimerDoublons (List.append r (reglesUtiles regles newsymboles)) in
	if (newsymboles = s) then newregles
	else algoUtile regles newregles newsymboles

let recupererReglesUtiles regles =
	let r = reglesDirectementUtiles regles in
	algoUtile regles r []

let nettoyerGrammaireReglesInutiles grammaire =
	(* Printf.printf "==Nettoyage des règles inutiles==\n"; *)
	let {axiome=axiome;regles=regles}=grammaire in
	axiome @@ (recupererReglesUtiles regles)

(* Autre algorithme de tri une règle contient à droite un symbole qui n'apparaît jamais à gauche, on peut la retire *)

let rec getToutesPartiesGauche = function
    | [] -> []
    | r::t -> r.partiegauche @ (getToutesPartiesGauche t)

let rec checkSymboleUtiles partiesGauche = function
    | [] -> true
    | Terminal(s)::t -> checkSymboleUtiles partiesGauche t
    | h::t -> List.mem h partiesGauche && checkSymboleUtiles partiesGauche t

let checkSymboleUtilesRegle partiesGauche r = checkSymboleUtiles partiesGauche r.partiedroite

let nettoyerGrammaireSymbolesInutiles grammaire =
	(* Printf.printf "==Nettoyage des symboles inutiles==\n"; *)
    let partiesGauche=List.sort_uniq compare (getToutesPartiesGauche grammaire.regles) in
    grammaire.axiome@@(List.filter (checkSymboleUtilesRegle partiesGauche) grammaire.regles)

(** Algorithme de tri des règles pour faciliter la dérivation gauche **)
let comparaisonPoids un deux =
	match (un,deux) with
	| ((p1,_),(p2,_)) when p1=p2 -> 0
	| ((p1,_),(p2,_)) when p1>p2 -> 1
	| ((p1,_),(p2,_)) when p1<p2 -> -1
	| _ -> failwith "Exhaustivité du pattern"

let countNT regle = let {partiegauche=g;partiedroite=d} = regle in
		   let poids = List.fold_left (+) 0 (List.map (fun x->match x with | Terminal(_) -> 0 | Nonterminal(_) -> 1) d) in (poids,regle)

(* utilité ? *)

let trierRegles regles = let weightedList = List.map countNT regles in
			 let triee = List.sort comparaisonPoids weightedList in
			 List.map (fun x -> let (_,r) = x in r) triee

let trierGrammaire grammaire =
	grammaire.axiome @@ (trierRegles grammaire.regles)

(** Epsilon production **)

let rec calculerEpsilonAux regles reglesAux epsPlus =
match reglesAux with
| [] -> epsPlus
| {partiegauche=gauche;partiedroite=droite}::rest ->
		if( (not(List.mem (List.hd gauche) epsPlus)) && (((List.length droite) = 0) ||
		 (List.for_all (fun x -> (List.mem x epsPlus)) droite) )) then
		  calculerEpsilonAux regles regles ((List.hd gauche)::epsPlus)
		  else calculerEpsilonAux regles rest epsPlus


let calculerEpsilon regles = calculerEpsilonAux regles regles []

let removeDuplicate l =
	let rec loopAux acu = function
		| [] -> acu
		| x::rest -> if(List.mem x rest) then loopAux acu rest else loopAux (x::acu) rest
	in loopAux [] l

let supprimerEpsilon grammaire =
(* Printf.printf "Nombre d'element produisant epsilon : %d \n" (List.length (calculerEpsilon grammaire.regles)); *)
let rec loop regles axiome = function
| [] -> {axiome = axiome ; regles = removeDuplicate regles}
| element::restElementsEps -> let rec loop1 newRegles axiome = function
		| [] -> loop newRegles axiome restElementsEps
		| {partiegauche=gauche;partiedroite=droite}::restRegles ->
		if( List.mem element droite ) then
			begin
				(* Printf.printf "Duplication \n";*)
				loop1 (({partiegauche = gauche ; partiedroite = (List.filter (fun x -> x <> element) droite)})::((gauche-->droite)::newRegles)) axiome restRegles
			end
		else if ( (List.mem element gauche) && (droite=[]) ) then
			begin
				if (element = axiome) then
					begin
						(* Printf.printf "S-> Eps, Nouvel axiome \n"; *)
						let naxiome = match axiome with
							| Terminal(x) -> Terminal(x ^ "'")
							| Nonterminal(x) -> Nonterminal(x ^ "'")
						in
						loop1 (([naxiome]-->[axiome])::([naxiome]-->[])::newRegles) naxiome restRegles
					end
				else
					begin
						(* Printf.printf "X -> Eps, rien faire\n" ; *)
						loop1 newRegles axiome restRegles
					end
			end
		else
			begin
				if (not(List.mem ({partiegauche = gauche ; partiedroite = droite}) newRegles)) then
					begin
					  (* Printf.printf "Ajout de la règle telle quelle \n" ; *)
					  loop1 ((gauche-->droite)::newRegles) axiome restRegles
					end
				else loop1 newRegles axiome restRegles
			end
	 in loop1 [] axiome regles
in loop grammaire.regles grammaire.axiome (calculerEpsilon grammaire.regles)



(** Algorithme de nettoyage général**)
let nettoyage grammaire =
	trierGrammaire (nettoyerGrammaireReglesInaccessibles (nettoyerGrammaireSymbolesInutiles (nettoyerGrammaireReglesInutiles (supprimerEpsilon grammaire))))
