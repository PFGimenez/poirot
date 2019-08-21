open Base
open Nettoyage

(** Algorithme de génération de la grammaire des injections **)


(* Alias pour la conversion int vers string *)
let int2string = string_of_int

(* Generation du nom du nouveau non terminal, fonction du numéro de l'itération et du nom précédent *)
let etiquette nom numero = nom ^ "." ^ (int2string numero)

(* Quotient à Gauche d'une règle pour l'itération "numéro" par le terminal "terminal"
   => renvoie (nouvellesrègles,nouvelaxiome) *)
let quotientGaucheRegle numero terminal axiome = function
	(* A -> B alpha *) | {partiegauche = [Nonterminal(a)];partiedroite=(Nonterminal(b))::alpha } ->
	([[Nonterminal(etiquette a numero)]-->((Nonterminal(etiquette b numero))::alpha) ;
	 [Nonterminal(a)]-->((Nonterminal(b))::alpha)],
	if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)
	(* A -> t alpha *) | {partiegauche = [Nonterminal(a)];partiedroite=t::alpha } when t=terminal ->
	([ [Nonterminal(etiquette a numero)]-->alpha ;
	   [Nonterminal(a)]-->(t::alpha) ],
	if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)
			   (*| {partiegauche = [Nonterminal(a)];partiedroite=[]} -> 
	([([Nonterminal(etiquette a numero)]-->[]);([Nonterminal(a)]-->[])],if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)*)
	(* autre *)	   | autreregle -> ([autreregle],None)

(* Inverser la partie droite d'une règle *)
let inverserOrdrePartieDroite = function
	| {partiegauche=gauche;partiedroite=droite} -> {partiegauche=gauche;partiedroite=List.rev droite}

(* Quotient à droite de regle = inversionPartieDroite (quotient à gauche de (inversionPartieDroite regle)) (ouf) *)
let quotientDroiteRegle numero terminal axiome regle =
	let (r,a) = (quotientGaucheRegle numero terminal axiome (inverserOrdrePartieDroite regle)) in
	(List.map inverserOrdrePartieDroite r,a)

(* Quotient générique pour plusieurs règles où fquotientregle est la fonction quotientRegle à appliquer *)
let rec quotient fquotientregle acc iteration terminal axiome = function
	| [] -> let (r,a) = acc in {axiome=a;regles=r}
	| x::rest -> let (newregles,newaxiome) = (fquotientregle iteration terminal axiome x) in
		     let (oldregles,oldaxiome) = acc in
		     let accaxiome = match newaxiome with
			| None -> oldaxiome
			| Some(a) -> a
		     in
		     let accregles = List.append newregles oldregles in
		     quotient fquotientregle (accregles,accaxiome) iteration terminal axiome rest

(* Quotient à gauche pour plusieurs règles par un terminal "terminal" *)
let quotientGauche iteration terminal grammaire =
	let {axiome=axiome;regles=regles} = grammaire in
	nettoyage (quotient quotientGaucheRegle ([],axiome) iteration terminal axiome regles)

(* Quotient à droite pour plusieurs règles par un terminal "terminal" *)
let quotientDroite iteration terminal grammaire =
	let {axiome=axiome;regles=regles} = grammaire in
	nettoyage (quotient quotientDroiteRegle ([],axiome) iteration terminal axiome regles)

(* Algorithme de génération de la grammaire complète pour la requête
WARNING : les grammaires générées sont à nettoyer, car elles impliquent énormément de backtracking lors de la dérivation du mot.
	 Penser à appliquer :
	-> suppression des eps production
	-> suppression des symboles inaccessibles
	-> suppression des symboles inutiles
*)
let rec genererNouvelleGrammaire analysedroite iteration grammaire = function
	| [] -> grammaire
	| Mot(x)::rest when analysedroite=false-> genererNouvelleGrammaire false (iteration+1) (quotientGauche iteration (Terminal(x)) grammaire) rest
	| Mot(x)::rest when analysedroite=true -> genererNouvelleGrammaire true (iteration+1) (quotientDroite iteration (Terminal(x)) grammaire) rest
	| Entree::rest -> genererNouvelleGrammaire true iteration grammaire (List.rev rest)
	| _ -> failwith "Cas inconnu"

(* Interface *)
let genererGrammaireInjection = genererNouvelleGrammaire false 1
