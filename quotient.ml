open Base
open Nettoyage

(** Algorithme de génération de la grammaire des injections **)


(* Alias pour la conversion int vers string *)
let int2string = string_of_int

(* Generation du nom du nouveau non terminal, fonction du numéro de l'itération et du nom précédent *)
let etiquette nom numero = nom ^ "." ^ (int2string numero)

(* Quotient à Gauche d'une règle pour l'itération "numéro" par le terminal "terminal"
   => renvoie (nouvellesrègles,nouvelaxiome) *)
let left_quotient_of_rule numero terminal axiome = function

    (* A -> t alpha avec t terminal *)
    | {elementgauche = Nonterminal(a);partiedroite=t::alpha } when t=terminal && is_terminal t
        -> (* print_string ("t alpha "^(partie2string (t::alpha))^"\n"); *)
            ([ (Nonterminal(etiquette a numero))-->alpha ;
                (Nonterminal(a))-->(t::alpha) ],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)

    (* A -> t alpha avec t non-terminal *)
    | {elementgauche = Nonterminal(a);partiedroite=t::alpha } when t=terminal
        ->  (* print_string ("nt alpha "^(partie2string (t::alpha))^"\n"); *)
            ([ (Nonterminal(etiquette a numero))-->alpha ;
                (Nonterminal(etiquette a numero))-->(Nonterminal(etiquette (element2string t) numero)::alpha) ;
                (Nonterminal(a))-->(t::alpha) ],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)



    (* A -> B alpha *)
    | {elementgauche = Nonterminal(a);partiedroite=(Nonterminal(b))::alpha }
        -> (* print_string ("b alpha "^(partie2string (Nonterminal(b)::alpha))^"\n"); *)
            ([(Nonterminal(etiquette a numero))-->((Nonterminal(etiquette b numero))::alpha) ;
                (Nonterminal(a))-->((Nonterminal(b))::alpha)],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)

	(* autre *)	  
    | autreregle
        -> (* print_string ("other "^(partie2string (autreregle.partiedroite))^"\n"); *)
            ([autreregle],None)

(* Inverser la partie droite d'une règle *)
let reverse_right_part = function
	| {elementgauche=gauche;partiedroite=droite} -> {elementgauche=gauche;partiedroite=List.rev droite}

(* Quotient à droite de regle = inversionPartieDroite (quotient à gauche de (inversionPartieDroite regle)) (ouf) *)
let right_quotient_of_rule numero terminal axiome regle =
	let (r,a) = (left_quotient_of_rule numero terminal axiome (reverse_right_part regle)) in
	(List.map reverse_right_part r,a)

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
		     (quotient [@tailcall]) fquotientregle (accregles,accaxiome) iteration terminal axiome rest

(* Quotient à droite pour plusieurs règles par un terminal "terminal" *)
let quotient_and_nettoyage quotientregle iteration terminal grammaire =
	let {axiome=axiome;regles=regles} = grammaire in
	nettoyage (quotient quotientregle ([],axiome) iteration terminal axiome regles)

(* Algorithme de génération de la grammaire complète pour la requête
WARNING : les grammaires générées sont à nettoyer, car elles impliquent énormément de backtracking lors de la dérivation du mot.
	 Penser à appliquer :
	-> suppression des eps production
	-> suppression des symboles inaccessibles
	-> suppression des symboles inutiles
*)

let rec generate_blind_grammar quotient iteration grammaire = function
	| [] -> grammaire
    | x::rest -> (* print_string ("quotient par "^element2string2(x)^"\n");*) let g=(quotient_and_nettoyage quotient iteration x grammaire) in (* print_grammar g;*) (generate_blind_grammar [@tailcall]) quotient (iteration+1) g rest

let generate_blind_grammar_both_sides prefixe suffixe grammaire =
    let g=generate_blind_grammar left_quotient_of_rule  1 (nettoyage grammaire) prefixe in
    generate_blind_grammar right_quotient_of_rule 100 g (List.rev suffixe)
