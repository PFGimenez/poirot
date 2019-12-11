open Base

(** Algorithme de génération de la grammar des injections **)


(* Alias pour la conversion int vers string *)
let int2string = string_of_int

(* Generation du nom du nouveau non terminal, fonction du numéro de l'itération et du nom précédent *)
let etiquette nom numero = nom ^ "_" ^ (int2string numero)

let quotient_prefix_string str prefix = 
    if String.length prefix > String.length str then None else
    let b = Str.string_match (Str.regexp (Str.quote prefix)) str 0 in match b with
    | false -> None
    | true -> Some(Str.string_after str (String.length prefix))

let quotient_suffix_string str suffix = 
    if String.length suffix > String.length str then None else
    let pos = String.length str - String.length suffix in
    let b = Str.string_match (Str.regexp (Str.quote suffix)) str pos in match b with
    | false -> None
    | true -> Some(Str.string_before str pos)

(* Quotient à Gauche d'une règle pour l'itération "numéro" par le terminal "terminal"
   => renvoie (nouvellesrègles,nouvelaxiome) *)
let left_quotient_of_rule quotient_string numero terminal axiome = function

    (* A -> t alpha avec t terminal *)
    | {left_symbol = Nonterminal(a);right_part=t::alpha } when t=terminal && is_terminal t
        -> (* print_string ("t alpha "^(partie2string (t::alpha))^"\n"); *)
            ([ (Nonterminal(etiquette a numero))-->alpha ;
                (Nonterminal(a))-->(t::alpha) ],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)

    (* A -> t alpha avec t terminal *)
    | {left_symbol = Nonterminal(a);right_part=t::alpha } when t=terminal && is_terminal t
        -> (* print_string ("t alpha "^(partie2string (t::alpha))^"\n"); *)
            ([ (Nonterminal(etiquette a numero))-->alpha ;
                (Nonterminal(a))-->(t::alpha) ],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)

    (* A -> t alpha avec t terminal préfixe *)
(*    | {left_symbol = Nonterminal(a);right_part=t::alpha } when is_terminal t && quotient_string (element2string t) (element2string terminal) <> None
        -> (* print_string ("t alpha "^(partie2string (t::alpha))^"\n"); *)
            ([ (Nonterminal(etiquette a numero))-->alpha ;
                (Nonterminal(a))-->(t::alpha) ],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None) *)

    (* A -> t alpha avec t non-terminal *)
    | {left_symbol = Nonterminal(a);right_part=t::alpha } when t=terminal
        ->  (* print_string ("nt alpha "^(partie2string (t::alpha))^"\n"); *)
            ([ (Nonterminal(etiquette a numero))-->alpha ;
                (Nonterminal(etiquette a numero))-->(Nonterminal(etiquette (element2string t) numero)::alpha) ;
                (Nonterminal(a))-->(t::alpha) ],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)



    (* A -> B alpha *)
    | {left_symbol = Nonterminal(a);right_part=(Nonterminal(b))::alpha }
        -> (* print_string ("b alpha "^(partie2string (Nonterminal(b)::alpha))^"\n"); *)
            ([(Nonterminal(etiquette a numero))-->((Nonterminal(etiquette b numero))::alpha) ;
                (Nonterminal(a))-->((Nonterminal(b))::alpha)],
                if (axiome=Nonterminal(a)) then Some(Nonterminal(etiquette a numero)) else None)

	(* autre *)	  
    | autreregle
        -> (* print_string ("other "^(partie2string (autreregle.right_part))^"\n"); *)
            ([autreregle],None)

(* Inverser la partie droite d'une règle *)
let reverse_ext_right_part = function
	| {left_symbol=gauche;right_part=droite} -> {left_symbol=gauche;right_part=List.rev droite}

(* Quotient à droite de rule = inversionright_part (quotient à gauche de (inversionright_part rule)) (ouf) *)
let right_quotient_of_rule numero terminal axiome rule =
	let (r,a) = (left_quotient_of_rule quotient_suffix_string numero terminal axiome (reverse_ext_right_part rule)) in
	(List.map reverse_ext_right_part r,a)

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
let quotient_and_nettoyage quotientregle iteration terminal grammar =
	let {axiome=axiome;regles=regles} = grammar in
	Nettoyage.nettoyage (quotient quotientregle ([],axiome) iteration terminal axiome regles)

(* Algorithme de génération de la grammar complète pour la requête
WARNING : les grammaires générées sont à nettoyer, car elles impliquent énormément de backtracking lors de la dérivation du mot.
	 Penser à appliquer :
	-> suppression des eps production
	-> suppression des symboles inaccessibles
	-> suppression des symboles inutiles
*)

let rec generate_blind_grammar quotient iteration grammar = function
	| [] -> grammar
    | x::rest -> (* print_string ("quotient par "^element2string2(x)^"\n");*) let g=(quotient_and_nettoyage quotient iteration x grammar) in (* print_grammar g;*) (generate_blind_grammar [@tailcall]) quotient (iteration+1) g rest

let generate_blind_grammar_both_sides prefixe suffixe grammar =
    let g=generate_blind_grammar (left_quotient_of_rule quotient_prefix_string) 1 (Nettoyage.nettoyage grammar) prefixe in
    generate_blind_grammar right_quotient_of_rule 100 g (List.rev suffixe)

let rec generate_blind_grammar2 grammars ext_element grammar =
    let g = Hashtbl.find_opt grammars ext_element in match g with
    | None -> let g2 = grammar (* TODO *) in Hashtbl.add grammars ext_element g2; g2
    | Some(g2) -> g2

