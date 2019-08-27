open Base
open Nettoyage
open Quotient
open Reconstruct
open Parser

let rec elementList2string = function
    | [] -> ""
    | x::t -> element2string x ^ (elementList2string t)

let rec requete2string = function
	| [] -> ""
	| Mot(x)::rest -> x ^ (requete2string rest)
	| Entree::rest -> "[Entree]" ^ (requete2string rest)

let rec genererInjections grammaire = function
	| [] -> ()
	| requete::rest -> 
		Printf.printf "===== Génération de mot pour la requête : %s\n" (requete2string requete);
		let newgrammaire = trierGrammaire
		(genererGrammaireInjection (nettoyage grammaire) requete) in deriverPrint 22 newgrammaire;
		genererInjections grammaire rest
let old() =
	if (Array.length Sys.argv = 4) then
	let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2) in
	(*afficherGrammaire (supprimerEpsilon grammaire);*)
	(*deriver 15 (trierGrammaire grammaire);*)
	genererInjections grammaire (Parser.requetesDepuisFichier Sys.argv.(3))
	else Printf.printf "Usage : %s <fichierGrammaire> <axiome> <fichierRequetes>\n" Sys.argv.(0);;
	(*afficherGrammaire newgrammaire;*)

	(*let grammairepropre = nettoyage newgrammaire in
	afficherGrammaire grammairepropre;
	deriver 15 grammairepropre*)

let string2partie s = token2partie [] (string2tokens s)

let ()=
	let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2) in

    let pre = Sys.argv.(3) in
    let su = Sys.argv.(4) in
    let inte = Sys.argv.(5) in
    let prefix = string2partie pre in
    let suffix = string2partie su in
    let interest = List.hd (string2partie inte) in
    let blackbox = blackbox prefix suffix grammaire in
    let injectionToken = get_injection_leaves blackbox grammaire in
    let gt = find_grammar blackbox interest grammaire injectionToken in
    (* afficherGrammaireTreesCombined interest gt *)
    print_string ((partie2string (getInjection interest gt))^"\n")

let old3()=
	let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2) in
    let t = Leaf(Terminal("value")) in
    let t2 = construct_trees grammaire t in
    let t3 = construct_trees_from_list grammaire t2 in
    let t4 = List.nth t3 2 in
    print_tree t4;
    let p,s = get_prefix_suffix_tree t4 in
    print_string ("Prefix: "^(partie2string p)^", suffix: "^(partie2string s)^"\n");
    afficherGrammaire (get_grammar_from_tree grammaire t4)

let old2() =
	let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2) in
    afficherGrammaire grammaire;
    (* let g=genererGrammaireInjectionAveugle [Terminal("msg");Terminal("key");Terminal("=")] [Terminal("&");Nonterminal("Params")] (Nonterminal("Msg")@@grammaire.regles) in *)
    let g=genererGrammaireInjectionAveugle [] [Terminal("&");Nonterminal("Params")] (Nonterminal("Msg")@@grammaire.regles) in
    afficherGrammaire g;
(*    afficherGrammaire (genererGrammaireInjectionAveugle [Nonterminal("Msg")] [Terminal("cmd")] grammaire); *)
(*    afficherGrammaire (genererGrammaireInjectionAveugle [] [Nonterminal("Params")] (Nonterminal("Params")@@grammaire.regles)); *)
(*    afficherGrammaire (genererGrammaireInjectionAveugle [Terminal("key")] [] (Nonterminal("Params")@@grammaire.regles)); *)
    deriverPrint 10 g;
    print_bool (is_symbol_accessible (Terminal("exec")) g)
