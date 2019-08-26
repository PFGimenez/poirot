open Base
open Nettoyage
open Quotient
open Reconstruct

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
		(genererGrammaireInjection (nettoyage grammaire) requete) in deriver 22 newgrammaire;
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

let () =
	let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2) in
    let r=trouveRegles grammaire (Nonterminal("Exe")) in
    let par=trouveParents r in
    print_string ("Règles:\n"^(reglelist2string r)^"\n");
    print_string ("Parents:\n"^(elementList2string par)^"\n");
    afficherGrammaire (genererGrammaireInjectionAveugle [Nonterminal("Msg")] [Terminal("value")] grammaire);
(*    afficherGrammaire (genererGrammaireInjectionAveugle [] [Nonterminal("Params")] (Nonterminal("Params")@grammaire.regles)); *)
(*    afficherGrammaire (genererGrammaireInjectionAveugle [Terminal("key")] [] (Nonterminal("Params")@grammaire.regles)); *)


(* LE PROBLEME VIENT DU NETTOYAGE DES GRAMMAIRES : A FAIRE APRES CHAQUE ITÉRATION !!!!! *)
