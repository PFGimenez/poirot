open Base

(* renvoie les règles dont la partie droite contient l'élément cherché *)

let trouveRegles grammaire elem = List.filter (fun r -> List.mem elem r.partiedroite) grammaire.regles

let trouveParents r = List.sort_uniq compare (List.map (fun r -> List.hd r.partiegauche) r)
