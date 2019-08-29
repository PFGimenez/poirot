open Base
open Blind
open Parser

let ()=
	if Array.length Sys.argv = 6 then
        let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2)
        and prefix = string2partie (Sys.argv.(3))
        and suffix = string2partie (Sys.argv.(4))
        and intepart = string2partie (Sys.argv.(5)) in
        print_string "Grammaire lue\n";

        if List.length intepart != 1 then
            print_string "Un unique objectif est nécessaire\n"
        else
            let interest = List.hd intepart in

            if not (isTerminal interest) || not (is_symbol_accessible grammaire interest) then
                print_string "Objectif inconnu ou non-terminal !\n"
            else
                let blackbox = blackbox prefix suffix grammaire in
                let injectionToken = get_injection_leaves blackbox grammaire in
                if List.length injectionToken = 0 then
                    print_string "Pas de token d'injection !\n"
                else
                    print_string "Injection token:\n";
                    ignore (List.map (fun (p,e,s) -> print_string ("  "^(element2string e)^"\n")) injectionToken);
                    let g = search blackbox interest grammaire [] injectionToken in match g with
                    | None -> print_string "Pas de grammaire trouvée\n"
                    | Some(g2) -> print_string ("Injection:\n  "^(partie2string (derive_word_with_symbol g2 interest))^"\n")

    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <axiome> <prefixe> <suffixe> <objectif>\n")
