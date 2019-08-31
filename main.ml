open Base
open Blind
open Parser

let a()=
    let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2) in
    let path = find_path_symbol grammaire [Terminal("homme"),[]] in
    List.iter (fun r -> print_string ((regle2string r)^"\n")) path; flush stdout;
    let w = derive_with_path grammaire [[grammaire.axiome],path] in
    print_string (partie2string w)

let ()=
	if Array.length Sys.argv = 6 then
        let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2)
        and prefix = string2partie (Sys.argv.(3))
        and suffix = string2partie (Sys.argv.(4))
        and intepart = string2partie (Sys.argv.(5)) in
        print_string "Grammaire lue\n";

        if List.length intepart != 1 then
            if List.length intepart = 0 then begin
                print_string "Au moins un objectif nécessaire\n"
            end else begin
                print_string "Pas plus d'un objectif !\n"
            end
        else
            let interest = List.hd intepart in

            if not (is_symbol_accessible grammaire interest) then
                print_string "Objectif inconnu !\n"
            else
                let blackbox = blackbox prefix suffix grammaire in
                let injectionToken = get_injection_leaves blackbox grammaire in
                if List.length injectionToken = 0 then
                    print_string "Pas de token d'injection !\n"
                else
                    print_string "Injection token:\n";
                    List.iter (fun (p,e,s) -> print_string ("  "^(element2string e)^"\n")) injectionToken;
                    let g = search_api blackbox interest grammaire injectionToken in match g with
                    | None -> print_string "Pas de grammaire trouvée\n"
                    | Some(g2) -> print_string ("Injection:\n  "^(partie2string (derive_word_with_symbol g2 interest))^"\n")

    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <axiome> <prefixe> <suffixe> <objectif>\n")
