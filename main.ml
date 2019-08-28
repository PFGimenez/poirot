open Base
open Reconstruct
open Parser

let ()=
	if Array.length Sys.argv = 6 then
        let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2)
        and pre = Sys.argv.(3)
        and su = Sys.argv.(4)
        and inte = Sys.argv.(5) in
        print_string "Grammaire lue\n";
        let intepart = string2partie inte in
        if List.length intepart != 1 then
            print_string "Un unique objectif est nécessaire"
        else
            let prefix = string2partie pre
            and suffix = string2partie su
            and interest = List.hd intepart in

            if not (isTerminal interest) || not (is_symbol_accessible interest grammaire) then
                print_string "Objectif inconnu !\n"
            else
                let blackbox = blackbox prefix suffix grammaire in
                let injectionToken = get_injection_leaves blackbox grammaire in
                if List.length injectionToken = 0 then
                    print_string "Pas de token d'injection !\n"
                else
                    print_string "Injection token:\n";
                    ignore (List.map (fun t -> match t with Leaf(e) -> print_string ("  "^(element2string e)^"\n") | _ -> failwith "error") injectionToken);
                    let gt = find_grammar blackbox interest grammaire injectionToken in
                    print_string ("Injection:\n  "^(partie2string (getInjection interest gt))^"\n")

    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <axiome> <prefixe> <suffixe> <objectif>\n")
