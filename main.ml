open Base
open Reconstruct
open Parser

let ()=
	if Array.length Sys.argv = 6 then
        let grammaire = Parser.grammaireDepuisFichier Sys.argv.(1) Sys.argv.(2)
        and pre = Sys.argv.(3)
        and su = Sys.argv.(4)
        and inte = Sys.argv.(5) in

        let prefix = string2partie pre
        and suffix = string2partie su
        and interest = List.hd (string2partie inte) in

        let blackbox = blackbox prefix suffix grammaire in
        let injectionToken = get_injection_leaves blackbox grammaire in
        let gt = find_grammar blackbox interest grammaire injectionToken in
        print_string ((partie2string (getInjection interest gt))^"\n")

    else print_string ("Usage : "^Sys.argv.(0)^" <fichierGrammaire> <axiome> <prefixe> <suffixe> <objectif>\n")
