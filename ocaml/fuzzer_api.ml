open Base
open Fuzzer
open Nettoyage

let ()=
    let grammaire = Base.read_bnf_grammar Sys.argv.(1) in
    let order = Fuzzer.get_order (nettoyage grammaire) in
    print_string ((partie2string order)^"\n")
(*    derive_within_lengthPrint 10 grammaire;*)
(*    let path = find_path_symbol grammaire [Terminal(Sys.argv.(2)),[]] in
    List.iter (fun r -> print_string ((regle2string r)^"\n")) path; flush stdout *)
(*    let w = derive_with_path grammaire [[grammaire.axiome],path] in
    print_string ((partie2string w)^"\n")*)


