open Base
open Blind
open Parser
open Clean
open Quotient

let ()=
    let grammaire = Parser.read_grammar_from_file Sys.argv.(1) in
    let path = find_path_symbol grammaire [Terminal("homme"),[]] in
    List.iter (fun r -> print_string ((regle2string r)^"\n")) path; flush stdout;
    let w = derive_with_path grammaire [[grammaire.axiome],path] in
    print_string (partie2string w)


