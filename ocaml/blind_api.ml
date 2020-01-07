(* TODO: avec un vrai fuzzer, passer fuzzer et oracle de "part" à "string" *)

let ()=
    if Array.length Sys.argv = 7 then
        let grammar = Grammar_io.read_bnf_grammar Sys.argv.(1)
        and prefix = Grammar_io.read_tokens Sys.argv.(2)
        and suffix = Grammar_io.read_tokens Sys.argv.(3)
        and goal = List.hd (Grammar_io.read_tokens Sys.argv.(4))
        and max_depth = int_of_string Sys.argv.(5)
        and filename = Sys.argv.(6) in

        let values = Hashtbl.create 100 in
        Hashtbl.add values (Grammar.Terminal("value")) "val1";
        let oracle = Fuzzer.oracle prefix suffix grammar and
(*       let oracle = Oracle.oracle and*)
(*        let oracle = Tree_fuzzer.parenth_oracle prefix suffix and*)
        fuzzer = Tree_fuzzer.fuzzer in
(*        ignore (Tree_fuzzer.fuzzer grammar);
        exit 0;*)

        let g = Blind.search fuzzer oracle grammar goal max_depth in match g with
        | None -> print_endline "No grammar found"
        | Some(inj_g) -> print_endline ("Injection:  "^(Fuzzer.string_inst_of_part values (List.hd (fuzzer (Grammar.grammar_of_ext_grammar inj_g))))); Grammar_io.export_bnf filename inj_g
    else print_endline ("Usage : "^Sys.argv.(0)^" <input BNF filename> <prefix> <suffix> <goal> <max depth> <output BNF filename>")
