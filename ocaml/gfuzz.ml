let oracle_from_script (fname: string) (inj: string) : bool =
    let cmd = fname^" '"^inj^"'" in
    let out =  (Sys.command cmd) == 0 in
    print_endline ("Call to oracle: "^cmd^": "^(string_of_bool out));
    out

let ()=
    if Array.length Sys.argv = 5 then
        let grammar = Grammar_io.read_bnf_grammar Sys.argv.(1)
        and goal = List.hd (Grammar_io.read_tokens Sys.argv.(2))
        and max_depth = int_of_string Sys.argv.(3)
        and oracle_filename = Sys.argv.(4) in

        let fuzzer = Tree_fuzzer.fuzzer in

        let fuzzer_oracle (g: Grammar.grammar) : bool = g |> fuzzer |> Grammar.string_of_word |> oracle_from_script oracle_filename in

        let g = Blind.search fuzzer_oracle grammar goal max_depth in match g with
        | None -> print_endline "No grammar found"
        | Some(inj_g) -> print_endline ("Injection:  "^(Grammar.string_of_word (fuzzer (Grammar.grammar_of_ext_grammar inj_g))))
    else print_endline ("Usage : "^Sys.argv.(0)^" <input BNF filename> <goal> <max depth> <oracle script filename>")
