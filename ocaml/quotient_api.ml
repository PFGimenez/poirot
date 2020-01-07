open Grammar

let ()=
    let injg_fname = ref None in
    let grammar = ref None in
    let prefix = ref None in
    let suffix = ref None in

    let speclist = [
        ("-grammar",    Arg.String (fun s -> grammar := Some(Grammar_io.read_bnf_grammar s)),     "Target grammar");
        ("-pf",         Arg.String (fun s -> prefix := Some(Grammar_io.read_tokens s)),     "Prefix of the request");
        ("-sf",         Arg.String (fun s -> suffix := Some(Grammar_io.read_tokens s)),     "Suffix of the request");
        ("-injg",       Arg.String (fun s -> injg_fname := Some(s)),     "Save the final grammar")
    ] in
    let usage = "Error: grammar, prefix and suffix are necessary" in
    Arg.parse speclist ignore usage;

    if !grammar <> None && !prefix <> None && !suffix <> None then
        let grammar = Option.get !grammar
        and prefix = Option.get !prefix
        and suffix = Option.get !suffix in
        let q = Rec_quotient.quotient_mem grammar in
        let inj_g = q {e=grammar.axiom; pf=List.rev prefix; sf=suffix} in
        print_endline (string_of_ext_grammar inj_g);
        Option.iter (fun f -> Grammar_io.export_bnf f inj_g) !injg_fname
    else print_endline usage
