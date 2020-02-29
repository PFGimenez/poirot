type grammar = Grammar.grammar
type element = Grammar.element
type oracle_status = Oracle.oracle_status

let search ?(verbose: bool = false) ?(subst: (element,string) Hashtbl.t option = None) ?(max_depth: int = 10) ?(max_steps: int = 1000) ?(forbidden_chars: char list = []) ?(sgraph_fname: string option = None) ?(qgraph_fname: string option = None) (oracle: string option -> oracle_status)  (g: grammar) (goal: element) (start: element list) : grammar option =

    let fuzzer_oracle (subst: (element,string) Hashtbl.t option) (goal: element option) (oracle: string option -> oracle_status) : (grammar -> Oracle.oracle_status) =
        fun g -> g |> Fuzzer.fuzzer 0 subst goal verbose |> Option.map Grammar.string_of_word |> oracle in

    let qgraph_channel = Option.map open_out qgraph_fname in
    Option.map Grammar.grammar_of_ext_grammar (Inference.search (fuzzer_oracle subst (Some goal) oracle) g goal (Some start) max_depth max_steps forbidden_chars sgraph_fname qgraph_channel verbose)

let make_oracle_from_script ?(verbose: bool = false) (fname: string) = Oracle.oracle_mem_from_script fname verbose

let make_oracle_from_fun ?(verbose: bool = false) (f: string -> int) = Oracle.oracle_mem verbose (fun s -> Oracle.oracle_status_of_int (f s))

let read_subst : string -> (element,string) Hashtbl.t = Grammar_io.read_subst

let quotient (g: grammar) (prefix: element list) (suffix: element list) : grammar =
    Grammar.grammar_of_ext_grammar (Clean.clean (Quotient.quotient_mem (Clean.clean_grammar g) None {pf=List.rev prefix;e=g.axiom;sf=suffix}))

let fuzzer ?(subst: (element,string) Hashtbl.t option = None) ?(complexity: int = 10) ?(goal: element option = None) (g: grammar) : string option =
    Option.map Grammar.string_of_word (Fuzzer.fuzzer complexity subst goal false g)

let apply_and_simplify (simplify: bool) (g: grammar) (f: grammar -> grammar) : grammar =
    if simplify then Clean.simplify (f g) else (f g)

let to_uppercase ?(simplify: bool = false) (g: grammar) = apply_and_simplify simplify g Clean.to_uppercase
let to_lowercase ?(simplify: bool = false) (g: grammar) = apply_and_simplify simplify g Clean.to_lowercase

let set_axiom (g: grammar) (axiom: element) : grammar = {axiom; rules=g.rules}

let string_of_grammar : grammar -> string = Grammar.string_of_grammar
let read_bnf_grammar ?(unravel: bool = false) : string -> grammar = Grammar_io.read_bnf_grammar unravel
let read_tokens ?(unravel: bool = false) :  string -> element list = Grammar_io.read_tokens unravel
let read_token ?(unravel: bool = false) :  string -> element = Grammar_io.read_token unravel

let export_antlr4 : string -> grammar -> unit = Grammar_io.export_antlr4
