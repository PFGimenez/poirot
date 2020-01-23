type grammar = Grammar.grammar
type element = Grammar.element

let search (values: (element,string) Hashtbl.t option) (oracle_fname: string)  (g: grammar) (goal: element) (start: element list option) (max_depth: int) (forbidden: char list) (deep_search: bool) (sgraph_fname: string option) (qgraph_channel: out_channel option) : grammar option =
    let fuzzer_oracle (values: (element,string) Hashtbl.t option) (goal: element option) (oracle_fname: string) : (grammar -> Oracle.oracle_status) = fun g -> g |> Tree_fuzzer.fuzzer 0 values goal |> Option.map Grammar.string_of_word |> Oracle.oracle_mem_from_script oracle_fname in
    Option.map Grammar.grammar_of_ext_grammar (Inference.search (fuzzer_oracle values (Some goal) oracle_fname) g goal start max_depth forbidden deep_search sgraph_fname qgraph_channel)

let quotient (g: grammar) (prefix: element list) (suffix: element list) : grammar =
    Grammar.grammar_of_ext_grammar (Rec_quotient.quotient_mem g None {pf=List.rev prefix;e=g.axiom;sf=suffix})

let string_of_grammar : grammar -> string = Grammar.string_of_grammar
let read_bnf_grammar : string -> grammar = Grammar_io.read_bnf_grammar
let read_tokens : string -> element list = Grammar_io.read_tokens
let read_token : string -> element = Grammar_io.read_token
