type grammar = Grammar.grammar
type element = Grammar.element
type oracle_status = Oracle.oracle_status

(*let search_with_fuzzer_script ?(max_depth: int = 10) ?(max_steps: int = 1000) ?(sgraph_fname: string option = None) ?(qgraph_fname: string option = None) (fuzzer: grammar -> oracle_status) (g: grammar) (goal: element) (start: element list) : grammar option =

    let qgraph_channel = Option.map open_out qgraph_fname in
    let h_fname = ((string_of_int (Hashtbl.hash g + Hashtbl.hash goal))^".prt") in
    Option.map Grammar.grammar_of_ext_grammar (Inference.search fuzzer g goal (Some start) max_depth max_steps forbidden_chars sgraph_fname qgraph_channel h_fname)
*)


let search ?(oneline_comment: string option = None) ?(subst: (element,string) Hashtbl.t option = None) ?(max_depth: int = 10) ?(max_steps: int = 1000) ?(forbidden_chars: char list = []) ?(sgraph_fname: string option = None) ?(qgraph_fname: string option = None) (oracle: string option -> oracle_status) (g: grammar) (goal: element) (start: element list) : (grammar * string) option =
    let qgraph_channel = Option.map open_out qgraph_fname in
    let h_fname = ((string_of_int (Hashtbl.hash g + Hashtbl.hash goal))^".prt") in
    match Inference.search oracle g goal (Some start) oneline_comment subst max_depth max_steps sgraph_fname qgraph_channel h_fname forbidden_chars with
    | None -> None
    | Some (g,w) -> Some ((Grammar.grammar_of_ext_grammar g), w)

let make_oracle_from_script ?(timeout: float option = Some 5.) (fname: string) = Oracle.oracle_mem_from_script timeout fname

let make_oracle_from_fun (f: string -> int) = Oracle.oracle_mem (fun s -> Oracle.oracle_status_of_int (f s))

let read_subst : string -> (element,string) Hashtbl.t = Grammar_io.read_subst

let quotient ?(qgraph_fname: string option = None) (g: grammar) (prefix: element list) (suffix: element list) : (grammar * string option) =
    let qgraph_channel = Option.map open_out qgraph_fname in
    Option.iter (fun ch -> output_string ch "digraph {\n") qgraph_channel;
    let g,inj = Quotient.quotient_mem (Clean.clean_grammar g) [] None None None qgraph_channel {pf=List.rev prefix;e=g.axiom;sf=suffix} in
    let g2 = Grammar.grammar_of_ext_grammar (Clean.clean (g)) in
    Option.iter (fun ch -> output_string ch "}"; close_out ch) qgraph_channel;
    (g2,Option.map Grammar.string_of_part inj)

let fuzzer ?(subst: (element,string) Hashtbl.t option = None) ?(forbidden_chars: char list = []) ?(complexity: int = 10) ?(goal: element option = None) (g: grammar) : string option =
    Option.map Grammar.string_of_word (Fuzzer.fuzzer complexity subst goal forbidden_chars g)

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

let set_log_level (lvl: Logs.level option) : unit = Logs.Src.set_level Log.poirotsrc lvl
let set_reporter (r: Logs.reporter) : unit = Logs.set_reporter r;
