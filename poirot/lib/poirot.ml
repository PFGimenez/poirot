type grammar = Grammar.grammar
type part = Grammar.part
type oracle_status = Oracle.oracle_status

let clean = Clean.clean
let clean_grammar = Clean.clean_grammar

let oracle_mem_from_script = Oracle.oracle_mem_from_script
let quotient_mem = Rec_quotient.quotient_mem
let grammar_of_ext_grammar = Grammar.grammar_of_ext_grammar
let oracle = Fuzzer.oracle
let search = Inference.search
let fuzzer = Tree_fuzzer.fuzzer
let string_of_grammar = Grammar.string_of_grammar
let string_of_ext_grammar = Grammar.string_of_ext_grammar
let read_bnf_grammar = Grammar_io.read_bnf_grammar
let read_tokens = Grammar_io.read_tokens
let read_token = Grammar_io.read_token
let string_of_word = Grammar.string_of_word
let string_of_oracle_status = Oracle.string_of_oracle_status
