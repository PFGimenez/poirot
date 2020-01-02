open Grammar

let quoted_string_of_element : element -> string = function
    | Terminal(x) -> "\""^(String.escaped x)^"\""
    | Nonterminal(x) -> x

let quoted_string_of_part : part -> string = string_of_list " " "" quoted_string_of_element

let quoted_string_of_ext_element (e: ext_element) : string = let str=quoted_string_of_element e.e in match e.pf,e.sf with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (quoted_string_of_part (List.rev e.pf)) ^ "|" ^ (quoted_string_of_part e.sf) ^ "]"

let quoted_string_of_ext_part : ext_part -> string = string_of_list " " "" quoted_string_of_ext_element

let quoted_string_of_ext_rule (r: ext_rule) : string = (quoted_string_of_ext_element r.ext_left_symbol) ^ " ::= " ^ (quoted_string_of_ext_part r.ext_right_part)^";"

let quoted_string_of_ext_rules : ext_rule list -> string = string_of_list "\n" "" quoted_string_of_ext_rule

let bnf_string_of_ext_grammar (g : ext_grammar) : string = (quoted_string_of_ext_element g.ext_axiom) ^ ";" ^ (quoted_string_of_ext_rules g.ext_rules)

let bnf_string_of_grammar (g : grammar) : string = bnf_string_of_ext_grammar (ext_grammar_of_grammar g)

let rec read_part (part : (bool*string) list) (output : element list) : part = match part with
    | [] -> List.rev output
    | t::q when fst t -> (read_part [@tailcall]) q (Terminal(snd t)::output)
    | t::q -> (read_part [@tailcall]) q (Nonterminal(snd t)::output)

let rec read_rules (ext_rules : ((bool*string) * ((bool*string) list)) list) (output : rule list) : rule list = match ext_rules with
    | [] -> output
    | (n,l)::q -> assert (not (fst n)); (read_rules [@tailcall]) q ((Nonterminal(snd n) --> read_part l [])::output)

let convert_grammar (tokens : ((bool*string) * (((bool*string) * ((bool*string) list)) list))) : grammar =
    assert (not (fst (fst tokens))); (* the ext_axiom must be a nonterminal *)
    Nonterminal(snd (fst tokens)) @@ read_rules (snd tokens) []

let read_bnf_grammar (filename : string) : grammar =
    let lexbuf = Lexing.from_channel (open_in filename) in
    convert_grammar (Parserbnf.start Lexerbnf.token lexbuf)

let rec read_tokens_from_ch (ch: Lexing.lexbuf) : element list =
    let token = Lexerbnf.token ch in match token with
    | Parserbnf.EOF -> []
    | Parserbnf.NTERM(b,str) -> Nonterminal(str)::(read_tokens_from_ch ch)
    | Parserbnf.TERM(b,str) -> Terminal(str)::(read_tokens_from_ch ch)
    | _ -> failwith "Error token"

let read_tokens (str : string) : element list =
    read_tokens_from_ch (Lexing.from_string str)
