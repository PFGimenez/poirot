let print_bool = function
            | true -> print_string "true\n"
            | false -> print_string "false\n"

(** Type definition **)
(* An element represents a rule element that can be either a Terminal or a Nonterminal *)
type element = 	| Terminal of string
		| Nonterminal of string

(* A part represents a list of elements. It can either be a right-hand side of a rule or an intermediate derivation *)
type partie = element list

(* An axiom with a prefix and a suffix *)
type ext_element = partie * element * partie

(* Une règle est composée d'une partie gauche (un élément) et d'une partie droite *)
type rule =  { left_symbol : element; right_part : partie }

(* A grammar is composed of an axiom (an element) and a list of rules *)
type grammar = {axiome: element; regles : rule list }

type ext_part = ext_element list

type ext_rule = {ext_left_symbol : ext_element; ext_right_part : ext_part}

type ext_rules = ext_rule list

type ext_grammar = {axiom: ext_element; rules: ext_rules}

let (--->) g d = {ext_left_symbol=g;ext_right_part=d}

let ext_element_of_element e = ([],e,[])

let trim2 (pre,e,suf) = ([],e,[])

let element_of_ext_element ((pre,e,suf) : ext_element) : element = e

let word_of_ext_elements (ext_element_list: ext_element list) : partie = List.map element_of_ext_element ext_element_list

let ext_rule_of_rule r = (ext_element_of_element r.left_symbol) ---> (List.map ext_element_of_element r.right_part)

let ext_grammar_of_grammar g = {axiom = ext_element_of_element g.axiome; rules = List.map ext_rule_of_rule g.regles}

let is_ext_element_in_rules t r = List.exists t (List.map (fun r -> r.ext_left_symbol) r)

(** Fonctions de conversion en chaîne de caractères **)

(* Conversion d'un élément en chaîne de caractères *)
let element2string = function
		| Terminal(x) -> x
		| Nonterminal(x) -> x

let quoted_element2string = function
		| Terminal(x) -> "\""^(String.escaped x)^"\""
		| Nonterminal(x) -> x



let string_of_element = element2string

let element2string2 = function
		| Terminal(x) -> "(T) "^x
		| Nonterminal(x) -> "(NT) "^x

let is_terminal = function
    | Terminal(_) -> true
    | _ -> false

let is_non_terminal s = not (is_terminal s)

let is_ext_element_terminal (pre,s,suf) = is_terminal s

let is_ext_element_non_terminal (pre,s,suf) = not (is_terminal s)

(* Conversion d'une partie en chaîne de caractères *)
let concat_with_delimiter d s1 s2 = s1 ^ d ^ s2

let concat_space = concat_with_delimiter " "

let concat_new_line = concat_with_delimiter "\n"

let partie2string = function
    | t::q -> List.fold_left concat_space (element2string t) (List.map element2string q)
    | [] -> "ε"

let quoted_partie2string = function
    | t::q -> List.fold_left concat_space (quoted_element2string t) (List.map quoted_element2string q)
    | [] -> ""

let string_of_ext_element (l,s,r) = let str=element2string s in match l,r with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (partie2string l) ^ "],[" ^ (partie2string r) ^ "]"

let quoted_string_of_ext_element (l,s,r) = let str=quoted_element2string s in match l,r with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (quoted_partie2string l) ^ "],[" ^ (quoted_partie2string r) ^ "]"

let string_of_ext_part = function
    | t::q -> List.fold_left concat_space (string_of_ext_element t) (List.map string_of_ext_element q)
    | [] -> "ε"

let quoted_string_of_ext_part = function
    | t::q -> List.fold_left concat_space (quoted_string_of_ext_element t) (List.map quoted_string_of_ext_element q)
    | [] -> ""

let string_of_ext_rule r = (string_of_ext_element r.ext_left_symbol) ^ " -> " ^ (string_of_ext_part r.ext_right_part)

let quoted_string_of_ext_rule r = (quoted_string_of_ext_element r.ext_left_symbol) ^ " ::= " ^ (quoted_string_of_ext_part r.ext_right_part)^";"

let string_of_ext_rules = function
    | t::q -> List.fold_left concat_new_line (string_of_ext_rule t) (List.map string_of_ext_rule q)
    | [] -> "(no rules)"

let quoted_string_of_ext_rules = function
    | t::q -> List.fold_left concat_new_line (quoted_string_of_ext_rule t) (List.map quoted_string_of_ext_rule q)
    | [] -> ""

let bnf_string_of_ext_grammar (g : ext_grammar) : string = (quoted_string_of_ext_element g.axiom) ^ ";\n" ^ (quoted_string_of_ext_rules g.rules) ^ "\n"

let bnf_string_of_grammar (g : grammar) : string = bnf_string_of_ext_grammar (ext_grammar_of_grammar g)

let string_of_ext_grammar (g : ext_grammar) : string = "Axiom: " ^ (string_of_ext_element g.axiom) ^ "\nRules: " ^ (string_of_ext_rules g.rules)
(* Conversion d'une règle en chaîne de caractère *)
let regle2string : rule -> string = function
	| {left_symbol=g;right_part=d} -> element2string g ^ " --> " ^ partie2string d

(* Conversion d'une liste de règles en chaîne de caractère *)
let rec reglelist2string = function
    | [] -> "Nothing to display."
    | h::[] -> regle2string h
    | h::t -> regle2string h ^ "\n" ^ reglelist2string t


(** Fonctions utilitaire **)

(* L'opérateur --> permet de créer une règle à la volée (facilité syntaxique) à partie de deux parties *)
let (-->) g d = {left_symbol=g;right_part=d}

(* Création d'une grammar à la volée *)
let (@@) axiome regles = {axiome=axiome;regles=regles}

let (@@@) axiome regles = {axiom=axiome;rules=regles}


(* Récupération d'une monade option contenant le premier non terminal d'une partie (si il existe !) *)
let rec first_non_terminal = function
	| [] -> None
	| Terminal(x)::rest -> first_non_terminal rest
	| Nonterminal(x)::rest -> Some(Nonterminal(x))

let rec first_non_terminal2 : ext_element list -> ext_element option = function
	| [] -> None
	| (pre,Terminal(x),suf)::rest -> first_non_terminal2 rest
	| (pre,Nonterminal(x),suf)::rest -> Some((pre,Nonterminal(x),suf))

let string_inst_of_element (values : (element, string) Hashtbl.t) : element -> string  = function
    | s when Hashtbl.mem values s -> Hashtbl.find values s
    | s -> string_of_element s

let string_inst_of_part (values : (element, string) Hashtbl.t) : element list -> string = function
    | t::q -> List.fold_left concat_space (string_inst_of_element values t) (List.map (string_inst_of_element values) q)
    | [] -> "ε"

let print_words (w : partie list) : unit = List.iter (fun r -> print_string ("Mot: "^(partie2string r)^"\n")) w

(** Fonctions d'affichage **)

(* Affichage d'une liste de règles *)
let print_rules regles = List.iter (Printf.printf "%s\n") (List.map regle2string regles)

(* Affichage d'une grammar *)
let print_grammar grammar = Printf.printf "Axiome : %s \nRegles : \n" (element2string grammar.axiome);
				  print_rules grammar.regles

(* TODO *)
let equals_grammars (g1 : ext_grammar) (g2 : ext_grammar) : bool = true

let rec print_grammars = function
    | [] -> print_string "(vide)"
    | t::[] -> print_grammar t
    | t::q -> print_grammar t; print_grammars q


let rec read_part (part : (bool*string) list) (output : element list) : partie = match part with
    | [] -> List.rev output
    | t::q when fst t -> (read_part [@tailcall]) q (Terminal(snd t)::output)
    | t::q -> (read_part [@tailcall]) q (Nonterminal(snd t)::output)

let rec read_rules (rules : ((bool*string) * ((bool*string) list)) list) (output : rule list) : rule list = match rules with
    | [] -> output
    | (n,l)::q -> assert (not (fst n)); (read_rules [@tailcall]) q ((Nonterminal(snd n) --> read_part l [])::output)

let convert_grammar (tokens : ((bool*string) * (((bool*string) * ((bool*string) list)) list))) : grammar =
    assert (not (fst (fst tokens))); (* the axiom must be a nonterminal *)
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
