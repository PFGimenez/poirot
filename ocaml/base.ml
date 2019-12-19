(** Type definitions **)

(* An element represents a rule element that can be either a Terminal or a Nonterminal *)
type element =
        | Terminal of string
		| Nonterminal of string

(* A part represents a list of elements. It can either be a right-hand side of a rule or an intermediate derivation *)
type part = element list

(* An element with a prefix and a suffix *)
type ext_element = {pf: part; e: element; sf: part}

(* Une règle est composée d'une part gauche (un élément) et d'une part droite *)
type rule =  { left_symbol : element; right_part : part }

(* A grammar is composed of an ext_axiom (an element) and a list of ext_rules *)
type grammar = {axiom: element; rules : rule list }

type ext_part = ext_element list

type ext_rule = {ext_left_symbol : ext_element; ext_right_part : ext_part}

type ext_grammar = {ext_axiom: ext_element; ext_rules: ext_rule list}

let (--->) g d = {ext_left_symbol=g;ext_right_part=d}

let ext_element_of_element e = {pf=[]; e=e; sf=[]}

let element_of_ext_element (e : ext_element) : element = e.e

let word_of_ext_elements (ext_element_list: ext_element list) : part = List.map element_of_ext_element ext_element_list

let ext_rule_of_rule r = (ext_element_of_element r.left_symbol) ---> (List.map ext_element_of_element r.right_part)

let ext_grammar_of_grammar g = {ext_axiom = ext_element_of_element g.axiom; ext_rules = List.map ext_rule_of_rule g.rules}

(* let is_ext_element_in_rules t r = List.exists t (List.map (fun r -> r.ext_left_symbol) r) *)

(** Fonctions de conversion en chaîne de caractères **)

(* Conversion d'un élément en chaîne de caractères *)
let element2string = function
		| Terminal(x) -> x
		| Nonterminal(x) -> x

let quoted_element2string = function
		| Terminal(x) -> "\""^(String.escaped x)^"\""
		| Nonterminal(x) -> x

let print_bool = function
            | true -> print_endline "true"
            | false -> print_endline "false"



let string_of_element = element2string

let element2string2 = function
		| Terminal(x) -> "(T) "^x
		| Nonterminal(x) -> "(NT) "^x

let is_terminal = function
    | Terminal(_) -> true
    | _ -> false

let is_non_terminal s = not (is_terminal s)

let is_ext_element_terminal e = is_terminal e.e

let is_ext_element_non_terminal e = not (is_terminal e.e)

let get_all_symbols (g: grammar) : element list =
    List.sort_uniq compare ((List.map (fun r -> r.left_symbol) g.rules) @ (List.flatten (List.map (fun r -> r.right_part) g.rules)))

(* Conversion d'une part en chaîne de caractères *)
let concat_with_delimiter d s1 s2 = s1 ^ d ^ s2

let concat_space = concat_with_delimiter " "

let concat_new_line = concat_with_delimiter "\n"

let part2string = function
    | t::q -> List.fold_left concat_space (element2string t) (List.map element2string q)
    | [] -> "ε"

let quoted_part2string = function
    | t::q -> List.fold_left concat_space (quoted_element2string t) (List.map quoted_element2string q)
    | [] -> ""

let string_of_ext_element e = let str=element2string e.e in match e.pf,e.sf with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (part2string e.pf) ^ "],[" ^ (part2string e.sf) ^ "]"

let quoted_string_of_ext_element e = let str=quoted_element2string e.e in match e.pf,e.sf with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (quoted_part2string e.pf) ^ "],[" ^ (quoted_part2string e.sf) ^ "]"

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

let string_of_ext_grammar (g : ext_grammar) : string = "axiom: " ^ (string_of_ext_element g.ext_axiom) ^ "\nRules: " ^ (string_of_ext_rules g.ext_rules)

(* Conversion d'une règle en chaîne de caractère *)
let rule2string : rule -> string = function
	| {left_symbol=g;right_part=d} -> element2string g ^ " --> " ^ part2string d

(* Conversion d'une liste de règles en chaîne de caractère *)
(* let rec reglelist2string = function
    | [] -> "Nothing to display."
    | h::[] -> rule2string h
    | h::t -> rule2string h ^ "\n" ^ reglelist2string t *)


(** Fonctions utilitaire **)

(* L'opérateur --> permet de créer une règle à la volée (facilité syntaxique) à part de deux parties *)
let (-->) g d = {left_symbol=g;right_part=d}

(* Création d'une grammar à la volée *)
let (@@) axiom rules = {axiom=axiom;rules=rules}

let (@@@) axiom rules = {ext_axiom=axiom;ext_rules=rules}

(* Récupération d'une monade option contenant le premier non terminal d'une part (si il existe !) *)
let rec first_non_terminal = function
	| [] -> None
	| Terminal(x)::rest -> first_non_terminal rest
	| Nonterminal(x)::rest -> Some(Nonterminal(x))

let rec first_non_terminal2 : ext_element list -> ext_element option = function
	| [] -> None
    | {pf=pre; e=Terminal(x); sf=suf}::rest -> first_non_terminal2 rest
	| e::rest -> Some(e)

let string_inst_of_element (values : (element, string) Hashtbl.t) : element -> string  = function
    | s when Hashtbl.mem values s -> Hashtbl.find values s
    | s -> string_of_element s

let string_inst_of_part (values : (element, string) Hashtbl.t) : element list -> string = function
    | t::q -> List.fold_left concat_space (string_inst_of_element values t) (List.map (string_inst_of_element values) q)
    | [] -> "ε"

let print_words (w : part list) : unit = List.iter (fun r -> print_endline ("Mot: "^(part2string r))) w

(** Fonctions d'affichage **)

(* Affichage d'une liste de règles *)
let print_rules rules = List.iter (Printf.printf "%s\n") (List.map rule2string rules)

(* Affichage d'une grammar *)
let print_grammar grammar = Printf.printf "axiom : %s \nRegles : \n" (element2string grammar.axiom);
				  print_rules grammar.rules

let rec print_grammars = function
    | [] -> print_endline "(vide)"
    | t::[] -> print_grammar t
    | t::q -> print_grammar t; print_grammars q
