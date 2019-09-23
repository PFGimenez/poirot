open Hashtbl
open Parserbnf

(* TODO : renommer rec_* en ext_* *)

let print_bool = function
            | true -> print_string "true\n"
            | false -> print_string "false\n"

(** Définitions des types **)
(* Le type element représente un élément de règle, et peut être soit Terminal soit Nonterminal *)
type element = 	| Terminal of string
		| Nonterminal of string

(* Une partie représente une liste d'éléments.
 Elle peut représenter une partie de règle ou un mot intermédiaire dans une dérivation *)
type partie = element list

(* Un axiome et ses préfixe et suffixe *)
type tree_state = partie * element * partie

(* Une règle est composée d'une partie gauche et d'une partie droite, ces *)
type regle =  { elementgauche : element; partiedroite : partie }

(* Une grammaire est composée d'un élément axiome et d'une liste de règles *)
type grammaire = {axiome: element; regles : regle list }

type rec_part = tree_state list

type rec_rule = {left_symbol : tree_state; right_part : rec_part}

type rec_rules = rec_rule list

let (--->) g d = {left_symbol=g;right_part=d}

type rec_grammar = {axiom: tree_state; rules: rec_rules}

let tree_of_element e = ([],e,[])

let trim2 (pre,e,suf) = ([],e,[])

let element_of_tree ((pre,e,suf) : tree_state) : element = e

let word_of_trees (tree_list: tree_state list) : partie = List.map element_of_tree tree_list

let rec_rule_of_regle r = (tree_of_element r.elementgauche) ---> (List.map tree_of_element r.partiedroite)

let rec_grammar_of_grammar g = {axiom = tree_of_element g.axiome; rules = List.map rec_rule_of_regle g.regles}

let is_tree_in_rules t r = List.exists t (List.map (fun r -> r.left_symbol) r)

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

let is_tree_terminal (pre,s,suf) = is_terminal s

let is_tree_non_terminal (pre,s,suf) = not (is_terminal s)

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

let string_of_tree (l,s,r) = let str=element2string s in match l,r with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (partie2string l) ^ "],[" ^ (partie2string r) ^ "]"

let quoted_string_of_tree (l,s,r) = let str=quoted_element2string s in match l,r with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (quoted_partie2string l) ^ "],[" ^ (quoted_partie2string r) ^ "]"

let string_of_rec_part = function
    | t::q -> List.fold_left concat_space (string_of_tree t) (List.map string_of_tree q)
    | [] -> "ε"

let quoted_string_of_rec_part = function
    | t::q -> List.fold_left concat_space (quoted_string_of_tree t) (List.map quoted_string_of_tree q)
    | [] -> ""

let string_of_rec_rule r = (string_of_tree r.left_symbol) ^ " -> " ^ (string_of_rec_part r.right_part)

let quoted_string_of_rec_rule r = (quoted_string_of_tree r.left_symbol) ^ " ::= " ^ (quoted_string_of_rec_part r.right_part)^";"

let string_of_rec_rules = function
    | t::q -> List.fold_left concat_new_line (string_of_rec_rule t) (List.map string_of_rec_rule q)
    | [] -> "(no rules)"

let quoted_string_of_rec_rules = function
    | t::q -> List.fold_left concat_new_line (quoted_string_of_rec_rule t) (List.map quoted_string_of_rec_rule q)
    | [] -> ""

let bnf_string_of_rec_grammar (g : rec_grammar) : string = (quoted_string_of_tree g.axiom) ^ ";\n" ^ (quoted_string_of_rec_rules g.rules) ^ "\n"

let bnf_string_of_grammar (g : grammaire) : string = bnf_string_of_rec_grammar (rec_grammar_of_grammar g)

let string_of_rec_grammar (g : rec_grammar) : string = "Axiom: " ^ (string_of_tree g.axiom) ^ "\nRules: " ^ (string_of_rec_rules g.rules)
(* Conversion d'une règle en chaîne de caractère *)
let regle2string : regle -> string = function
	| {elementgauche=g;partiedroite=d} -> element2string g ^ " --> " ^ partie2string d

(* Conversion d'une liste de règles en chaîne de caractère *)
let rec reglelist2string = function
    | [] -> "Nothing to display."
    | h::[] -> regle2string h
    | h::t -> regle2string h ^ "\n" ^ reglelist2string t


(** Fonctions utilitaire **)

(* L'opérateur --> permet de créer une règle à la volée (facilité syntaxique) à partie de deux parties *)
let (-->) g d = {elementgauche=g;partiedroite=d}

(* Création d'une grammaire à la volée *)
let (@@) axiome regles = {axiome=axiome;regles=regles}

let (@@@) axiome regles = {axiom=axiome;rules=regles}


(* Récupération d'une monade option contenant le premier non terminal d'une partie (si il existe !) *)
let rec first_non_terminal = function
	| [] -> None
	| Terminal(x)::rest -> first_non_terminal rest
	| Nonterminal(x)::rest -> Some(Nonterminal(x))

let rec first_non_terminal2 : tree_state list -> tree_state option = function
	| [] -> None
	| (pre,Terminal(x),suf)::rest -> first_non_terminal2 rest
	| (pre,Nonterminal(x),suf)::rest -> Some((pre,Nonterminal(x),suf))

let string_inst_of_element (values : (element, string) t) : element -> string  = function
    | s when Hashtbl.mem values s -> Hashtbl.find values s
    | s -> string_of_element s

let string_inst_of_part (values : (element, string) t) : element list -> string = function
    | t::q -> List.fold_left concat_space (string_inst_of_element values t) (List.map (string_inst_of_element values) q)
    | [] -> "ε"

let print_words (w : partie list) : unit = List.iter (fun r -> print_string ("Mot: "^(partie2string r)^"\n")) w

(** Fonctions d'affichage **)

(* Affichage d'une liste de règles *)
let print_rules regles = List.iter (Printf.printf "%s\n") (List.map regle2string regles)

(* Affichage d'une grammaire *)
let print_grammar grammaire = Printf.printf "Axiome : %s \nRegles : \n" (element2string grammaire.axiome);
				  print_rules grammaire.regles

(* TODO *)
let equals_grammars (g1 : rec_grammar) (g2 : rec_grammar) : bool = true

let rec print_grammars = function
    | [] -> print_string "(vide)"
    | t::[] -> print_grammar t
    | t::q -> print_grammar t; print_grammars q


let rec read_part (part : (bool*string) list) : partie = match part with
    | [] -> []
    | t::q when fst t -> Terminal(snd t)::(read_part q)
    | t::q -> Nonterminal(snd t)::(read_part q)

let rec read_rules (rules : ((bool*string) * ((bool*string) list)) list) : regle list = match rules with
    | [] -> []
    | (n,l)::q -> assert (not (fst n)); (Nonterminal(snd n) --> read_part l)::read_rules q

let convert_grammar (tokens : ((bool*string) * (((bool*string) * ((bool*string) list)) list))) : grammaire =
    assert (not (fst (fst tokens)));
    Nonterminal(snd (fst tokens)) @@ read_rules (snd tokens)

let read_bnf_grammar (filename : string) : grammaire =
    let lexbuf = Lexing.from_channel (open_in filename) in
    convert_grammar (Parserbnf.start Lexerbnf.token lexbuf)

let rec read_tokens_from_ch (ch: Lexing.lexbuf) : element list =
    let token = Lexerbnf.token ch in match token with
    | EOF -> []
    | NTERM(b,str) -> Nonterminal(str)::(read_tokens_from_ch ch)
    | TERM(b,str) -> Terminal(str)::(read_tokens_from_ch ch)
    | _ -> failwith "Error token"

let read_tokens (str : string) : element list =
    read_tokens_from_ch (Lexing.from_string str)
