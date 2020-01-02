(** Type definitions **)

(* An element represents a rule element that can be either a Terminal or a Nonterminal *)
type element = Terminal of string | Nonterminal of string

(* A part represents a list of elements. It can either be a right-hand side of a rule or an intermediate derivation *)
type part = element list

(* A rule is composed of a left element and a right part *)
type rule = {left_symbol: element; right_part: part }

(* Easy rule building *)
let (-->) (g: element) (d: part) : rule = {left_symbol=g;right_part=d}

(* A grammar is composed of an ext_axiom (an element) and a list of ext_rules *)
type grammar = {axiom: element; rules: rule list}

let (@@) (axiom: element) (rules: rule list) : grammar = {axiom=axiom;rules=rules}

(* An element with a prefix and a suffix *)
type ext_element = {pf: part; e: element; sf: part}

type ext_part = ext_element list

type ext_rule = {ext_left_symbol: ext_element; ext_right_part: ext_part}

let (--->) (g: ext_element) (d: ext_part) : ext_rule = {ext_left_symbol=g;ext_right_part=d}

type ext_grammar = {ext_axiom: ext_element; ext_rules: ext_rule list}

let (@@@) (axiom: ext_element) (rules: ext_rule list) : ext_grammar = {ext_axiom=axiom;ext_rules=rules}

(* Conversion *)

let rhs_of_ext_rule (r: ext_rule): ext_part = r.ext_right_part

let lhs_of_ext_rule (r: ext_rule): ext_element = r.ext_left_symbol

let ext_element_of_element (e: element) : ext_element = {pf=[]; e=e; sf=[]}

let element_of_ext_element (e : ext_element) : element = e.e

let ext_rule_of_rule (r: rule) : ext_rule = (ext_element_of_element r.left_symbol) ---> (List.map ext_element_of_element r.right_part)

let ext_grammar_of_grammar (g: grammar) : ext_grammar = {ext_axiom = ext_element_of_element g.axiom; ext_rules = List.rev_map ext_rule_of_rule g.rules}

(* string of ... *)

let string_of_element : element -> string = function
    | Terminal(x) -> x
    | Nonterminal(x) -> x

let string_of_list (delim: string) (empty: string) (string_of_a: 'a -> string) (l: 'a list) : string =
    let concat_with_delimiter (d: string) (s1: string) (s2: 'a) : string = s1 ^ d ^ (string_of_a s2) in
    let concat : string -> 'a -> string = concat_with_delimiter delim in match l with
    | t::q -> List.fold_left concat (string_of_a t) q
    | [] -> empty

let string_of_part : part -> string = string_of_list " " "ε" string_of_element

let string_of_ext_element (e: ext_element) : string = let str=string_of_element e.e in match e.pf,e.sf with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (string_of_part (List.rev e.pf)) ^ "|" ^ (string_of_part e.sf) ^ "]"

let string_of_ext_part : ext_part -> string = string_of_list " " "ε" string_of_ext_element

let string_of_ext_rule (r: ext_rule) : string = (string_of_ext_element r.ext_left_symbol) ^ " -> " ^ (string_of_ext_part r.ext_right_part)

let string_of_ext_rules : ext_rule list -> string = string_of_list "\n" "(no rules)" string_of_ext_rule

let string_of_ext_grammar (g : ext_grammar) : string = "axiom: " ^ (string_of_ext_element g.ext_axiom) ^ "\nRules: " ^ (string_of_ext_rules g.ext_rules)

let string_of_rule ({left_symbol=g;right_part=d}: rule) : string = string_of_element g ^ " --> " ^ string_of_part d

let string_of_rules : rule list -> string = string_of_list "\n" "(no rules)" string_of_rule

let string_of_grammar (g : grammar) : string = "axiom: " ^ (string_of_element g.axiom) ^ "\nRules: " ^ (string_of_rules g.rules)

(* Utility functions *)

let is_terminal : element -> bool = function
    | Terminal(_) -> true
    | _ -> false

let is_non_terminal (s: element) : bool = not (is_terminal s)

let is_ext_element_terminal (e: ext_element) : bool = is_terminal e.e

let is_ext_element_non_terminal (e: ext_element) : bool = not (is_terminal e.e)

let get_all_tokens (grammar : grammar) : element list =
    grammar.rules |> List.rev_map (fun r -> List.filter is_terminal r.right_part) |> List.concat |> List.sort_uniq compare

let get_all_symbols (g: grammar) : element list =
    g.rules |> List.rev_map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare

(* get the list of rules with some left-hand side *)
let get_rules (rlist: ext_rule list) (e: ext_element) : ext_rule list =
    List.filter (fun r -> r.ext_left_symbol = e) rlist

let is_reachable (g: grammar) (s: element) (start: element) : bool =
let rec is_reachable_aux (g: grammar) (s: element) (reachable : element list) : bool =
    if List.mem s reachable then true
    else
        let ext_rules = List.filter (fun r -> List.mem r.left_symbol reachable) g.rules in
        let new_reachable = ext_rules |> List.rev_map (fun r -> r.right_part) |> List.flatten |> List.append reachable |> List.sort_uniq compare in
        if (List.compare_lengths reachable new_reachable) = 0 then false
        else (is_reachable_aux [@tailcall]) g s new_reachable in
    is_reachable_aux g s [start]

