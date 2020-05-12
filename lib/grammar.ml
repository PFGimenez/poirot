(** Type definitions **)

(* Simple grammar *)

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

(* Extented grammar *)

(* An element with a prefix and a suffix *)
type ext_element = {pf: part; e: element; sf: part}

type ext_part = ext_element list

type ext_rule = {ext_left_symbol: ext_element; ext_right_part: ext_part}

let (--->) (g: ext_element) (d: ext_part) : ext_rule = {ext_left_symbol=g;ext_right_part=d}

type ext_grammar = {ext_axiom: ext_element; ext_rules: ext_rule list}

let (@@@) (axiom: ext_element) (rules: ext_rule list) : ext_grammar = {ext_axiom=axiom;ext_rules=rules}


(* Utility functions *)

let is_terminal : element -> bool = function
    | Terminal _ -> true
    | _ -> false

let is_non_terminal (s: element) : bool = not (is_terminal s)

let is_ext_element_terminal (e: ext_element) : bool = is_terminal e.e

let is_ext_element_non_terminal (e: ext_element) : bool = not (is_terminal e.e)

(* returns all terminals *)
let get_all_tokens (grammar : grammar) : element list =
    grammar.rules |> List.rev_map (fun r -> List.filter is_terminal r.right_part) |> List.concat |> List.sort_uniq compare

(* returns all symbols: terminals and nonterminals *)
let get_all_symbols (g: grammar) : element list =
    g.rules |> List.rev_map (fun r -> r.left_symbol::r.right_part) |> List.flatten |> List.sort_uniq compare

(* get the list of rules with some left-hand side *)
let get_rules (rlist: ext_rule list) (e: ext_element) : ext_rule list =
    List.filter (fun r -> r.ext_left_symbol = e) rlist

let get_all_rhs (rlist: rule list) (e: element) : part list =
    List.map (fun r -> r.right_part) (List.filter (fun r -> r.left_symbol = e) rlist)

(* an hashtbl used to explore a grammar *)
let seen : (element,unit) Hashtbl.t = Hashtbl.create 500

(* look for a goal from a list of starting tokens. Returns the starting token that found the goal (or None) *)
let find_start (g: grammar) (goal: element) (start: element list) : element option =
    let rec find_start_aux (g: grammar) (goal: element) (queue: (element*element) list) : element option = match queue with
        | [] -> None
        | (t,s)::_ when t=goal -> Some s
        | (t,_)::q when Hashtbl.mem seen t -> (find_start_aux [@tailcall]) g goal q
        | (t,s)::q -> let new_elems = List.flatten (List.filter_map (fun r -> if r.left_symbol = t then Some (List.map (fun e -> e,s) r.right_part) else None) g.rules) in
            Hashtbl.add seen t ();
            (find_start_aux [@tailcall]) g goal (q@new_elems) in
    Hashtbl.clear seen;
    find_start_aux g goal (List.map (fun s -> (s,s)) start)

(* is the element s reachable in the grammar g from the element start ? *)
let is_reachable (g: grammar) (s: element) (start: element) : bool =
    let rec is_reachable_aux (queue: element list) : bool = match queue with
        | [] -> false
        | t::_ when t=s -> true
        | t::q when Hashtbl.mem seen t -> (is_reachable_aux [@tailcall]) q
        | t::q -> let new_elems = List.flatten (List.filter_map (fun r -> if r.left_symbol = t then Some r.right_part else None) g.rules) in
                Hashtbl.add seen t ();
                (is_reachable_aux [@tailcall]) (q@new_elems) in
    Hashtbl.clear seen;
    is_reachable_aux [start]

(* true iff g1 is a subgrammar of g2, i.e. the rules of g1 are included in the rules of g2 and their axiom and identical *)
let is_subgrammar (g1: grammar) (g2: grammar) : bool =
    g1.axiom = g2.axiom &&
    List.for_all (fun r -> List.exists ((=) r) g2.rules) g1.rules

let add_comment (g: grammar) (s: string) : grammar =
    let new_axiom = Nonterminal "poirot_axiom_for_comment"
    and new_nterm = Nonterminal "poirot_nonterminal_comment" in
    let new_rules = List.map (fun e -> new_nterm --> [e;new_nterm]) (get_all_symbols g) in
    let new_rules = (new_axiom --> [g.axiom])::(new_axiom --> [g.axiom; Terminal s; new_nterm])::(new_nterm --> [])::(new_rules@g.rules) in
    new_axiom@@new_rules

(* get the list of token that can directly produce "axiom" *)
let symbols_from_parents (g: grammar) (axiom : element) : element list =
    g.rules |> List.filter (fun r -> List.mem axiom r.right_part) |> List.rev_map (fun r -> r.left_symbol) |> List.sort_uniq compare

(* string of ... *)

let decorated_string_of_element : element -> string = function
    | Terminal x -> "'"^x^"'"
    | Nonterminal x -> "<"^x^">"

let string_of_element : element -> string = function
    | Terminal x | Nonterminal x -> x

let string_of_list (delim: string) (empty: string) (string_of_a: 'a -> string) (l: 'a list) : string =
    let concat_with_delimiter (d: string) (s1: string) (s2: 'a) : string = s1 ^ d ^ (string_of_a s2) in
    let concat : string -> 'a -> string = concat_with_delimiter delim in match l with
    | t::q -> List.fold_left concat (string_of_a t) q
    | [] -> empty

let string_of_word : part -> string = string_of_list "" "" string_of_element

let string_of_part : part -> string = string_of_list " " "ε" decorated_string_of_element

let string_of_ext_element (e: ext_element) : string = let str=string_of_element e.e in match e.pf,e.sf with
    | [],[] -> str
    | _,_ -> str ^ "_[" ^ (string_of_word (List.rev e.pf)) ^ "|" ^ (string_of_word e.sf) ^ "]"

let string_of_ext_part : ext_part -> string = string_of_list " " "ε" string_of_ext_element

let string_of_ext_rule (r: ext_rule) : string = (string_of_ext_element r.ext_left_symbol) ^ " -> " ^ (string_of_ext_part r.ext_right_part)

let string_of_ext_rules : ext_rule list -> string = string_of_list "\n" "(no rules)" string_of_ext_rule

let string_of_ext_grammar (g : ext_grammar) : string = "axiom: " ^ (string_of_ext_element g.ext_axiom) ^ "\nRules: " ^ (string_of_ext_rules g.ext_rules)

let string_of_rule ({left_symbol;right_part}: rule) : string = decorated_string_of_element left_symbol ^ " --> " ^ string_of_part right_part

(*let string_of_rules : rule list -> string = string_of_list "\n" "(no rules)" string_of_rule

let string_of_grammar (g : grammar) : string = "axiom: " ^ (string_of_element g.axiom) ^ "\nRules: " ^ (string_of_rules g.rules)*)

let string_of_grammar (g: grammar) : string =
    let string_of_rules_of_elem (e: element): string =
        (decorated_string_of_element e)^" --> "^(string_of_list " | " "" string_of_part (get_all_rhs g.rules e))^";\n" in
    "axiom: " ^ (string_of_element g.axiom) ^ "\n" ^ (string_of_list "" "(no rules)" (fun e -> (string_of_rules_of_elem e)) (List.filter is_non_terminal (get_all_symbols g)))

(* Conversion *)

let rhs_of_ext_rule (r: ext_rule): ext_part = r.ext_right_part

let lhs_of_ext_rule (r: ext_rule): ext_element = r.ext_left_symbol

let element_of_ext_element ({e;_} : ext_element) : element = e

let ext_element_of_element (e: element) : ext_element = {pf=[]; e=e; sf=[]}

let ext_rule_of_rule (r: rule) : ext_rule = (ext_element_of_element r.left_symbol) ---> (List.map ext_element_of_element r.right_part)

let ext_grammar_of_grammar (g: grammar) : ext_grammar = {ext_axiom = ext_element_of_element g.axiom; ext_rules = List.rev_map ext_rule_of_rule g.rules}

let full_element_of_ext_element (e : ext_element) : element =
    let underscore_string_of_part = string_of_list "_" "epsilon" string_of_element in match e with
    | {pf=_;e=Terminal _;sf=_} -> e.e
    | {pf=[];e=Nonterminal x;sf=[]} -> Nonterminal(x)
    | {pf=_;e=Nonterminal x;sf=_} -> Nonterminal((underscore_string_of_part (List.rev e.pf))^"__"^x^"__"^(underscore_string_of_part e.sf))

let rule_of_ext_rule (r: ext_rule) : rule = full_element_of_ext_element r.ext_left_symbol --> (List.map full_element_of_ext_element r.ext_right_part)

let grammar_of_ext_grammar (g: ext_grammar) : grammar = (full_element_of_ext_element g.ext_axiom) @@ (List.rev_map rule_of_ext_rule g.ext_rules)
