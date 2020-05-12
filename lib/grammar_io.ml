open Grammar

let quoted_string_of_element : element -> string = function
    | Terminal x -> "'"^(String.escaped x)^"'" (* TODO: corriger *)
    | Nonterminal x -> x

let bnf_string_of_part : part -> string = string_of_list " " "" string_of_element

let bnf_string_of_ext_element (e: ext_element) : string = match e.pf,e.sf with
    | [],[] -> quoted_string_of_element e.e
    | _,_ -> (string_of_element e.e) ^ "_[" ^ (bnf_string_of_part (List.rev e.pf)) ^ "|" ^ (bnf_string_of_part e.sf) ^ "]"

let bnf_string_of_ext_part : ext_part -> string = string_of_list " " "" bnf_string_of_ext_element

let bnf_string_of_ext_rule (r: ext_rule) : string = (bnf_string_of_ext_element r.ext_left_symbol) ^ " ::= " ^ (bnf_string_of_ext_part r.ext_right_part) ^ ";\n"

let bnf_string_of_ext_rules : ext_rule list -> string = string_of_list "" "" bnf_string_of_ext_rule

let bnf_string_of_ext_grammar (g : ext_grammar) : string = (bnf_string_of_ext_element g.ext_axiom) ^ ";\n" ^ (bnf_string_of_ext_rules g.ext_rules)

(* read a grammar from a BNF file *)
let read_bnf_grammar (unravel: bool) (filename : string) : grammar =
    Log.L.debug (fun m -> m "Load grammar %s" filename);
    let lexbuf = Lexing.from_channel (open_in filename) in match unravel with
    | true -> Parserbnf.start_unravel Lexerbnf.token lexbuf
    | _ -> Parserbnf.start Lexerbnf.token lexbuf

(* read a list of tokens from a string *)
let read_tokens (unravel: bool) (str : string) : element list = match unravel with
    | true -> Parserbnf.token_list_unravel Lexerbnf.token (Lexing.from_string (str))
    | _ -> Parserbnf.token_list Lexerbnf.token (Lexing.from_string (str))

(* read a single token from a string *)
let read_token (unravel: bool) (str : string) : element =
    let token = Lexerbnf.token (Lexing.from_string str) in match token with
    | Parserbnf.TERM e -> Terminal e
    | Parserbnf.NTERM e -> Nonterminal e
    | Parserbnf.PSEUDO_TERM e -> (match unravel with
        | true -> Nonterminal e
        | _ -> Terminal e)
    | _ -> failwith "No token!"

(* read a semantics dictionary file *)
let read_dict (fname: string) : (element,string) Hashtbl.t =
    let rec read_dict_tokens lexbuf : (element * string) list =
        match (Lexerconf.token lexbuf : Lexerconf.token) with
        | LINE (e,s) -> (e,s)::(read_dict_tokens lexbuf)
        | EOF -> [] in
    let l = read_dict_tokens (Lexing.from_channel (open_in fname)) in
    let table = Hashtbl.create (List.length l) in
    List.iter (fun (e,s) -> Hashtbl.add table e s) l;
    table

(* export to BNF format FIXME *)
let export_bnf (fname : string) (g: ext_grammar) =
    let channel = open_out fname in
    output_string channel (bnf_string_of_ext_grammar g);
    close_out channel

let export_string (f: char -> string) (s: string) : string =
    (string_of_list "" "" f (List.init (String.length s) (String.get s)))

(* export a grammar in ANTR4 format *)
let export_antlr4 (fname: string) (g: grammar) : unit =
    (* all the non-terminals are written in lowercase to have parser rule and not lexer rule *)

    let export_antlr4_char_nonterminal (c: char) : string = match c with
        | '\'' -> "singlequote" | '\\' -> "antislash" | '"' -> "doublequote"
        | '%' -> "percent" | '&' -> "and" | '|' -> "or" | '=' -> "equal" | '*' -> "star" | '+' -> "plus" | '-' -> "minus" | '/' -> "slash" | '#' -> "number" | '$' -> "dollar"
        | '1' -> "one" | '2' -> "two" | '3' -> "three" | '4' -> "four" | '5' -> "five" | '6' -> "six" | '7' -> "seven" | '8' -> "eight" | '9' -> "nine" | '0' -> "zero"
        | '?' -> "questionmark" | '!' -> "exclammark" | ',' -> "comma" | ';' -> "semicolon" | ':' -> "colon" | '.' -> "point" | '(' -> "leftpar" | ')' -> "rightpar" | '[' -> "leftbra" | ']' -> "rightbra" | '{' -> "leftcurl" | '}' -> "rightcurl" | '>' -> "bigger" | '<' -> "smaller"
        | ' ' -> "ws" | '\t' -> "tab"
        | '@' -> "at" | '~' -> "tilda" | '^' -> "caret"
        | _ -> String.make 1 c in

    let export_antlr4_char_terminal (c: char) : string = match c with
        | '\'' -> "\\'"
        | '\\' -> "\\\\"
        | _ -> String.make 1 c in

   let export_antlr4_elem (e: element) : string = match e with
        | Nonterminal s -> String.lowercase_ascii (export_string export_antlr4_char_nonterminal s)
        | Terminal s -> "'" ^ (export_string export_antlr4_char_terminal s)  ^ "'" in

    let export_antlr4_rhs (elist: element list): string =
        (string_of_list " " "" export_antlr4_elem elist) in

    let export_antlr4_rules (e: element): string =
        (String.lowercase_ascii (export_string export_antlr4_char_nonterminal (string_of_element e)))^" : "^(string_of_list " | " "" export_antlr4_rhs (get_all_rhs g.rules e))^";\n" in
    let channel = open_out (fname^".g4") in
    let last_slash = String.rindex_opt fname '/' in
    let fname_end = match last_slash with
        | None -> fname
        | Some index -> String.sub fname (index + 1) (String.length fname - index - 1) in
    output_string channel ("/* This ANTLR4 grammar has been generated by Poirot */\n\ngrammar "^fname_end^";\n\n");

    output_string channel ("bnf2antlr4_axiom : "^(String.lowercase_ascii ((export_string export_antlr4_char_nonterminal (string_of_element g.axiom))))^" EOF;\n");
    List.iter (fun e -> output_string channel (export_antlr4_rules e)) (List.filter is_non_terminal (get_all_symbols g));
    close_out channel

let export_xdot_name (c: char) : string = match c with
    | '"' -> "\\\""
    | _ -> String.make 1 c

let export_ext_element (e: ext_element) = export_string export_xdot_name (string_of_ext_element e)

(* add an edge in the graphviz output *)
let add_edge_in_graph (graph_channel: out_channel option) (color: string) (from: ext_element) (dest: ext_element): unit =
    Option.iter (fun ch -> output_string ch ("\""^(export_ext_element from)^"\"->\""^(export_ext_element dest)^"\" ["^color^"]\n")) graph_channel

(* set the node attribute in the graphviz output *)
let set_node_attr (graph_channel: out_channel option) (attr: string) (e: ext_element) : unit =
    Option.iter (fun ch -> output_string ch ("\""^(export_ext_element e)^"\""^attr^"\n")) graph_channel

(* color a node in the graphviz output *)
let set_node_color_in_graph (graph_channel: out_channel option) (e: ext_element) (c: string): unit =
    set_node_attr graph_channel ("[color="^c^",style=filled]") e

