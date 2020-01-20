{
    open Parserbnf
    exception Syntax_error of string
    let loc = ref 1
    let syntax_error msg = raise (Syntax_error (msg^" on line "^(string_of_int !loc)))
(*
    let remove_backslash (l: char list) (len: int) (c: char) (i: int) (current: char) : char option =
        if i = len - 1 then Some current
        else if current = '\\' && (List.nth l (i+1)) = c then None
        else Some current

    (* remove the \ before the symbol c *)
    let unescape_symbol (c: char) (s: string) : string =
        let len = String.length s in
        let l = List.init len (String.get s) in
        let l2 = List.mapi (remove_backslash l len c) l in
        let l3 = List.filter_map (fun x -> x) l2 in
        String.init (List.length l3) (List.nth l3)
    *)

    let make_term (s: string) : token =
        try
            if String.length s = 1 then TERM (Grammar.Terminal (Scanf.unescaped s))
            else PSEUDO_TERM (Grammar.Nonterminal (Scanf.unescaped s))
        with Scanf.Scan_failure str -> syntax_error ("Error while unescaping "^s^": "^str)
}


let eol = "\n"|"\r"|"\r\n"

rule token = parse
    | "#" [^ '\n' '\r']* eol { incr loc; EOL } (* comment *)

    | [' ' '\t']* { token lexbuf } (* whitespace *)
    | eol { incr loc; EOL } (* new line *)

    | eof { EOF } (* end of file *)

    | "::=" { SEP } (* separator of the rule *)
    | "ε" { EPSILON } (* empty sentence *)

    | "EOF" { TERM (Grammar.Terminal "") }
    | '\'' (([^ '\'' '\n' '\r']|"\\\'")* as s) '\'' { make_term s } (* terminal *)
    | '"' (([^ '"' '\n' '\r']|"\\\"")* as s) '"' { make_term s } (* terminal *)
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* ("_[" [^ ']' '\n' '\r']* "]")? as s { (*Printf.printf "NT:%s\n%!" s;*) NTERM (Grammar.Nonterminal s) } (* nonterminal *)

    | _ as c { syntax_error ("couldn't identify the token '"^(String.make 1 c)^"'")}
