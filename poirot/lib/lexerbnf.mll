{
    open Parserbnf
    let loc = ref 1
    let syntax_error msg = failwith (msg^" on line "^(string_of_int !loc))

    let make_term (s: string) : token =
        try
            if String.length s = 1 then TERM (Grammar.Terminal (Scanf.unescaped s))
            else PSEUDO_TERM (Grammar.Nonterminal (Scanf.unescaped s))
        with Scanf.Scan_failure str -> syntax_error ("Error while unescaping "^s^": "^str)
}

let eol = "\n"|"\r"|"\r\n"

rule token = parse
    | "#" [^ '\n' '\r']* eol { incr loc; token lexbuf } (* comment *)
    | eol { incr loc; token lexbuf } (* new line *)

    | [' ' '\t']* { token lexbuf } (* whitespace *)

    | eof { EOF } (* end of file *)

    | ";" { END_RULE } (* end of a rule *)
    | "::=" { SEP } (* separator of the rule *)
    | "ε" { EPSILON } (* empty sentence *)

    | "EOF" { TERM (Grammar.Terminal "") }
    | "<EOF>" { TERM (Grammar.Terminal "") }
    | '\'' (([^ '\'' '\n' '\r']|"\\\'")* as s) '\'' { make_term s } (* terminal *)
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* ("_[" [^ ']' '\n' '\r']* "]")? as s { NTERM (Grammar.Nonterminal s) } (* nonterminal *)
    | '<' ([^ '>' '\n' '\r']* as s) '>' { NTERM (Grammar.Nonterminal s) } (* nonterminal *)

    | _ as c { syntax_error ("couldn't identify the token '"^(String.make 1 c)^"'")}
