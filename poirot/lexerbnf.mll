{
    open Parserbnf
    open Scanf
    open List
    exception UnknownToken of string
    let loc = ref 1;;
}

let eol = "\n"|"\r"|"\r\n"

rule token = parse
    | "#" [^ '\n' '\r']* eol { loc := !loc + 1; token lexbuf } (* comment *)

    | [' ' '\t']* { token lexbuf } (* whitespace *)
    | eol { loc := !loc + 1; print_endline ("line "^(string_of_int !loc)); token lexbuf } (* new line *)

    | ';' { END_RULE } (* end of a rule or of the axiom *)

    | eof { EOF } (* end of file *)

    | "::=" { SEP } (* separator of the rule *)
    | "ε" { EPSILON } (* empty sentence *)

    | "EOF" { TERM (true,"EOF") }
    | "'" (([^ '\'' '\n' '\r']|"\\\'")* as s) "'" { TERM (true,unescaped s) } (* terminal *)
    | '"' (([^ '"' '\n' '\r']|"\\\"")* as s) '"' { TERM (true,unescaped s) } (* terminal *)
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* ("_[" [^ ']' '\n' '\r']* "]")? as s { (*Printf.printf "NT:%s\n%!" s;*) NTERM (false,s) } (* nonterminal *)

    | _ as c { raise (UnknownToken (Char.escaped c))}
