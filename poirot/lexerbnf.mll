{
    open Parserbnf
    open Scanf
    open List
    exception UnknownToken of string
    let loc = ref 1;;
}

let eol = "\n"|"\r"|"\r\n"

rule token = parse
    | "#" [^ '\n' '\r']* eol { loc := !loc + 1; EOL } (* comment *)

    | [' ' '\t']* { token lexbuf } (* whitespace *)
    | eol { loc := !loc + 1; print_endline ("line "^(string_of_int !loc)); EOL } (* new line *)

    | eof { EOF } (* end of file *)

    | "::=" { SEP } (* separator of the rule *)
    | "ε" { EPSILON } (* empty sentence *)

    | "EOF" { TERM (Grammar.Terminal "EOF") }
    | "'" (([^ '\'' '\n' '\r']|"\\\'")* as s) "'" { TERM (Grammar.Terminal (unescaped s)) } (* terminal *)
    | '"' (([^ '"' '\n' '\r']|"\\\"")* as s) '"' { TERM (Grammar.Terminal (unescaped s)) } (* terminal *)
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* ("_[" [^ ']' '\n' '\r']* "]")? as s { (*Printf.printf "NT:%s\n%!" s;*) NTERM (Grammar.Nonterminal s) } (* nonterminal *)

    | _ as c { raise (UnknownToken (Char.escaped c))}
