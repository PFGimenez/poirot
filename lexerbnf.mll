{
    open Parserbnf
    open Scanf
    exception UnknownToken
}

let space = [' ' '\t']
let eol = "\n"|"\r"|"\r\n"
let legit_symbol = [' ' '(' ')' '.' ',' ':' ';' '>' '<' '*' '+' '-' '/' '^' '`' '[' ']' '{' '}' '!' '=' '|' '&' '~' '%' '#' '$' '@']
let upper_letter = ['A'-'Z']
let lower_letter = ['a'-'z']
let letter = upper_letter|lower_letter
let digit = ['0'-'9']

rule token = parse
    | space* { token lexbuf }
    | space* eol { NEW_LINE }
    | ';' { END_RULE }
    | eol* eof { EOF }
    | "::=" { SEP } (* separator of the rule *)
    | "#" (letter|legit_symbol)* eol { token lexbuf } (* comment *)
    | "'" ((['a'-'z' 'A'-'Z']|legit_symbol|'"'|"\\'")+ as s) "'" { TERM (true,Scanf.unescaped s) } (* terminal *)
    | upper_letter (letter|digit|'_')* as s { NTERM (false,s) } (* parser nonterminal *)
    | lower_letter (letter|digit|'_')* as s { NTERM (false,s) } (* lexer nonterminal *)
    | _ { raise UnknownToken }
