{
    type token = LINE of (Grammar.element * string) | EOF
    let loc = ref 1
    let syntax_error msg = failwith (msg^" on line "^(string_of_int !loc))
}

let eol = "\n"|"\r"|"\r\n"

rule token = parse
    | "#" [^ '\n' '\r']* eol { incr loc; token lexbuf } (* comment *)
    | eol { incr loc; token lexbuf } (* new line *)
    | '<' ([^ '>' '\n' '\r']* as lhs) ">=" ([^ '\n' '\r']* as rhs) { LINE (Grammar.Nonterminal lhs,rhs) } (* nonterminal *)
    | eof { EOF } (* end of file *)
    | _ as c { syntax_error ("couldn't identify the token '"^(String.make 1 c)^"'")}
