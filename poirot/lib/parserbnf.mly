%token <string> TERM
%token <string> PSEUDO_TERM
%token <string> NTERM
%token EPSILON
%token EOF
%token SEP
%token END_RULE

%start <Grammar.element list> token_list_unravel
%start <Grammar.element list> token_list

%start <Grammar.grammar> start_unravel
%start <Grammar.grammar> start

%type <(bool*Grammar.element) list> right_part_unravel
%type <Grammar.element list> right_part

%type <Grammar.rule list> rlist_unravel
%type <Grammar.rule list> rlist

%type <Grammar.rule list> rule_unravel
%type <Grammar.rule> rule
%%

start_unravel:
    | NTERM END_RULE rlist_unravel { {axiom=Nonterminal $1; rules=List.sort_uniq compare $3} }
;

start:
    | NTERM END_RULE rlist { {axiom=Nonterminal $1; rules=List.sort_uniq compare $3} }
;

rlist_unravel:
    | rule_unravel rlist_unravel { $1 @ $2 }
    | EOF { [] }
;

rlist:
    | rule rlist { $1 :: $2 }
    | EOF { [] }
;

rule_unravel:
    | NTERM SEP right_part_unravel {
        let build_term (s: string) : Grammar.element list = List.init (String.length s) (fun i -> Grammar.Terminal (String.sub s i 1)) in
        let make_new_rules (b,e: bool*Grammar.element) : Grammar.rule option = match b,e with
            | true,Nonterminal s -> Some {left_symbol=e; right_part=build_term s}
            | _ -> None in
        let rlist = List.filter_map make_new_rules $3 in
        let elist= snd (List.split $3) in
        {left_symbol=Nonterminal $1; right_part=elist}::rlist }
;

rule:
    | NTERM SEP right_part { {left_symbol=Nonterminal $1; right_part=$3} }
;

right_part_unravel:
    | PSEUDO_TERM right_part_unravel { (true, Nonterminal $1) :: $2 }
    | NTERM right_part_unravel { (false, Nonterminal $1) :: $2 }
    | TERM right_part_unravel { (false, Terminal $1) :: $2 }
    | EPSILON right_part_unravel { $2 }
    | END_RULE { [] }
;

right_part:
    | PSEUDO_TERM right_part { Terminal $1 :: $2 }
    | NTERM right_part { Nonterminal $1 :: $2 }
    | TERM right_part { Terminal $1 :: $2 }
    | EPSILON right_part { $2 }
    | END_RULE { [] }
;

token_list_unravel:
    | PSEUDO_TERM token_list { Nonterminal $1 :: $2 }
    | NTERM token_list { Nonterminal $1 :: $2 }
    | TERM token_list { Terminal $1 :: $2 }
    | EPSILON token_list { $2 }
    | EOF { [] }
;

token_list:
    | PSEUDO_TERM token_list { Terminal $1 :: $2 }
    | NTERM token_list { Nonterminal $1 :: $2 }
    | TERM token_list { Terminal $1 :: $2 }
    | EPSILON token_list { $2 }
    | EOF { [] }
;
