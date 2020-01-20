%token <Grammar.element> TERM
%token <Grammar.element> PSEUDO_TERM
%token <Grammar.element> NTERM
%token EPSILON
%token EOF
%token SEP
%token EOL

%start start
%type <Grammar.grammar> start
%type <(bool*Grammar.element) list> right_part
%type <Grammar.rule list> rlist
%type <Grammar.rule list> rule
%%

start:
    | NTERM EOL rlist { {axiom=$1; rules=List.sort_uniq compare $3} }
;

rlist:
    | rule rlist { $1 @ $2 }
    | EOL rlist { $2 }
    | EOF { [] }
;

rule:
    | NTERM SEP right_part {
        let build_term (s: string) : Grammar.element list = List.init (String.length s) (fun i -> Grammar.Terminal (String.make 1 (String.get s i))) in
        let make_new_rules (b,e: bool*Grammar.element) : Grammar.rule option = match b,e with
            | true,Nonterminal s -> Some {left_symbol=e; right_part=build_term s}
            | _ -> None in
        let rlist = List.filter_map make_new_rules $3 in
        let elist= snd (List.split $3) in
        rlist@[{left_symbol=$1; right_part=elist}] }
;

right_part:
    | PSEUDO_TERM right_part { (true, $1) :: $2 }
    | NTERM right_part { (false, $1) :: $2 }
    | TERM right_part { (false, $1) :: $2 }
    | EPSILON right_part { $2 }
    | EOL { [] }
;
