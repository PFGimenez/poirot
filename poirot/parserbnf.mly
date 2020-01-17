%token <Grammar.element> TERM
%token <Grammar.element> NTERM
%token EPSILON
%token EOF
%token SEP
%token EOL

%start start
%type <Grammar.grammar> start
%type <Grammar.element list> ext_right_part
%type <Grammar.rule list> rlist
%%

start:
    | NTERM EOL rlist EOF { {axiom=$1; rules=$3} }
;

rlist:
    | EOL rlist { $2 }
    | rule { [$1] }
    | rule rlist { $1 :: $2 }
;

rule:
    | NTERM SEP ext_right_part { {left_symbol=$1; right_part=$3} }
;

ext_right_part:
    | NTERM ext_right_part { $1 :: $2 }
    | TERM ext_right_part { $1 :: $2 }
    | EPSILON ext_right_part { $2 }
    | EOL { [] }
;
