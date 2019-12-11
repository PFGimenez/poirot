%token <bool*string> TERM
%token <bool*string> NTERM
%token EOF
%token SEP
%token END_RULE

%start start
%type <((bool*string) * (((bool*string) * ((bool*string) list)) list))> start
%type <(bool*string) list> ext_right_part
%type <((bool*string) * ((bool*string) list)) list> rlist
%%

start:
    | NTERM END_RULE rlist EOF { ($1,$3) }
;

rule:
    | NTERM SEP ext_right_part { (* Printf.printf "Rule %s\n%!" (snd $1) ; *) ($1,$3) } ;

rlist:
    | rule { [$1] }
    | rule rlist { $1 :: $2 }
;

ext_right_part:
    | NTERM ext_right_part { $1 :: $2 }
    | TERM ext_right_part { $1 :: $2 }
    | END_RULE { [] }
;
