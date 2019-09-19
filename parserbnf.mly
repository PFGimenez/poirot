%token <bool*string> TERM
%token <bool*string> NTERM
%token EOF
%token SEP
%token END_RULE

%start start
%type <((bool*string) * (((bool*string) * ((bool*string) list)) list))> start
%type <(bool*string) list> right_part
%type <((bool*string) * ((bool*string) list)) list> rlist
%%

start:
    | NTERM END_RULE rlist EOF { ($1,$3) }
;

rlist:
    | NTERM SEP right_part { [($1,$3)] }
    | NTERM SEP right_part rlist { ($1,$3) :: $4 }
;

right_part:
    | NTERM right_part { $1 :: $2 }
    | TERM right_part { $1 :: $2 }
    | END_RULE { [] }
;
