%token <bool*string> TERM
%token <bool*string> NTERM
%token NEW_LINE
%token EOF
%token SEP
%token END_RULE

%start start
%type <((bool*string) * (((bool*string) * ((bool*string) list)) list))> start
%%

start:
    | NTERM NEW_LINE rlist EOF { ($1,$3) }
;

rlist:
    | NTERM SEP right_part { [($1,$3)] }
    | NTERM SEP right_part NEW_LINE rlist { ($1,$3) :: $5 }
    | NEW_LINE rlist { $2 }
;

right_part:
    | NTERM right_part { $1 :: $2 }
    | TERM right_part { $1 :: $2 }
    | END_RULE { [] }
;
