grammar mime;

axiom: s EOF;

s: fields (CRLF body)?;

body: phrase (CRLF phrase)* CRLF?;


// Primitive tokens

atom: TEXT;

dot_atom: atom ('.' atom)*;

phrase: atom (FWS atom)*;


// Date specs

date_time: day_name ',' FWS day_number FWS month_name FWS year FWS time;

day_name: 'Mon' | 'Tue' | 'Wed' | 'Thu' | 'Fri' | 'Sat' | 'Sun';

month_name: 'Jan' | 'Feb' | 'Mar' | 'Apr' | 'May' | 'Jun' | 'Jul' | 'Aug' | 'Sep' | 'Oct' | 'Nov' | 'Dec';

day_number: C_NUMBER;

year: Q_NUMBER;

time: hour ':' minute (':' second)? FWS ('+'|'-') zone;

zone: Q_NUMBER FWS '(' UPPER_CHARS ')';

hour: D_NUMBER;

minute: D_NUMBER;

second: D_NUMBER;



// Address specs

address: name_addr | addr_spec;

name_addr: display_name? angle_addr;

angle_addr: FWS? '<'  addr_spec '>';

display_name: phrase;

addr_spec: local_part '@' domain;

local_part: dot_atom;

domain: dot_atom;

address_list: address (',' FWS? address)*;



// Fields specs

fields: (return_path | received | orig_date | r_from | google_from | to | cc | bcc | message_id | subject)*;

return_path: 'Return-Path:' angle_addr CRLF;

received: 'Received:' FWS? RECEIVED_CONTENT ';' CRLF? date_time CRLF;

orig_date: 'Date:' FWS? date_time CRLF;

r_from: 'From:' FWS? address_list CRLF;

google_from: 'X-Google-Original-From:' FWS? address_list CRLF;

to: 'To:' FWS? address_list CRLF;

cc: 'Cc:' FWS? address_list CRLF;

bcc: 'Bcc:' FWS? address_list CRLF;

message_id: 'Message-ID:' FWS? msg_id CRLF;

msg_id: '<' dot_atom '@' dot_atom '>';

subject: 'Subject:' FWS? phrase CRLF;



// Lexer rules

fragment DIGIT: [0-9];

D_NUMBER: DIGIT DIGIT;

Q_NUMBER: DIGIT DIGIT DIGIT DIGIT;

C_NUMBER: DIGIT | DIGIT DIGIT;

UPPER_CHARS: [A-Z]+;

fragment CR: '%0A';

fragment LF: '%0D';

fragment SP: '%20';

RECEIVED_CONTENT: [^;]+;

CRLF: CR;

TEXT: [a-zA-Z0-9\-_]+;

FWS: SP;
