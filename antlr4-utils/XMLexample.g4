grammar XMLexample;

axiom: userlist EOF;

userlist: '<userlist>' user* '</userlist>';

user: '<user>' status? name '</user>';

status: '<status>' st '</status>';

st: 'root' | 'visitor';

name: '<name>' TEXT '</name>';

S: [ \t\r\n] -> skip ;

TEXT: CHAR+;

fragment CHAR: [_a-zA-Z];

