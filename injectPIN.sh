rm /tmp/injectionpin /tmp/requete 2> /dev/null
echo "===ELABORATION DE LA REQUETE==="
echo "[X]" > /tmp/requete
echo "===GENERATION DES INJECTIONS==="
./fuzzer grammaires/pin.gr S requetepin.req | grep "Mot :" | sed "s/Mot\ :\ //g" > /tmp/injectionpin
echo "====TENTATIVES D' INTRUSION===="
python injectBash.py -d --param=0.1 --input=/tmp/injectionpin --arg=pin --url=http://localhost/injections/pin.php


