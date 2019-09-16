# Mot("ping");Entree
rm /tmp/injections
./fuzzer grammaires/bash.gr S requeteBash.req | grep "Mot" | sed 's/Mot\ :\ //' | sed 's/<arg>/\ machin\ /g' | sed 's/&&/\ \&\&\ /g'  | sed 's/;/\ ;\ /g' > /tmp/injections
python injectBash.py
