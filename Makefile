all: clean main

main:base.cmo parser.cmo nettoyage.cmo quotient.cmo 
	ocamlc base.cmo str.cma parser.cmo nettoyage.cmo quotient.cmo main.ml -o fuzzer

parser.cmo:base.cmo
	ocamlc base.cmo str.cma parser.ml -c

nettoyage.cmo:base.cmo
	ocamlc base.cmo str.cma nettoyage.ml -c

quotient.cmo:base.cmo
	ocamlc base.cmo quotient.ml -c


base.cmo:
	ocamlc base.ml -c

clean:
	rm -f *.cm* fuzzer
