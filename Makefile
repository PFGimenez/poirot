all: clean main

main:base.cmo parser.cmo nettoyage.cmo quotient.cmo reconstruct.cmo
	ocamlc base.cmo str.cma parser.cmo nettoyage.cmo quotient.cmo reconstruct.cmo main.ml -g -o fuzzer

parser.cmo:base.cmo
	ocamlc base.cmo str.cma parser.ml -c

nettoyage.cmo:base.cmo
	ocamlc base.cmo str.cma nettoyage.ml -c

quotient.cmo:base.cmo
	ocamlc base.cmo quotient.ml -c

reconstruct.cmo:base.cmo
	ocamlc base.cmo reconstruct.ml -c

base.cmo:
	ocamlc base.ml -c

clean:
	rm -f *.cm* fuzzer
