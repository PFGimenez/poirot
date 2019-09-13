all: clean main

main:
	ocamlopt str.cmxa base.ml parser.ml nettoyage.ml clean.ml quotient.ml blind.ml main.ml -g -o fuzzer

clean:
	rm -f *.cm* *.o fuzzer
