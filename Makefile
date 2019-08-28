all: clean main

main:
	ocamlopt str.cmxa base.ml parser.ml nettoyage.ml quotient.ml blind.ml main.ml -o fuzzer

clean:
	rm -f *.cm* *.o fuzzer
