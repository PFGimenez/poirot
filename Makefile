all: clean main bytecode

main:
	ocamlopt str.cmxa base.ml parser.ml nettoyage.ml clean.ml quotient.ml blind.ml main.ml -g -o fuzzer

bytecode:
	ocamlc str.cma base.ml parser.ml nettoyage.ml clean.ml quotient.ml blind.ml main.ml -g -o fuzzer-bcode

clean:
	rm -f *.cm* *.o fuzzer fuzzer-bcode
