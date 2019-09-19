all: clean main

main: parser
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml parser.ml nettoyage.ml clean.ml quotient.ml blind.ml main.ml -g -o fuzzer

clean:
	rm -f *.cm* *.o fuzzer

parser:
	ocamllex lexerbnf.mll
	ocamlyacc parserbnf.mly
	ocamlc -c parserbnf.mli
	ocamlopt -c parserbnf.ml
	ocamlopt -c lexerbnf.ml
