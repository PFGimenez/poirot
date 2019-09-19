all: clean main

main: parser
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml parser.ml nettoyage.ml clean.ml quotient.ml blind.ml blind_api.ml -g -o blind_api
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml parser.ml nettoyage.ml clean.ml quotient.ml blind.ml quotient_api.ml -g -o quotient_api
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml parser.ml nettoyage.ml clean.ml quotient.ml blind.ml fuzzer_api.ml -g -o fuzzer_api

clean:
	rm -f *.cm* *.o *_api

parser:
	ocamllex lexerbnf.mll
	ocamlyacc parserbnf.mly
	ocamlc -c parserbnf.mli
	ocamlopt -c parserbnf.ml
	ocamlopt -c lexerbnf.ml
