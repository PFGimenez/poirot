all: main

main: blind_api

blind_api: parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml clean.ml rec_quotient.ml fuzzer.ml tree_fuzzer.ml blind.ml blind_api.ml
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml clean.ml rec_quotient.ml blind.ml fuzzer.ml tree_fuzzer.ml blind_api.ml -g -o blind_api

clean:
	rm -f *.cm* *.o *_api parserbnf.ml lexerbnf.ml parserbnf.mli

parserbnf.cmx lexenbnf.cmx: lexerbnf.mll parserbnf.mly
	ocamllex lexerbnf.mll
	ocamlyacc parserbnf.mly
	ocamlc -c parserbnf.mli
	ocamlopt -c parserbnf.ml
	ocamlopt -c lexerbnf.ml
