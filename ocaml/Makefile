all: blind_api gfuzz fuzzer_api

blind_api: parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml clean.ml rec_quotient.ml fuzzer.ml tree_fuzzer.ml blind.ml blind_api.ml
	ocamlopt parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml clean.ml rec_quotient.ml blind.ml fuzzer.ml tree_fuzzer.ml blind_api.ml -g -o blind_api

gfuzz: parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml clean.ml rec_quotient.ml tree_fuzzer.ml blind.ml gfuzz.ml
	ocamlopt parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml clean.ml rec_quotient.ml blind.ml tree_fuzzer.ml gfuzz.ml -g -o gfuzz

fuzzer_api: parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml tree_fuzzer.ml fuzzer_api.ml
	ocamlopt parserbnf.cmx lexerbnf.cmx grammar.ml grammar_io.ml tree_fuzzer.ml fuzzer_api.ml -g -o fuzzer_api

clean:
	rm -f *.cm* *.o *_api gfuzz parserbnf.ml lexerbnf.ml parserbnf.mli

parserbnf.cmx lexenbnf.cmx: lexerbnf.mll parserbnf.mly
	ocamllex lexerbnf.mll
	ocamlyacc parserbnf.mly
	ocamlc -c parserbnf.mli
	ocamlopt -c parserbnf.ml
	ocamlopt -c lexerbnf.ml
