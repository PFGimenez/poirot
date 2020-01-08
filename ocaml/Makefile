all: blind_api gfuzz fuzzer_api quotient_api

blind_api: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml blind.ml fuzzer.ml tree_fuzzer.ml oracle.ml blind_api.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml blind.ml fuzzer.ml tree_fuzzer.ml oracle.ml blind_api.ml -g -o blind_api

gfuzz: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml tree_fuzzer.ml blind.ml oracle.ml gfuzz.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml blind.ml tree_fuzzer.ml oracle.ml gfuzz.ml -g -o gfuzz

fuzzer_api: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml tree_fuzzer.ml fuzzer_api.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml tree_fuzzer.ml fuzzer_api.ml -g -o fuzzer_api

quotient_api: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml quotient_api.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml quotient_api.ml -g -o quotient_api

parserbnf.ml lexerbnf.ml: lexerbnf.mll parserbnf.mly
	ocamllex lexerbnf.mll
	ocamlyacc parserbnf.mly
	ocamlc -c parserbnf.mli

clean:
	rm -f *.cm* *.o *_api gfuzz parserbnf.ml lexerbnf.ml parserbnf.mli
