all: inference_api gfuzz fuzzer_api quotient_api

inference_api: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml inference.ml fuzzer.ml tree_fuzzer.ml oracle.ml inference_api.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml oracle.ml rec_quotient.ml inference.ml fuzzer.ml tree_fuzzer.ml inference_api.ml -g -o inference_api

gfuzz: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml tree_fuzzer.ml inference.ml oracle.ml gfuzz.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml oracle.ml rec_quotient.ml inference.ml tree_fuzzer.ml gfuzz.ml -g -o gfuzz

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
