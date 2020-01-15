.PHONY: all clean

all: poirot_inference poirot poirot_fuzzer poirot_quotient

poirot: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml tree_fuzzer.ml inference.ml oracle.ml poirot.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml oracle.ml rec_quotient.ml inference.ml tree_fuzzer.ml poirot.ml -g -o poirot

poirot_inference: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml inference.ml fuzzer.ml tree_fuzzer.ml oracle.ml poirot_inference.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml oracle.ml rec_quotient.ml inference.ml fuzzer.ml tree_fuzzer.ml poirot_inference.ml -g -o poirot_inference

poirot_fuzzer: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml tree_fuzzer.ml poirot_fuzzer.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml tree_fuzzer.ml poirot_fuzzer.ml -g -o poirot_fuzzer

poirot_quotient: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml poirot_quotient.ml
	ocamlopt parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml poirot_quotient.ml -g -o poirot_quotient

parserbnf.ml lexerbnf.ml: lexerbnf.mll parserbnf.mly
	ocamllex lexerbnf.mll
	ocamlyacc parserbnf.mly
	ocamlc -c parserbnf.mli

clean:
	rm -f *.cm* *.o poirot_inference poirot_quotient poirot_fuzzer poirot parserbnf.ml lexerbnf.ml parserbnf.mli
