.PHONY: all clean

CC=ocamlc

all: test_inference test_fuzzer test_quotient clean

test_inference: parserbnf.ml lexerbnf.ml grammar.ml grammar_io.ml clean.ml rec_quotient.ml inference.ml fuzzer.ml tree_fuzzer.ml oracle.ml test_inference.ml
	$(CC) parserbnf.ml lexerbnf.ml grammar.ml clean.ml grammar_io.ml oracle.ml rec_quotient.ml inference.ml fuzzer.ml tree_fuzzer.ml test_inference.ml -g -o test_inference

test_fuzzer: parserbnf.ml lexerbnf.ml grammar.ml clean.ml grammar_io.ml tree_fuzzer.ml test_fuzzer.ml
	$(CC) parserbnf.ml lexerbnf.ml grammar.ml clean.ml grammar_io.ml tree_fuzzer.ml test_fuzzer.ml -g -o test_fuzzer

test_quotient: parserbnf.ml lexerbnf.ml grammar.ml clean.ml grammar_io.ml rec_quotient.ml test_quotient.ml
	$(CC) parserbnf.ml lexerbnf.ml grammar.ml clean.ml grammar_io.ml rec_quotient.ml test_quotient.ml -g -o test_quotient

parserbnf.ml lexerbnf.ml: lexerbnf.mll parserbnf.mly grammar.ml
	$(CC) grammar.ml
	ocamllex lexerbnf.mll
	menhir parserbnf.mly
	$(CC) -c parserbnf.mli

clean:
	rm -f *.cm* *.o parserbnf.ml lexerbnf.ml parserbnf.mli
