all: main

main: rec_blind_api blind_api quotient_api fuzzer_api

rec_blind_api: parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml rec_quotient.ml rec_blind_api.ml
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml rec_quotient.ml rec_blind_api.ml -g -o rec_blind_api

blind_api: parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml fuzzer.ml nettoyage.ml clean.ml quotient.ml blind.ml blind_api.ml
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml fuzzer.ml nettoyage.ml clean.ml quotient.ml blind.ml blind_api.ml -g -o blind_api

quotient_api: parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml fuzzer.ml nettoyage.ml clean.ml quotient.ml blind.ml quotient_api.ml
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml fuzzer.ml nettoyage.ml clean.ml quotient.ml blind.ml quotient_api.ml -g -o quotient_api

fuzzer_api: parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml fuzzer.ml nettoyage.ml clean.ml quotient.ml blind.ml fuzzer_api.ml
	ocamlopt str.cmxa parserbnf.cmx lexerbnf.cmx base.ml grammar_io.ml fuzzer.ml nettoyage.ml clean.ml quotient.ml blind.ml fuzzer_api.ml -g -o fuzzer_api

clean:
	rm -f *.cm* *.o *_api

parserbnf.cmx lexenbnf.cmx: lexerbnf.mll parserbnf.mly
	ocamllex lexerbnf.mll
	ocamlyacc parserbnf.mly
	ocamlc -c parserbnf.mli
	ocamlopt -c parserbnf.ml
	ocamlopt -c lexerbnf.ml
