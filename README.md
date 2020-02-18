# Poirot: grammar-based injection fuzzer for black box systems

_Poirot is still a work in progress_

## Install the library

First, you need the OCaml package manager, `opam`: https://opam.ocaml.org/doc/Install.html. Beware, Poirot needs at least OCaml 4.08.1. See https://ocaml.org/docs/install.html for more information on how to install OCaml.

Then, clone the Poirot repository: `git clone https://github.com/PFGimenez/poirot.git`

Go into the directory `poirot/poirot`.

Finally, install Poirot: `opam pin poirot .` and then `opam install .`

## Use the library in your own project

Using Poirot in your project with `dune` is easy: just add `poirot` in the list of the dependencies. Poirot should be available to `ocamlfind` as well (you can check with `ocamlfind query poirot`).

The documentation is available online at https://pfgimenez.github.io/poirot/poirot/Poirot/index.html. To generate the documentation locally, make sure `odoc` is installed (or install it with `opam install odoc`). You can then generate the documentation with `dune build @doc`. It will be stored in `_build/default/_doc/_html/poirot/Poirot/index.html`.

Check `poirot/poirot/examples/poirot_example.ml` for an example using the library.

## Use the ANTLR4 ⇌ BNF converter

Poirot uses a simple grammar format inspired from the BNF format.

### Install antlr4

You will certainly need ANTLR4. Make sure you have Python 3 and Java JRE installed. Go into the directory `antlr4-utils` and execute `pip3 install -r requirements.txt`

### ANTLR4 → BNF

You can find many grammars in ANTLR4 format in this repository: https://github.com/antlr/grammars-v4.

Go into the directory `antlr4-utils`. If you have a grammar named `test.g4`, you can simply write `make test.bnf` to generate its BNF version. The Makefile will automatically download the ANTLR4 jar file if necessary.

### BNF → ANTLR4

If you have a grammar `test.bnf`, run `dune exec poirot/examples/bnf2antlr4.exe test.bnf`. It will generate the file `test.g4` containing the grammar in ANTLR4 format. Its axiom is named `axiom`.

## Oracles

### What is an oracle ?

An oracle is a script that can send injection to the black-box system and, by examining this system, tell whether this injection is syntactically correct or not. Poirot gets the answer by checking its error code: 0 if the injection is syntactically correct, 1 otherwise.

More precisely, Poirot will use this oracle by calling it with a single parameter, the injection. If you need more parameters for your oracle (e.g. an URL), you should give a partial application of the oracle to Poirot. For example, if your oracle script is `oracle.sh param1 param2 param3 inj`, just tell Poirot your script is `oracle.sh param1 param2 param3`.

The oracles are in the directory `oracles`.

### Use the prefix/suffix oracle

We provide an oracle that simulates a black-box, given a grammar, a prefix and a suffix. To use it, you must first create the ANTLR4 lexer and parser of its grammar. To do that, put the ANTLR4 grammar (such as `example.g4`) into the `antlr4-utils` directory and execute `make exampleLexer.py` (change accordingly to the name of your grammar).

This oracle needs five parameters: the ANTLR4 grammar name (without the `.g4` extension), the axiom, a prefix, a suffix and an injection.

## Use Poirot directly

The example code `poirot_example` allows you to use Poirot without making your own program. You can run `dune exec -- poirot/examples/poirot_example.exe -help` to get the list of parameters on how to use it.

Here is an example that uses the simple grammar `msg_exec`. This example uses the prefix/suffix oracle, so first create the lexer and parser by executing `make msg_execLexer.py` from the `antlr4-utils` directory. Then, run `dune exec -- poirot/examples/poirot_example.exe -grammar bnf_grammars/msg_exec.bnf -goal "Exe" -start "'value'" -oracle "oracles/prefix-suffix.py msg_exec axiom 'msg key = ' ' & key = value'"`. It should generates the injection `value ; exec cmd ; msg key = value`.
