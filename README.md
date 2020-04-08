# Poirot: grammar-based injection fuzzer for black-box systems

_Poirot is still a work in progress !_

## Requirements

To use Poirot, you will need the OCaml package manager, `opam`. You can also follow these steps:

    sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
    eval `opam env`
    opam switch create 4.08.1
    eval `opam env`

To install Poirot, run the following steps. `opam` will automatically install the dependencies of Poirot.

    git clone https://github.com/PFGimenez/poirot.git
    cd poirot
    opam install fmt
    opam install .

You will certainly need ANTLR4. Make sure you have python 3 and Java JRE installed. Execute:

    pip3 install --user -r antlr4-utils/requirements.txt

# Use Poirot directly

## Injection in black-box systems

The workflow of injection searching in black-box systems is described in the following figure:

![poirot.sh workflow](https://raw.githubusercontent.com/PFGimenez/poirot/master/resources/poirot_workflow.png)

The script `poirot.sh` allows you to use Poirot without making your own program. You can run `./poirot.sh -help` to get the list of parameters on how to use it.

Here is an example that uses the simple grammar `msg_exec`. This example uses the prefix/suffix oracle generator (explained in a later section), so first create the lexer and parser by executing `make msg_execLexer.py` from the `antlr4-utils` directory. Then run:

    ./poirot.sh -grammar bnf_grammars/msg_exec.bnf -goal "Exe" -start "'value'" -oracle "oracles/prefix-suffix.py msg_exec axiom 'msg key = ' ' & key = value'"

It will generates the injection `value ; exec cmd ; msg key = value`.

You can experiment with the more complex grammar `parenthesis` as well. Create its lexer and parser and then run:

    ./poirot.sh -grammar bnf_grammars/parenthesis.bnf -goal "'b'" -start "'a'" -oracle "oracles/prefix-suffix.py parenthesis axiom '([[([' '])]])'"

It will generate the injection `a])]])b([[([a`.

## Injection grammar of white-box systems

If you know the query (i.e. the prefix and the suffix surrounding the injection point), you can directly use Poirot to get the grammar of the injection with the `quotient` function, export it to an ANTLR4 grammar, or ask directly Poirot to generate an injection containing some symbol by using the `fuzzer` function. White-box fuzzing is of course greatly faster than black-box fuzzing.

For example, you can run:

    ./poirot_whitebox.sh -grammar bnf_grammars/msg_exec.bnf -pf "'msg ' 'key' ' = '" -sf "' & ' 'key' ' = ' 'value'" -goal "Exe"

It will generate the injection `value ; exec cmd ; msg key = value`.

# Use the library in your own project

Using Poirot in your project with `dune` is easy: just add `poirot` in the list of the dependencies. Poirot should be available to `ocamlfind` as well (you can check with `ocamlfind query poirot`).

The documentation is available online at https://pfgimenez.github.io/poirot/Poirot/index.html. To generate the documentation locally, make sure `odoc` is installed (or install it with `opam install odoc`). You can then generate the documentation with `dune build @doc`. It will be stored in `_build/default/_doc/_html/poirot/Poirot/index.html`.

Check `examples/poirot_example.ml` for an example using the library.

# Use the ANTLR4 ⇌ BNF converter

Poirot uses a simple grammar format inspired from the BNF format.

## ANTLR4 → BNF

You can find many grammars in ANTLR4 format in this repository: https://github.com/antlr/grammars-v4.

Go into the directory `antlr4-utils`. If you have a grammar named `test.g4`, you can simply write `make test.bnf` to generate its BNF version. The Makefile will automatically download the ANTLR4 jar file if necessary.

## BNF → ANTLR4

If you have a grammar `test.bnf`, run

    dune exec examples/bnf2antlr4.exe test.bnf

It will generate the file `test.g4` containing the grammar in ANTLR4 format. Its axiom is named `axiom`.

## Oracles

### What is an oracle ?

An oracle is a script that can send injection to the black-box system and, by examining this system, tell whether this injection is syntactically correct or not. Poirot gets the answer by checking its error code: 0 if the injection is syntactically correct, 1 otherwise.

More precisely, Poirot will use this oracle by calling it with a single parameter, the injection. If you need more parameters for your oracle (e.g. an URL), you should give a partial application of the oracle to Poirot. For example, if your oracle script is `oracle.sh param1 param2 param3 inj`, just tell Poirot your script is `oracle.sh param1 param2 param3`.

The oracles are in the directory `oracles`.

### Use the prefix/suffix oracle generator

The workflow of creating an oracle with the prefix/suffix oracle generator is described in the following figure:

![Prefix/suffix oracle generator workflow](https://raw.githubusercontent.com/PFGimenez/poirot/master/resources/prefix_suffix_oracle_workflow.png)

We provide an oracle generator that simulates a black-box, given a grammar, a prefix and a suffix. To use it, you must first create the ANTLR4 lexer and parser of its grammar. To do that, put the ANTLR4 grammar (such as `example.g4`) into the `antlr4-utils` directory and execute `make exampleLexer.py` (change accordingly to the name of your grammar).

This oracle generator needs four parameters: the ANTLR4 grammar name (without the `.g4` extension), the axiom, a prefix and a suffix. Beware, the grammar path is relative to the directory `antlr4-utils`.

