# Poirot: grammar-based injection fuzzer for black-box systems

_Poirot is still a work in progress !_

## Requirements

To use Poirot, you will need the OCaml package manager, `opam`. You can also follow these steps:

    sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
    eval `opam env`
    opam switch create 4.08.1
    eval `opam env`

It is recommended that you add ``eval `opam env` `` in the configuration file of your shell (most likely `~/.bashrc` or `~/.profile`)

To install Poirot, run the following steps. `opam` will automatically install the dependencies of Poirot.

    git clone https://github.com/PFGimenez/poirot.git
    cd poirot
    opam install .
    eval `opam env`

Optionnaly, if you want to use grammars that are not included in this repository, you can use ANTLR4 to convert grammars from .g4 format. Make sure you have python 3 and Java JRE installed and execute:

    pip3 install --user -r antlr4-utils/requirements.txt

# Use Poirot

## Injection in black-box systems

Run `poirot -help` to get the list of parameters on how to use it. The workflow of injection searching in black-box systems is described in the following figure:

![poirot workflow](https://raw.githubusercontent.com/PFGimenez/poirot/master/resources/poirot_workflow.png)

The following examples use the local oracle described in a later section.

Here is an example that uses the simple grammar `msg_exec`. Run:

    poirot -grammar bnf_grammars/toy/msg_exec.bnf -goal "Exe" -legitimate "'value'" -local_oracle 'msg key = ' ' & key = value'

It will generates the injection `value ; exec cmd ; msg key = value`.

You can experiment with the more complex grammar `parenthesis` as well. Run:

    poirot -grammar bnf_grammars/toy/parenthesis.bnf -goal "'b'" -legitimate "'a'" -local_oracle '([[([' '])]])'

It will generate the injection `a])]])b([[([a`.

Goal and start tokens can be either a terminal or a nonterminal symbol. Terminal symbols should be surrounded by single-quote: `'terminal'`.

### Parameters

Here is the list of the options of `poirot`:

- `-avoid` List of characters to avoid (if for example some characters are filtered).
- `-dict` Filename of the semantics dictionary (more information in the next section).
- `-maxdepth` Set the max depth search (default: 10).
- `-maxsteps` Set the max steps search (default: 1000).
- `-oracle_timeout` Set the timeout to oracle calls (in seconds, -1 for no timeout).
- `-throttle` Set the minimal duration between two oracle calls (in seconds, -1 for no wait).
- `-sgraph` Save the search graph in dot format.
- `-nosave_h` Disable the heuristics save.
- `-nosave_oracle` Disable the oracle calls save.
- `-oneline_comment` The string that starts one-line comment. For example, use `-oneline_comment "--"` for SQL grammars.
- `-injg` Export the injection grammar in ANTLR4 format (you don't need to specify the .g4 extension).
- `-lowercase` Convert all terminals to lowercase.
- `-uppercase` Convert all terminals to uppercase.
- `-verbose_lvl` Choose Poirot verbosity: debug, info, warning or error. "Info" by default.
- `-v` Print the version of Poirot.

### Semantics dictionary

Using a semantics dictionary is a way to add semantics to Poirot. It is a simple text file that associate string to nonterminal symbols so Poirot can use them during fuzzing. An example:

```
<column_name>=login
<database_name>=db
<table_name>=users
```

## Injection grammar of white-box systems

If you know the query (i.e. the prefix and the suffix surrounding the injection point), you can directly use Poirot to get the grammar of the injection with the `quotient` function (and possibly export it to an ANTLR4 grammar) along with an injection. White-box fuzzing is of course greatly faster than black-box fuzzing.

For example, you can run:

    whitebox_poirot -grammar bnf_grammars/toy/msg_exec.bnf -prefix "msg key = " -suffix " & key = value" -goal "Exe"

It will generate the injection `value ; exec cmd ; msg key = value`.

# Use the library in your own project

Using Poirot in your project with `dune` is easy: just add `poirot` in the list of the dependencies. Poirot should be available to `ocamlfind` as well (you can check with `ocamlfind query poirot`).

The documentation is available online at https://pfgimenez.github.io/poirot/Poirot/index.html. To generate the documentation locally, make sure `odoc` is installed (or install it with `opam install odoc`). You can then generate the documentation with `dune build @doc`. It will be stored in `_build/default/_doc/_html/poirot/Poirot/index.html`.

Check `src/poirot.ml` for an executable using the library.

# Use the ANTLR4 ⇌ BNF converter

Poirot uses a simple grammar format inspired from the BNF format.

## ANTLR4 → BNF

You can find many grammars in ANTLR4 format in this repository: https://github.com/antlr/grammars-v4.

Go into the directory `antlr4-utils`. If you have a grammar named `test.g4`, you can simply write `make test.bnf` to generate its BNF version. The Makefile will automatically download the ANTLR4 jar file if necessary.

## BNF → ANTLR4

If you have a grammar `test.bnf`, run

    bnf2antlr4 test.bnf

It will generate the file `test.g4` containing the grammar in ANTLR4 format. Its axiom is named `axiom`.

## Oracles

### What is an oracle ?

An oracle is a script that can send injection to the black-box system and, by examining this system, tell whether this injection is syntactically correct or not. Poirot gets the answer by checking its error code: 0 if the injection is syntactically correct, 180 otherwise. Make sure the executable permission is enabled (added with `chmod +x`).

More precisely, Poirot will use this oracle by calling it with a single parameter, the injection. If you need more parameters for your oracle (e.g. an URL), you should give a partial application of the oracle to Poirot. For example, if your oracle script is `oracle.sh param1 param2 param3 inj` where `inj` is the injection to examine, just tell Poirot your script is `oracle.sh param1 param2 param3`.

The oracles are in the directory `oracles`.

We also provide a local oracle that simulates a black-box, given the prefix and the suffix of a query. This can be used to verify the effectiveness of Poirot without interacting with another system. Remark that if you know the prefix and the suffix of the query, you should you the white-box utility instead of the black-box one.