# v0.5
- Rename `quotient_poirot` into `whitebox_poirot`
- "is_in_language" feature
- Inference uses previous oracle calls
- Oracle based on prefix and suffix
- API rework

# v0.4
- Fuzzer and quotient rewrite
- Oracle timeout
- Time and oracle call statistics
- ANTLR4 export more stable
- Dune installs `quotient_poirot`
- Heuristic save can be disabled
- Can set a minimal interval between two oracle calls
- By default, save the previous oracle calls

# v0.3
- Dune installs `poirot` and `bnf2antlr4`
- Oracle should return error code 180 in case of an syntax error
- Fuzzer update
- Use the Logs library
- Handle ctrl-C
- Various bugfixes

# v0.2
- Heuristic serialization and memoization
- Optimization of the A* openset ordering
- Add the `make_oracle_from_…` functions
- Rewrite the heuristic computation
- Rewrite the fuzzer
- The initial injection can be empty
- Read substitutions from a file
- API has default values
- Some minor changes in the API (notably, add `verbose` parameter in `search`)
- Various bugfixes

# v0.1
- First available version
