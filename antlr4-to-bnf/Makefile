bnf_sql:
	cat SQLite.g4 | python3 Main.py > SQLite.bnf

prepare:
	echo "NOTE: antlr4 -Dlanguage=Python3 ANTLRv4Lexer.g4"
	echo "NOTE: antlr4 -Dlanguage=Python3 ANTLRv4Parser.g4"

info:
	# Simple parser without translation.
	cat SQLite.g4 | python3 pygrun -t ANTLRv4 grammarSpec
