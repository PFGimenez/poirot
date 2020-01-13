bnf_sql: SQLite.g4 ANTLRv4Lexer.py ANTLRv4Parser.py
	cat SQLite.g4 | python3 main.py > SQLite.bnf

ANTLRv4Lexer.py: ANTLRv4LexerPythonTarget.g4 ANTLRv4Parser.g4 LexerAdaptor.py LexBasic.g4
	java org.antlr.v4.Tool -Dlanguage=Python3 ANTLRv4LexerPythonTarget.g4

ANTLRv4Parser.py: ANTLRv4LexerPythonTarget.g4 ANTLRv4Parser.g4 LexerAdaptor.py LexBasic.g4
	java org.antlr.v4.Tool -Dlanguage=Python3 ANTLRv4Parser.g4

ANTLRv4LexerPythonTarget.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/ANTLRv4LexerPythonTarget.g4

ANTLRv4Parser.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/ANTLRv4Parser.g4
	sed -i 's/ANTLRv4Lexer/ANTLRv4LexerPythonTarget/' ANTLRv4Parser.g4

LexerAdaptor.py:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/LexerAdaptor.py

LexBasic.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/LexBasic.g4

SQLite.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/sqlite/SQLite.g4
